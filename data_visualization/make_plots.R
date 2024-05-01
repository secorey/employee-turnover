suppressMessages({
  suppressWarnings({
    # Load packages:
    library(tidyverse)
    library(survival)
    library(survminer)
    library(glmnet)
    library(readxl)
    library(here)
    source(here("utils.R"))
    
    # Load data:
    # - Survival data:
    df <- read_csv(here("data", "agg_survival.csv"))
    surv_df <- prepare_data(df, cox = TRUE, prediction_year = FALSE)
    # - One-year predictions data:
    df <- read_csv(here("data","agg_one_year.csv"))
    oy_df <- prepare_data(df, prediction_year = FALSE)
    # - Predictions:
    predictions_2023 <- read_csv(here("one_year_predictions", "models",
                                      "regressions",
                                      "lasso_v6_predictions.csv"))
  })
})

# Standard colors for plots:
colors <- c("#800000", "#7d7d7d", "#4d4d4d", "#e9aa03", "#b36955", "#01293a")

# ~HELPER FUNCTIONS~
get_retirements <- function() {
  base_path = here("data", "snapshots")
  retirement_df <- data.frame()
  for (year in c(2018, 2019, 2020, 2021, 2022, 2023)) {
    path <- paste0(base_path, "/EE Snapshot ", year, ".12.31.xlsx")
    suppressMessages({
      new_df <- read_excel(path)
    })
    # Remove top rows and change column names:
    colnames(new_df) <- unlist(new_df[4, ])
    new_df <- new_df[-(1:4), ]
    new_df <- filter(new_df, `Termination Reason` == "Voluntary: Retirement")
    retirement_df <- rbind(retirement_df, new_df)
  }
  retirement_df <- mutate(retirement_df,
                          `As of Date` =
                            as.Date(as.numeric(`As of Date`),
                                    origin = "1899-12-30"),
                          year = year(`As of Date`))
  return(retirement_df)
}

# ~PLOTS~

# General headcount trends:
make_headcount_plot <- function() {
  retirements <- get_retirements()
  retirements$outcome <- "Retired"
  retirements <- select(retirements, year, outcome)
  
  plot_df <- mutate(df,
                    fired = attrition - vol_attrition,
                    outcome = case_when(
                      fired == 1 ~ "Fired",
                      vol_attrition == 1 ~ "Quit",
                      TRUE ~ "Stayed"
                    )) %>%
    select(year, outcome) %>%
    rbind(retirements)
  
  plot <- plot_df %>%
    group_by(year, outcome) %>%
    summarize(count = n(), .groups = 'drop') %>%
    group_by(year) %>%
    mutate(total = sum(count),
           percentage = count / total * 100) %>%
    filter(outcome %in% c("Fired", "Quit", "Retired")) %>% 
    ggplot(aes(x = year, y = percentage, color = outcome, group = outcome)) +
    geom_line(linewidth = 2) +
    labs(x = "Year", y = "Percentage of headcount", color = "Outcome") +
    scale_color_manual(values = c("Fired" = colors[2],
                                  "Quit" = colors[1],
                                  "Retired" = colors[6])) +
    scale_y_continuous(limits = c(0, 15)) +
    univ_plot_theme
  ggsave(here("data_visualization",
              "plots",
              "general_headcount_trends_line.pdf"),
         plot,
         width = 12,
         height = 8)
}
make_headcount_plot()

# Lasso coefficients:
make_coefs_plot <- function(coefs, title, file_name) {
  coef_colors <- colors
  coefs$var_name <- rownames(coefs)
  rownames(coefs) <- NULL
  coefs_flipped <- coefs %>%
    filter(!(var_name %in% c("(Intercept)")), s1 != 0) %>%
    mutate(
      neg_estimate = -s1,
      var_name = str_replace_all(var_name, c(
        "_norm" = "",
        "_encoded" = "",
        "location" = "Location = ",
        "country" = "Country = ",
        "grouping" = "Grouping = ",
        "gender" = "Gender = ",
        "_" = " ",
        "over" = "/",
        "binned" = "",
        "rhl" = "Reporting Hierarchy Level",
        "\\(10,100\\]" = "= 11+",
        "\\(50,100\\]" = "= 51+",
        "\\(5,10\\]" = "= 6-10",
        "\\(40,50\\]" = "= 41-50",
        "\\(20,30\\]" = "= 21-30",
        "\\(0,2\\]" = "= 0-2",
        "\\(3,7\\]" = "= 4-7",
        "\\(7,13\\]" = "= 7+",
        "\\(30,40\\]" = "= 31-40",
        "\\(2,5\\]" = "= 3-5"
      )),
      var_name = recode(var_name,
                        "job family groupHuman Resources (HR)" =
                          "Job Family Group = Human Resources",
                        "rhl squared" = "Reporting Hierarchy Level Squared",
                        "job familyOperational Excellence (OE) - Continuous Improvement (CI)" =
                          "Job Family = OE - CI"),
      var_name = str_to_title(var_name)
    )
  
  # Handle case when all coefficients are negative:
  if (sum(coefs_flipped$neg_estimate > 0) == 0) {
    coef_colors[2] = coef_colors[1]
  }
  
  plot <- coefs_flipped %>%
    ggplot(aes(x = neg_estimate,
               y = reorder(var_name, +neg_estimate),
               fill = neg_estimate < 0)) +
    geom_col() +
    scale_fill_discrete(type = c(coef_colors[2], coef_colors[1])) +
    labs(title = title) +
    geom_vline(xintercept = 0) +
    univ_plot_theme +
    theme(legend.position = "none",
          axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.line.x = element_blank(),
          axis.title.y = element_blank())
  ggsave(here("data_visualization", "plots", file_name),
         plot, width = 12, height = 6)
}
make_multiple_coefs_plots <- function(model, lambda = NULL, file_name_base) {
  # Set maximum reasonable lambda value if null:
  if (is.null(lambda)) {lambda <- model$lambda.1se}
  
  coefs <- as.data.frame(as.matrix(coef(cv_fit, s = lambda))) %>%
    arrange(desc(abs(s1)))
  title = "What drives retention?"
  
  # Plot coefficients for all variables combined:
  file_name = paste0(file_name_base, ".pdf")
  make_coefs_plot(coefs, title, file_name)
  
  # Plot coefficients for locations:
  file_name = paste0(file_name_base, "_loc.pdf")
  loc_coefs <- filter(coefs, grepl("location|country", rownames(coefs)))
  make_coefs_plot(loc_coefs, title, file_name)
  
  # Plot coefficients for other variables:
  file_name = paste0(file_name_base, "_other.pdf")
  other_coefs <- filter(coefs, !grepl("location|country", rownames(coefs)))
  make_coefs_plot(other_coefs, title, file_name)
}
# Plot coefficients for lasso v4:
cv_fit <- readRDS(here("one_year_predictions", "models", "regressions",
                       "cv_model_v4.rds"))
lambda = 0.02
make_multiple_coefs_plots(cv_fit, lambda, "lasso_v4_coefs")
# Plot coefficients for lasso v6:
lambda = 0.004
cv_fit <- readRDS(here("one_year_predictions", "models", "regressions",
                       "cv_model_v6.rds"))
make_multiple_coefs_plots(cv_fit, lambda, file_name_base = "lasso_v6_coefs")


# Tenure distribution by voluntary turnover (y/n):
make_tenure_plot <- function() {
  # Determine position of peak of "Yes" curve:
  yes_density <- density(oy_df$tenure[oy_df$vol_attrition_next_year == 1])
  peak_x <- yes_density$x[which.max(yes_density$y)]
  print(paste("X-value at peak:", round(peak_x * 365), "days"))
  
  plot <- oy_df %>% 
    mutate(va = ifelse(vol_attrition_next_year == 1, "Yes", "No")) %>%
    ggplot(aes(x = tenure, color = va)) +
    geom_density(linewidth = 2) +
    labs(x = "Tenure",
         y = "Density",
         color = "Voluntary turnover") +
    scale_color_manual(values = c("No" = colors[2], "Yes" = colors[1])) +
    scale_y_continuous(limits = c(0, 0.25)) +
    univ_plot_theme +
    theme(legend.position = c(0.9, 0.8),
          legend.justification = c(1, 1))
  ggsave(here("data_visualization", "plots", "tenure_dist.pdf"),
         plot,
         width = 10,
         height = 8)
}
make_tenure_plot()

# Unionization turnover rates:
make_union_plot <- function() {
  plot <- oy_df %>%
    mutate(in_manufacturing = ifelse(job_family_group == "Manufacturing (MF)",
                                     "Manufacturing",
                                     "Other")) %>%
    group_by(is_active_union_member, in_manufacturing) %>%
    summarize(va_rate = mean(vol_attrition_next_year),
              count = n()) %>%
    mutate(se = 100 * sqrt(va_rate * (1 - va_rate) / count),
           va_percent = 100 * va_rate,
           lower = va_percent - 1.96 * se,
           upper = va_percent + 1.96 * se) %>%
    ggplot(mapping = aes(x = is_active_union_member,
                         y = va_percent,
                         color = as.factor(in_manufacturing))) +
    geom_line(linewidth = 2) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, color = "black", linewidth = 1) +
    scale_color_manual(values = c("Other" = colors[2], "Manufacturing" = colors[1])) +
    scale_x_continuous(breaks = c(0, 1), labels = c("Not unionized", "Unionized")) +
    scale_y_continuous(limits = c(0, 8)) +
    labs(x = "",
         y = "Voluntary turnover percentage",
         color = "Job family group") +
    univ_plot_theme
  ggsave(here("data_visualization", "plots", "union_turnvoer.pdf"),
         plot,
         width = 10,
         height = 8)
}
make_union_plot()

# Relationship between age and tenure:
make_age_tenure_plot <- function() {
  set.seed(42)
  plot <- oy_df %>%
    mutate(va = ifelse(vol_attrition_next_year == 1, "Yes", "No")) %>%
    group_by(va) %>%
    sample_n(size = 1000) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = tenure, y = age, color = va)) +
    scale_color_manual(values = c("No" = colors[2], "Yes" = colors[1])) +
    geom_point() +
    geom_smooth() +
    labs(x = "Tenure",
         y = "Age",
         color = "Voluntary turnover") +
    univ_plot_theme
  ggsave(here("data_visualization", "plots", "tenure_age.pdf"),
         plot,
         width = 10)
}
make_age_tenure_plot()

# Relationship between PIR and tenure:
make_pir_tenure_plot <- function() {
  set.seed(42)
  plot <- oy_df %>%
    filter(position_in_range < 3, grade_grouping != "Local") %>%
    mutate(va = ifelse(vol_attrition_next_year == 1, "Yes", "No")) %>%
    group_by(va) %>%
    sample_n(size = 500) %>%
    ungroup() %>%
    ggplot(mapping = aes(x = tenure, y = position_in_range, color = va)) +
    scale_color_manual(values = c("No" = colors[2], "Yes" = colors[1])) +
    geom_point() +
    geom_smooth() +
    labs(x = "Tenure",
         y = "Position in Range",
         color = "Voluntary turnover",
         caption = "Non-local grade grouping") +
    univ_plot_theme
  ggsave(here("data_visualization", "plots", "tenure_pir.pdf"),
         plot,
         width = 10,
         height = 8)
}
make_pir_tenure_plot()

# Turnover by country:
make_country_plot <- function() {
  va_rate_all <- 100 * sum(oy_df$vol_attrition_next_year) / nrow(oy_df)
  data_plot <- oy_df %>% 
    group_by(country) %>%
    summarize(va_rate = sum(vol_attrition_next_year) / n(),
              count = n()) %>%
    mutate(percent = va_rate * 100,
           se = 100 * sqrt(va_rate * (1 - va_rate) / count),
           lower = percent - 1.96 * se,
           upper = percent + 1.96 * se,
           country = recode(country, "United States of America" = "USA")) %>%
    arrange(desc(count)) %>%
    head(5)
  
  # Plotting
  plot <- ggplot(data_plot, aes(x = reorder(country, -percent), y = percent)) +
    geom_col(show.legend = FALSE, fill = colors[1]) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, color = "black", linewidth = 1) +
    geom_hline(yintercept = va_rate_all, linewidth = 1, color = colors[2]) +
    geom_text(aes(x = 5, y = va_rate_all, label = "Overall Average",
                  vjust = -0.5),
              color = colors[2]) +
    labs(x = "Country",
         y = "Voluntary turnover percentage",
         title = "Top 5 countries by population") +
    univ_plot_theme
  
  ggsave(here("data_visualization", "plots", "country_rates.pdf"), plot,
         width = 10)
}
make_country_plot()

make_top_or_bottom_country_plot <- function(top) {
  # Set `top = TRUE` to get the top 5 countries by attrition rate, set
  # `top = FALSE` to get the bottom 5
  
  over100_avg_yearly_count <-
    oy_df %>%
    group_by(country, year) %>%
    summarize(count = n()) %>%
    group_by(country) %>%
    summarize(avg_count = mean(count)) %>%
    filter(avg_count > 100)
  
  va_rate_all <- 100 * sum(oy_df$vol_attrition_next_year) / nrow(oy_df)
  data_plot <- oy_df %>% 
    group_by(country) %>%
    summarize(va_rate = sum(vol_attrition_next_year) / n(),
              count = n()) %>%
    filter(country %in% over100_avg_yearly_count$country) %>%
    mutate(percent = va_rate * 100,
           se = 100 * sqrt(va_rate * (1 - va_rate) / count),
           lower = percent - 1.96 * se,
           upper = percent + 1.96 * se,
           country = recode(country, "United States of America" = "USA")) %>%
    arrange(if (top) desc(percent) else percent) %>%
    head(5)
  
  if (top) {
    plot_title <- "Top 5 countries by voluntary turnover rate"
    file_name <- "top_5_countries.pdf"
  } else {
    plot_title <- "Bottom 5 countries by voluntary turnover rate"
    file_name <- "bottom_5_countries.pdf"
  }
  
  # Plotting
  plot <- ggplot(data_plot, aes(x = reorder(country, -percent), y = percent)) +
    geom_col(fill = colors[1]) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  width = 0.2, color = "black", linewidth = 1) +
    geom_hline(yintercept = va_rate_all, linewidth = 1, color = colors[2]) +
    geom_text(aes(x = 5, y = va_rate_all, label = "Overall Average",
                  vjust = -0.5),
              color = colors[2]) +
    scale_y_continuous(limits = c(0, 10)) +
    labs(x = "Country",
         y = "Voluntary turnover percentage",
         title = plot_title,
         caption = "Countries with >100 average employees per year") +
    univ_plot_theme
  
  ggsave(here("data_visualization", "plots", file_name), plot,
         width = 8)
}
make_top_or_bottom_country_plot(top = TRUE)
make_top_or_bottom_country_plot(top = FALSE)

# Reporting hierarchy level:
make_rhl_plot <- function() {
  data_plot <- oy_df %>%
    mutate(rhl_binned = cut(reporting_hierarchy_level, c(-1, 3, 7, 13))) %>%
    group_by(rhl_binned) %>%
    summarize(va_rate = sum(vol_attrition_next_year) / n(),
              count = n()) %>%
    filter(count > 100) %>%
    mutate(percent = va_rate * 100,
           se = 100 * sqrt(va_rate * (1 - va_rate) / count),
           lower = percent - 1.96 * se,
           upper = percent + 1.96 * se)
  
  # Plotting
  plot <- ggplot(data_plot, aes(x = rhl_binned, y = percent)) +
    geom_col(fill = colors[1]) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black",
                  linewidth = 1) +
    scale_x_discrete(labels = c("(-1,3]" = "0-3",
                                "(3,7]" = "4-7",
                                "(7,13]" = "8+")) +
    labs(x = "Reporting hierarchy level",
         y = "Voluntary turnover percentage") +
    univ_plot_theme
  
  ggsave(here("data_visualization", "plots", "rhl_rates.pdf"), plot,
         width = 12, height = 8)
}
make_rhl_plot()

# Grade grouping:
make_gg_plot <- function() {
  data_plot <- oy_df %>%
    group_by(grade_grouping) %>%
    summarize(va_rate = sum(vol_attrition_next_year) / n(),
              count = n()) %>%
    mutate(percent = va_rate * 100,
           se = 100 * sqrt(va_rate * (1 - va_rate) / count),
           lower = percent - 1.96 * se,
           upper = percent + 1.96 * se)
  
  # Plotting
  plot <- ggplot(data_plot, aes(x = factor(grade_grouping,
                                           levels = c("Local", "Grade A-F",
                                                      "Grade G-I", "Grade J+")),
                                y = percent)) +
    geom_col(fill = colors[1]) +
    geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2, color = "black",
                  linewidth = 1) +
    labs(x = "Grade grouping",
         y = "Voluntary turnover percentage") +
    univ_plot_theme

  ggsave(here("data_visualization", "plots", "gg_rates.pdf"), plot,
         width = 10, height = 8)
}
make_gg_plot()

# Gender and tenure:
make_gender_tenure_plot <- function(quit, title, file_name) {
  # `quit` should be 1 or 0 depending on if you want to make the plot for
  # quit employees or non-quit employees
  plot <- oy_df %>%
    filter(vol_attrition_next_year == quit,
           gender != "Not Specified") %>%
    ggplot(aes(x = tenure, color = gender)) +
    geom_density(linewidth = 2) +
    labs(x = "Tenure",
         y = "Density",
         color = "Gender") +
    scale_color_manual(values = c("Female" = colors[2], "Male" = colors[1])) +
    univ_plot_theme +
    labs(title = title) +
    theme(legend.position = c(0.9, 0.8),
          legend.justification = c(1, 1))
  ggsave(here("data_visualization", "plots", file_name),
         plot,
         width = 10,
         height = 8)
}
make_gender_tenure_plot(1, "Quit employees", "gender_tenure_quit.pdf")
make_gender_tenure_plot(0, "Retained employees", "gender_tenure_retained.pdf")

# ~~PREDICTIONS~~

make_jfg_gg_pred_plot <- function() {
  ordered_preds <- predictions_2023 %>%
    group_by(job_family_group, grade_grouping) %>%
    summarize(count = n(),
              avg_prediction = mean(prediction)) %>%
    mutate(jfg_gg = paste(job_family_group, grade_grouping, sep = " - ")) %>%
    filter(count > 20) %>%
    arrange(desc(avg_prediction))
  ordered_preds <- rbind(head(ordered_preds, 10), tail(ordered_preds, 10))
  
  plot <- ggplot(ordered_preds, 
         aes(y = reorder(jfg_gg, avg_prediction),
             x = avg_prediction)) +
    geom_segment(aes(y = jfg_gg,
                     yend = jfg_gg,
                     x = 0,
                     xend = avg_prediction),
                 linetype = "dashed",
                 linewidth = 1,
                 color = colors[3]) +
    geom_point(size = 5, color = colors[1]) +
    geom_hline(yintercept = 10.5, linewidth = 0.75, color = colors[6]) +
    scale_x_continuous(breaks = seq(0, 0.12, length.out = 13)) +
    labs(title = "2024 voluntary turnover risk",
         x = "Risk",
         y = "") +
    univ_plot_theme
  return(plot)
  ggsave(here("data_visualization", "plots", "jfg_gg_preds.pdf"), plot,
         width = 12)
}
make_jfg_gg_pred_plot()

# ~TABLES~

tenure_age_table <- oy_df %>%
  mutate(tenure_over_8 = ifelse(tenure > 8, 1, 0),
         age_over_38 = ifelse(age > 38, 1, 0)) %>%
  group_by(tenure_over_8, age_over_38) %>%
  summarize(prop_attrited = sum(vol_attrition_next_year) / n()) %>%
  pivot_wider(names_from = age_over_38, values_from = prop_attrited)
print(tenure_age_table)

rhl_count_table <- df %>%
  filter(year == 2023) %>%
  mutate(rhl_binned = cut(`Reporting hierarchy level`, c(-1, 3, 7, 13))) %>%
  group_by(rhl_binned) %>%
  summarize(count = n())
print(rhl_count_table)

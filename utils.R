# This script contains utility functions to be used by other files.

library(tidyverse)
library(caret)
library(ROCR)
set.seed(42)

colors <- c("#800000", "#7d7d7d", "#4d4d4d", "#e9aa03", "#b36955", "#01293a")

# Generate universal theme for plots:
univ_plot_theme <- theme(panel.background = element_rect(fill = "white"),
                         title = element_text(size = 22),
                         legend.title = element_text(size = 20),
                         legend.text = element_text(size = 16),
                         axis.title = element_text(size = 20),
                         axis.text = element_text(size = 14),
                         plot.caption = element_text(size = 10),
                         axis.line = element_line(color = "black"))


prepare_data <- function(old_df, cox = FALSE, prediction_year) {
  # - Function for cleaning data
  # - Renames columns, filters out/fills NA values, encodes ordinal features
  # - `prediction_year` is FALSE when you are preparing the data for model
  # training. If making predictions based on the final year of the dataset,
  # this variable should be set to the year (e.g, `prediction_year = 2023`)
  
  df <- rename(old_df,
               age =
                 `INGR - Compensation - STIP - Age as of Report Effective Date`,
               is_active_union_member =
                 `INGR - Compensation - Employee - Is Active Union Member?`)
  colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
  
  # Set countries to create continent variable:
  asian_countries <- c("Thailand", "Malaysia", "Singapore", "China", "Pakistan",
                       "United Arab Emirates", "Vietnam", "Japan", 
                       "Philippines", "Indonesia", "India", "Türkiye")
  oceanian_countries <- c("Australia", "New Zealand")
  north_american_countries <- c("United States of America", "Mexico", "Canada",
                                "Costa Rica")
  south_american_countries <- c("Uruguay", "Brazil", "Argentina", "Colombia",
                                "Peru", "Chile", "Paraguay", "Ecuador",
                                "Guatemala")
  european_countries <- c("Germany", "United Kingdom", "France", "Italy",
                          "Russian Federation", "Netherlands", "Denmark",
                          "Belgium", "Poland", "Ukraine", "Sweden", "Spain")
  african_countries <- c("Kenya", "South Africa", "Algeria")
  
  # Make edits that only apply to one-year predictions:
  if (!cox) {
    df <- filter(df, !is.na(vol_attrition_next_year), attrition == 0) %>%
      # Add feature for supervisor change from the previous year:
      arrange(employee_id, year) %>%
      group_by(employee_id) %>%
      mutate(prev_year_supervisor = lag(reports_to_employee_id),
             supervisor_change =
               ifelse((reports_to_employee_id == prev_year_supervisor) |
                        (is.na(prev_year_supervisor)), 0, 1),
             prev_job_family = lag(job_family),
             job_fam_change = ifelse((job_family == prev_job_family) |
                                       (is.na(prev_job_family)), 0, 1)) %>%
      ungroup()
    # Add supervisor tenure:
    df <- left_join(df,
                    select(df, employee_id, tenure, year),
                    by = c("reports_to_employee_id" = "employee_id",
                           "year" = "year"),
                    suffix = c("", "_supervisor"))
  } else {
    # Add supervisor tenure:
    df <- left_join(df,
                    select(df, employee_id, tenure),
                    by = c("reports_to_employee_id" = "employee_id"),
                    suffix = c("", "_supervisor"))
  }
  
  # Filter out NA values and do feature engineering:
  df <- filter(df,
               !is.na(company), # Only one case
               !is.na(`management_level`)) %>% # Only one case
    mutate(is_active_union_member = # Change NAs to 0
             as.numeric(ifelse(is.na(is_active_union_member), 0, 1)),
           ethnicity = ifelse((ethnicity == "-") |
                                (ethnicity == "INGR - Not Specified (USA)"),
                              "Not Specified", 
                              ethnicity),
           cost_centre = as.factor(cost_centre),
           elt = ifelse(is.na(elt), "Not Specified", elt),
           tenure = ifelse(is.na(tenure),
                           median(df$tenure, na.rm = TRUE), tenure),
           # If tenure is less than time in job profile, change:
           tenure = ifelse(tenure < time_in_job_profile,
                           time_in_job_profile,
                           tenure),
           tenure_supervisor = ifelse(is.na(tenure_supervisor),
                                      median(df$tenure_supervisor, na.rm= TRUE),
                                      tenure_supervisor),
           location = ifelse(grepl("Home Office", location),
                             "Home Office",
                             location),
           management_level_encoded = as.numeric(recode(
             management_level, 
             "Associate" = "1",
             "Professional" = "2",
             "Manager" = "3",
             "Director" = "4",
             "Executive" = "5",
             "Executive Leadership Team" = "6",
             "Chief Executive Officer" = "7"
           )),
           zone_encoded = as.numeric(recode(
             zone,
             "Below Zone 1" = "1",
             "Zone 1" = "2",
             "Zone 2" = "3",
             "Zone 3" = "4",
             "Above Zone 3" = "5"
           )),
           grade_grouping_encoded = as.numeric(recode(
             grade_grouping,
             "Grade J+" = "4",
             "Grade G-I" = "3",
             "Grade A-F" = "2",
             "Local" = "1"
           )),
           annual_review_encoded = as.numeric(recode(
             curr_annual_review,
             "U" = "1",
             "N" = "1",
             "M" = "2",
             "E" = "3",
             "S" = "3"
           )),
           # Add NA values for annual review encoded:
           annual_review_encoded = ifelse(annual_review_encoded == 0,
                                          NA,
                                          annual_review_encoded),
           potential_encoded = recode(
             potential,
             "1 - Develop at Level" = "1",
             "Well Placed" = "1",
             "2 - Promotable one Leader Level" = "2",
             "Moderate" = "2",
             "3 - Promotable more than one Leader Level" = "3",
             "High" = "3"
           ),
           # Add NA values for potential encoded:
           potential_encoded = as.numeric(
             ifelse(potential_encoded == "No Rating", NA, potential_encoded)),
           # Create continent variable:
           continent = as.factor(case_when(
             country %in% asian_countries ~ "Asia",
             country %in% oceanian_countries ~ "Oceania",
             country %in% north_american_countries ~ "North America",
             country %in% south_american_countries ~ "South America",
             country %in% european_countries ~ "Europe",
             country %in% african_countries ~ "Africa"
           ))) %>%
    filter(tenure != 0) # Only two cases
  # Convert categorical features to factors:
  cat_features <- c("job_family", "job_family_group", "country", "location", 
                    "ethnicity", "gender", "cost_centre", "company", 
                    "elt", "flsa_status", "continent", "grade_grouping")
  for (feature in cat_features) {
    df[[feature]] <- as.factor(df[[feature]])
  }
  if (prediction_year) {
    df <- filter(df, year == prediction_year)
  }
  return(df)
}


min_max_scale <- function(x) {
  # Function to perform min-max scaling
  min_x <- min(x, na.rm = TRUE)
  max_x <- max(x, na.rm = TRUE)
  (x - min_x) / (max_x - min_x)
}


prepare_data_for_regression <- function(old_df,
                                        exclude_cols = c(),
                                        cox = FALSE,
                                        prediction_year = FALSE) {
  # Function for preparing data for regression.
  # The `cox` argument determines whether to include two output variables for
  # survival analysis or one binary output variable for classification
  # Inputs:
  #   old_df: Unedited data frame (agg_one_year.csv or agg_survival.csv)
  #   exclude_cols: Columns to exclude
  #   cox: Determines whether to include two output variables for survival
  #        analysis or one binary output variable for classification
  
  df <- prepare_data(old_df, cox, prediction_year)
  
  # Create new columns to handle correlated variables:
  df <- mutate(df,
               tijp_over_tenure = time_in_job_profile / tenure,
               # Take log to handle extreme values:
               age_over_tenure = log(age / tenure),
               rhl_squared = reporting_hierarchy_level ** 2,
               tenure_binned = cut(tenure, c(0, 2, 5, 10, 100)),
               age_binned = cut(age, c(0, 20, 30, 40, 50, 100)),
               rhl_binned = cut(reporting_hierarchy_level, c(-1, 3, 7, 13)))
  
  # Normalize numeric features:
  numeric_features <- c("number_of_direct_reports",
                        "number_of_direct_and_indirect_reports",
                        "reporting_hierarchy_level",
                        "position_in_range",
                        "tenure",
                        "time_in_job_profile",
                        "age",
                        "management_level_encoded",
                        "zone_encoded",
                        "grade_grouping_encoded",
                        "annual_review_encoded",
                        "potential_encoded",
                        "tenure_supervisor",
                        "tijp_over_tenure",
                        "age_over_tenure",
                        "rhl_squared")
  for (feature in numeric_features) {
    new_name <- paste0(tolower(gsub(" ", "_", feature)), "_norm")
    df[new_name] <- min_max_scale(df[feature])
  }
  
  features <- c("job_family", "job_family_group", "country", "location",
                "ethnicity", "gender", "cost_centre", "company", "elt",
                "is_active_union_member", "continent",
                "management_level_encoded_norm", "zone_encoded_norm",
                "annual_review_encoded_norm", "potential_encoded_norm",
                "grade_grouping_encoded_norm", "number_of_direct_reports_norm",
                "number_of_direct_and_indirect_reports_norm",
                "reporting_hierarchy_level_norm", "position_in_range_norm",
                "tenure_norm", "time_in_job_profile_norm", "age_norm",
                "tenure_supervisor_norm",
                "tijp_over_tenure_norm", "age_over_tenure_norm",
                "rhl_squared_norm", "tenure_binned", "age_binned", "rhl_binned",
                "grade_grouping", "supervisor_change", "job_fam_change")
  
  # Add DV columns (depending on if this is for SA or classification):
  if (cox) {
    features <- features[1:(length(features) - 2)]
    df <- df[c(features, "vol_attrition", "num_years")]
  } else {
    df <- df[c(features, "vol_attrition_next_year")]
  }
  all_cols <- colnames(df)
  columns_to_keep <- setdiff(all_cols, exclude_cols)
  return(df[, columns_to_keep, drop = FALSE])
}


get_X_y <- function(df, cat_features, num_features, cox = FALSE) {
  # Encodes categorical features and returns X and y separately to be used for
  # model training or testing (only needed for regression)
  
  # One-hot encode categorical features:
  # Set sparse = TRUE to deal with factors with many levels:
  cat_X <- as.data.frame(
    as.matrix(makeX(df[cat_features], sparse = TRUE))
  )
  cat_X <- rownames_to_column(cat_X, var = "index")
  
  # Extract numerical features and join with encoded categorical features:
  num_X <- df[num_features]
  num_X <- rownames_to_column(num_X, var = "index")
  X <- left_join(cat_X, num_X, by = "index") %>% select(-index)
  
  if (cox) {
    X <- select(X, -vol_attrition)
    y <- data.frame(time = df$num_years + 1,
                              status = as.factor(df$vol_attrition))
  } else {
    y <- df$vol_attrition_next_year
  }
  return(list(X, y))
}


prepare_data_for_DT <- function(old_df, exclude_cols = c(), prediction_year = FALSE) {
  # Prepares data for running a decision tree classifier
  
  df <- prepare_data(old_df, prediction_year = prediction_year)
  df$is_active_union_member <- as.factor(df$is_active_union_member)
  features <- c("job_family", "job_family_group", "country", "location",
                "ethnicity", "gender", "cost_centre", "company", "elt",
                "is_active_union_member", "continent",
                "management_level_encoded", "zone_encoded",
                "grade_grouping_encoded", "number_of_direct_reports",
                "number_of_direct_and_indirect_reports",
                "reporting_hierarchy_level", "position_in_range",
                "tenure", "time_in_job_profile", "age")
  df <- df[c(features, "vol_attrition_next_year")]
  all_cols <- colnames(df)
  columns_to_keep <- setdiff(all_cols, exclude_cols)
  df$vol_attrition_next_year <- as.factor(df$vol_attrition_next_year)
  df$is_active_union_member <- ifelse(df$is_active_union_member == 1,
                                      "Yes",
                                      "No")
  return(df[, columns_to_keep, drop = FALSE])
}


split_data <- function(df) {
  # Splits data into train (60%), validate (20%), and test data (20%)
  test_index <- createDataPartition(seq(nrow(df)),
                                    p = 0.2,
                                    list = FALSE)
  test_data <- df[test_index, ]
  non_test_data <- df[-test_index, ]
  validate_index <- createDataPartition(seq(nrow(non_test_data)),
                                        p = 0.25,
                                        list = FALSE)
  validate_data <- non_test_data[validate_index, ]
  train_data <- non_test_data[-validate_index, ]
  return(list(train_data, validate_data, test_data))
}


evaluate_class_model <- function(y_probs, y_actual, threshold = 0.5, file_path = NULL) {
  # Given predicted probabilities and actual labels, print ROC curve, AUC, and
  # confusion matrix
  
  pred <- prediction(y_probs, y_actual)
  # Calculate performance measures:
  perf <- performance(pred, "tpr", "fpr")
  # Plot ROC curve:
  plot(perf, main = "ROC Curve", col = colors[1], lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = colors[6])
  # Confusion matrix:
  y_pred <- ifelse(y_probs >= threshold, 1, 0)
  conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_actual))
  print(conf_matrix)
  # Calculate AUC:
  auc <- performance(pred, "auc")
  auc_value <- as.numeric(auc@y.values)
  print(cat("AUC:", auc_value, "\n"))
  
  if (!is.null(file_path)) {
    # Save the plot as a PDF
    pdf(file_path)
    plot(perf, main = "ROC Curve", col = colors[1], lwd = 2)
    abline(a = 0, b = 1, lty = 2, col = colors[6])
    dev.off()
  }
}

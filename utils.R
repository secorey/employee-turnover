# This script contains utility functions to be used by other files.

library(tidyverse)
library(caret)
set.seed(42)

# Generate universal theme for plots:
univ_plot_theme <- theme(panel.background = element_rect(fill = "white"),
                         title = element_text(size = 22),
                         legend.title = element_text(size = 20),
                         legend.text = element_text(size = 16),
                         axis.title = element_text(size = 20),
                         axis.text = element_text(size = 14),
                         plot.caption = element_text(size = 10),
                         axis.line = element_line(color = "black"))


prepare_data <- function(old_df, cox = FALSE) {
  # Function for cleaning data
  # Renames columns, filters out/fills NA values, encodes ordinal features
  df <- rename(old_df,
               age =
                 `INGR - Compensation - STIP - Age as of Report Effective Date`,
               is_active_union_member =
                 `INGR - Compensation - Employee - Is Active Union Member?`)
  colnames(df) <- tolower(gsub(" ", "_", colnames(df)))
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
  if (!cox) {
    df <- filter(df, !is.na(vol_attrition_next_year), attrition == 0)
  }
  df <- filter(df,
               !is.na(company), # Only one case
               !is.na(`management_level`)) %>% # Only one case
    mutate(is_active_union_member =
             as.numeric(ifelse(is.na(is_active_union_member), 0, 1)),
           ethnicity = ifelse((ethnicity == "-") |
                                (ethnicity == "INGR - Not Specified (USA)"), 
                              "Not Specified", 
                              ethnicity),
           cost_centre = as.factor(cost_centre),
           elt = ifelse(is.na(elt), "Not Specified", elt),
           tenure = ifelse(is.na(tenure),
                           median(df$tenure, na.rm = TRUE), tenure),
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
           grade_grouping_encoded = as.numeric(recode( #Need to flip the order (would have to re-train the models)
             grade_grouping,
             "Grade A-F" = "1",
             "Grade G-I" = "2",
             "Grade J+" = "3",
             "Local" = "4"
           )),
           continent = as.factor(case_when(
             country %in% asian_countries ~ "Asia",
             country %in% oceanian_countries ~ "Oceania",
             country %in% north_american_countries ~ "North America",
             country %in% south_american_countries ~ "South America",
             country %in% european_countries ~ "Europe",
             country %in% african_countries ~ "Africa"
           )))
  # Convert categorical features to factors:
  cat_features <- c("job_family", "job_family_group", "country", "location", 
                    "ethnicity", "gender", "cost_centre", "company", 
                    "elt", "flsa_status", "continent")
  for (feature in cat_features) {
    df[[feature]] <- as.factor(df[[feature]])
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
                                        cox = FALSE) {
  # Function for preparing data for regression
  # The `cox` argument determines whether to include two output variables for
  # survival analysis or one binary output variable for classification
  
  df <- prepare_data(old_df, cox)
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
                        "grade_grouping_encoded")
  for (feature in numeric_features) {
    new_name <- paste0(tolower(gsub(" ", "_", feature)), "_norm")
    df[new_name] <- min_max_scale(df[feature])
  }
  features <- c("job_family", "job_family_group", "country", "location",
                "ethnicity", "gender", "cost_centre", "company", "elt",
                "is_active_union_member", "continent",
                "management_level_encoded_norm", "zone_encoded_norm",
                "grade_grouping_encoded_norm", "number_of_direct_reports_norm",
                "number_of_direct_and_indirect_reports_norm",
                "reporting_hierarchy_level_norm", "position_in_range_norm",
                "tenure_norm", "time_in_job_profile_norm", "age_norm")
  # Add DV columns (depending on if this is for SA or classification):
  if (cox) {
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
    y <- data.frame(time = df$num_years + 1,
                              status = as.factor(df$vol_attrition))
  } else {
    y <- df$vol_attrition_next_year
  }
  return(list(X, y))
}


prepare_data_for_DT <- function(old_df, exclude_cols = c()) {
  # Prepares data for running a decision tree classifier
  df <- prepare_data(old_df)
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


evaluate_class_model <- function(y_probs, y_actual) {
  # Given predicted probabilities and actual labels, print ROC curve, AUC, and
  # confusion matrix
  
  pred <- prediction(y_probs, y_actual)
  # Calculate performance measures:
  perf <- performance(pred, "tpr", "fpr")
  # Plot ROC curve:
  plot(perf, main = "ROC Curve", col = "blue", lwd = 2)
  abline(a = 0, b = 1, lty = 2, col = "red")
  # Confusion matrix:
  y_pred <- ifelse(y_probs >= .5, 1, 0)
  conf_matrix <- confusionMatrix(as.factor(y_pred), as.factor(y_actual))
  print(conf_matrix)
  # Calculate AUC:
  auc <- performance(pred, "auc")
  auc_value <- as.numeric(auc@y.values)
  print(cat("AUC:", auc_value, "\n"))
}

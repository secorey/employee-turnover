# This script takes in the original raw data files and generates two files.
# The two files are as follows:
# 1) Combined .csv file of all the original raw files across all years.
# 2) .csv file for survival analysis. This will be the earliest file (2018)
#    with new hires from following years added. Two more columns will be added:
#    attrition y/n and number of years (either until attrition or until end of
#    analysis).

library(tidyverse)

df_2021 <- read_csv("data/sample/sample_data_2021.csv")
df_2022 <- read_csv("data/sample/sample_data_2022.csv")
df_2023 <- read_csv("data/sample/sample_data_2023.csv")

# Combine data frames into one:
df <- rbind(df_2021, df_2022, df_2023)

# Category name for voluntary turnover:
quit_cat_name <- "Terminate Employee > Involuntary" # Change this #######

# Create columns:
# Year of data collection:
df$year <- substr(df$`As of Date`,
                  nchar(df$`As of Date`) - 3,
                  nchar(df$`As of Date`))
# Whether the employee turned over in that year:
df$attrition <- ifelse(df$`Termination Category` == quit_cat_name, 1, 0)
df$attrition[is.na(df$attrition)] <- 0

# Filter out irrelevant units (retirements):
# df <- df %>%
#   filter((`Termination Reason` !=
#            "Involuntary: Retirement/Resignation with Package") | 
#            is.na(`Termination Reason`))

# Export first file to CSV:
write_csv(df, "data/sample/sample_agg.csv")

# Create second file:
# For each iteration, in the old file, add 1 year to everyone who has not 
# attrited. Then, in the new file, check for new employee IDs and add those rows 
# with 0 for `num_years`. Then, check for all quits in the new file and change 
# those employees to 1 for `attrition`.

df_for_survival <- df_2021

df_for_survival <- mutate(df_for_survival, 
                          attrition = ifelse(
                            (df_for_survival$`Termination Category` != 
                               quit_cat_name) | 
                              is.na(`Termination Category`), 
                            0, 1),
                          num_years = 0)
for (df_year in list(df_2022, df_2023)) {
  # Add one year to non-attrited employees:
  df_for_survival$num_years[df_for_survival$attrition == 0] <- 
    df_for_survival$num_years[df_for_survival$attrition == 0] + 1
  # Add new employees from df_year to df_for_survival:
  new_employees <- 
    filter(df_year, 
           !(`Employee ID` %in% df_for_survival$`Employee ID`)) %>%
    mutate(attrition = 0, num_years = 0)
  df_for_survival <- rbind(df_for_survival, new_employees)
  # Find employees who quit in df_year and 
  # change attrition status in df_for_survival:
  quit_employees <- df_year %>%
    filter(`Termination Category` == quit_cat_name) %>%
    select(`Employee ID`, 
           `Termination Date`, 
           `Termination Category`, 
           `Termination Reason`)
  df_for_survival$attrition[
    df_for_surival$`Employee ID` %in% quit_employees$`Employee ID`] <- 1
  df_for_survival$`Active Status`[
    df_for_surival$`Employee ID` %in% quit_employees$`Employee ID`] <- NA
  # Change term date, term category, and term reason for quit employees:
  df_for_survival$`Termination Date`[
    df_for_surival$`Employee ID` %in% quit_employees$`Employee ID`] <- 
    quit_employees$`Termination Date`
  df_for_survival$`Termination Category`[
    df_for_surival$`Employee ID` %in% quit_employees$`Employee ID`] <- 
    quit_employees$`Termination Category`
  df_for_survival$`Termination Reason`[
    df_for_surival$`Employee ID` %in% quit_employees$`Employee ID`] <- 
    quit_employees$`Termination Reason`
  # `As of Date` now becomes the first date of observation of this employee.
  # This is either the year they are hired or the first year of observation.
  # All other variables are also from this first date.
}

write_csv(df_for_survival, "data/sample/sample_survival_agg.csv")



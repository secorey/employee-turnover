# This script takes in the original raw data files and generates two files.
# The two files are as follows:
# 1) Combined .csv file of all the original raw files across all years. Column
#    added:
#    - vol_attrition_next_year: whether the employee quit in the following year
# 2) .csv file for survival analysis. This will be the earliest file (2018)
#    with new hires from following years added. Four more columns will be added:
#    - year: year of the earliest file where the employee shows up. This is
#      either the year they were hired or 2018
#    - vol_attrition (1/0): whether the employee quit or retired
#    - attrition (1/0): whether the employee turned over (either voluntarily or
#      involuntarily)
#    - num_years: number of years until attrition or until the end of analysis

suppressMessages({
  library(tidyverse)
  library(readxl)
  library(here)
})

read_file <- function(file_path) {
  # This function reads in the raw .xlsx file and applies filters that will
  # be necessary for both the one-year predictions and survival analysis
  suppressMessages({
    df <- read_excel(file_path)
  })
  # Remove top rows and change column names:
  colnames(df) <- unlist(df[4, ])
  df <- df[-(1:4), ]
  
  df <- df %>%
    filter(is.na(`Next day rehire`),
           `Employment Status` == "Regular",
           Country != "Korea, Republic of",
           (`Termination Reason` != "Voluntary: Retirement") |
             (is.na(`Termination Reason`))) %>%
    mutate(`As of Date` =
             as.Date(as.numeric(`As of Date`), origin = "1899-12-30"),
           `Termination Date` =
             as.Date(as.numeric(`Termination Date`), origin = "1899-12-30"),
           year = year(`As of Date`),
           vol_attrition = 
             ifelse((`Termination Category` == "Terminate Employee > Voluntary")
                    & is.na(`Active Status`), 1, 0),
           attrition = ifelse(is.na(`Termination Category`), 0, 1),
           Tenure = as.numeric(ifelse(Tenure < 0, NA, Tenure)))
  # Add column for current year annual review rating:
  year <- df$year[1]
  if (year <= 2022 & year >= 2019) {
    df$curr_annual_review <- df[[paste(year, "Annual Review Rating")]]
  } else {df$curr_annual_review <- NA}

  return(df)
}

base_path = here("data", "snapshots")
df_2018 <- read_file(paste0(base_path, "/EE Snapshot 2018.12.31.xlsx"))
df_2019 <- read_file(paste0(base_path, "/EE Snapshot 2019.12.31.xlsx"))
df_2020 <- read_file(paste0(base_path, "/EE Snapshot 2020.12.31.xlsx"))
df_2021 <- read_file(paste0(base_path, "/EE Snapshot 2021.12.31.xlsx"))
df_2022 <- read_file(paste0(base_path, "/EE Snapshot 2022.12.31.xlsx"))
df_2023 <- read_file(paste0(base_path, "/EE Snapshot 2023.12.31.xlsx"))

# Combine data frames into one:
df <- rbind(df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)

# Add a variable that indicates if an employee quit in the following year:
df <- df %>% 
  arrange(`Employee ID`, year) %>%
  group_by(`Employee ID`) %>%
  mutate(vol_attrition_next_year = lead(vol_attrition)) %>%
  ungroup()

# Export first file to CSV:
write_csv(df, here("data", "agg_one_year.csv"))


# Create survival analysis file:
# For each iteration, in the old file, add 1 year to everyone who has not 
# attrited. Then, in the new file, check for new employee IDs and add those rows 
# with 0 for `num_years`. Then, check for all quits in the new file and change 
# those employees to 1 for `attrition` and for `vol_attrition`. Involuntary
# turnovers receive a 1 for `attrition` and a 0 for `vol_attrition`.

df_for_survival <- mutate(df_2018, num_years = 0)
for (df_year in list(df_2019, df_2020, df_2021, df_2022, df_2023)) {
  # Add one year to non-attrited employees:
  df_for_survival$num_years[df_for_survival$attrition == 0] <-
    df_for_survival$num_years[df_for_survival$attrition == 0] + 1
  # Add new employees from df_year to df_for_survival:
  new_employees <-
    filter(df_year,
           !(`Employee ID` %in% df_for_survival$`Employee ID`)) %>%
    mutate(num_years = 0)
  df_for_survival <- rbind(df_for_survival, new_employees)
  # Find employees who attrited/quit in df_year:
  attrited_employees <- df_year %>%
    filter(attrition == 1) %>%
    select(`Employee ID`,
           `Termination Date`,
           `Termination Category`,
           `Termination Reason`,
           attrition,
           vol_attrition)
  # Overwrite termination information in df_for_survival:
  for (feature in c("Termination Date", "Termination Category", 
                    "Termination Reason", "attrition", "vol_attrition")) {
    df_for_survival[[feature]][
      df_for_survival$`Employee ID` %in% attrited_employees$`Employee ID`] <-
      attrited_employees[[feature]]
  }
  # `As of Date` now becomes the first date of observation of this employee.
  # This is either the year they are hired or the first year of observation.
  # All other variables are also from this first date.
}

# Change active status for attrited employees:
df_for_survival$`Active Status` <- ifelse(df_for_survival$attrition == 0, 
                                          "Yes", 
                                          NA)

write_csv(df_for_survival, "data/agg_survival.csv")

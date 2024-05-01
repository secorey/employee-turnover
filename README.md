# MA Thesis: Employee Turnover
#### Sam Corey
#### MA in Computational Social Science
#### The University of Chicago
#### Last updated: May 1, 2024

## Necessary R packages

- tidyverse ~= 2.0.0
- readxl ~= 1.4.3
- here ~= 1.0.1
- survival ~= 3.5.5
- survminer ~= 0.4.9
- glmnet ~= 4.1.8
- caret ~= 6.0.94
- mlbench ~= 2.1.3.1
- reshape2 ~= 1.4.4
- ROCR ~= 1.0.11

## Utilities file
[utils.R](https://github.com/secorey/employee-turnover/blob/main/utils.R)

Contains functions that are accessed by various parts of the program

## Pipeline

### Generate aggregated files

Run the script: [data_cleaning.R](https://github.com/secorey/employee-turnover/blob/main/data_cleaning.R)

Must be run before subsequent files

### Run lasso regression

Run the notebook: [one_year_predictions/logistic_regression.Rmd](https://github.com/secorey/employee-turnover/blob/main/one_year_predictions/logistic_regression.Rmd)

### Run decision tree classifier

Run the notebook: [one_year_predictions/decision_tree.Rmd](https://github.com/secorey/employee-turnover/blob/main/one_year_predictions/decision_tree.Rmd)

### Run survival anlaysis

Run the notebook: [survival_analysis/survival_analysis.Rmd](https://github.com/secorey/employee-turnover/blob/main/survival_analysis/survival_analysis.Rmd)

### Generate data visualizations

Run the script: [data_visualization/make_plots.R](https://github.com/secorey/employee-turnover/blob/main/data_visualization/make_plots.R)

Must be run after the above files

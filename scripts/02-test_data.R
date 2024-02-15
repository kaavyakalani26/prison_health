#### Preamble ####
# Purpose: Tests for the cleaned datasets to be used for analysis
# Author: Kaavya Kalani, Monica Sainani, Aamishi Avarsekar
# Date: 15 February 2024
# Contact: kaavya.kalani@mail.utoronto.ca, monica.sainani@mail.utoronto.ca, aamishi.avarsekar@mail.utoronto.ca
# License: MIT
# Pre-requisites: run scripts/01-data_cleaning.R

#### Workspace setup ####
# install.packages("readxl")
# install.packages("tidyverse")

library(tidyverse)
library(readxl)

#### Test for cleaned_covid19_data
# Read the Excel file
file_path <- "data/analysis_data/cleaned_covid19_data.csv"
data <- read_csv(file_path)

# Test 1: Check for missing data
if (any(is.na(data))) {
  print("There are missing data.")
} else {
  print("There are no missing data.")
}

# Test 2: Check the number of columns
if (ncol(data) == 3) {
  print("Number of columns is 3.")
} else {
  print("Number of columns is not 3.")
}

# Test 3: Check the values in the "group" column
valid_groups <- c("Incarcerated", "General Population", "Prison Staff")
if (all(data$group %in% valid_groups)) {
  print("Values in the 'group' column are valid.")
} else {
  print("There are invalid values in the 'group' column.")
}

#### Test for cleaned_health_data_by_year
# Read the CSV file
file_path <- "data/analysis_data/cleaned_health_data_by_year.csv"
data <- read.csv(file_path)

# Test 1: Check values in the "Year" column
valid_years <- 2006:2016
if (all(data$Year %in% valid_years)) {
  print("Values in the 'Year' column are between 2006 to 2016 inclusive.")
} else {
  print("There are invalid values in the 'Year' column.")
}

# Test 2: Check for missing data
if (any(is.na(data))) {
  print("There is missing data.")
} else {
  print("There is no missing data.")
}

# Test 3: Check the number of columns
if (ncol(data) == 5) {
  print("Number of columns is 5.")
} else {
  print("Number of columns is not 5.")
}

# Test 4: Check values in the "Cause" column
valid_causes <- c("Cancer", "Heart Disease", "Liver Disease",
                  "Respiratory Disease", "Aid related", "All other illnesses",
                  "Suicide", "Drug/Alcohol Intoxication", "Accident", "Homicide")
if (all(data$Cause %in% valid_causes)) {
  print("Values in the 'Cause' column are valid.")
} else {
  print("There are invalid values in the 'Cause' column.")
}


#### Test for cleaned_health_data_gender.csv

# Read the CSV file
file_path <- "data/analysis_data/cleaned_health_data_gender.csv"
data <- read.csv(file_path)

# Test 1: Check values in the "x1" column
valid_genders <- c("Male", "Female")
if (all(data$x1 %in% valid_genders)) {
  print("Values in the 'x1' column are either 'Male' or 'Female'.")
} else {
  print("There are invalid values in the 'x1' column.")
}

# Test 2: Check for missing data
if (any(is.na(data))) {
  print("There is missing data.")
} else {
  print("There is no missing data.")
}

# Test 3: Check the number of columns
if (ncol(data) == 4) {
  print("Number of columns is 4.")
} else {
  print("Number of columns is not 4.")
}

# Test 4: Check values in the "Cause" column
valid_causes <- c("Cancer", "Heart Disease", "Liver Disease",
                  "Respiratory Disease", "Aid related", "All other illnesses",
                  "Suicide", "Drug/Alcohol Intoxication", "Accident", "Homicide")
if (all(data$Cause %in% valid_causes)) {
  print("Values in the 'Cause' column are valid.")
} else {
  print("There are invalid values in the 'Cause' column.")
}

#### Test for cleaned_health_data_race.csv

# Read the CSV file
file_path <- "data/analysis_data/cleaned_health_data_race.csv"
data <- read.csv(file_path)

# Test 1: Check values in the "x1" column
valid_genders <- c("Hispanic", "White", "Black", "Other")
if (all(data$x1 %in% valid_genders)) {
  print("Values in the 'x1' column are valid.")
} else {
  print("There are invalid values in the 'x1' column.")
}

# Test 2: Check for missing data
if (any(is.na(data))) {
  print("There is missing data.")
} else {
  print("There is no missing data.")
}

# Test 3: Check the number of columns
if (ncol(data) == 4) {
  print("Number of columns is 4.")
} else {
  print("Number of columns is not 4.")
}

# Test 4: Check values in the "Cause" column
valid_causes <- c("Cancer", "Heart Disease", "Liver Disease",
                  "Respiratory Disease", "Aid related", "All other illnesses",
                  "Suicide", "Drug/Alcohol Intoxication", "Accident", "Homicide")
if (all(data$Cause %in% valid_causes)) {
  print("Values in the 'Cause' column are valid.")
} else {
  print("There are invalid values in the 'Cause' column.")
}



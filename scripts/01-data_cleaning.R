#### Preamble ####
# Purpose: Data cleaning the raw files
# Author: Kaavya Kalani, Monica Sainani, Aamishi Avarsekar
# Date: 15 February 2024
# Contact: kaavya.kalani@mail.utoronto.ca, monica.sainani@mail.utoronto.ca, aamishi.avarsekar@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

#### Workspace setup ####
# install.packages("tidyverse")
# install.packages("dplyr")
# install.packages("tidyr")
# install.packages("zoo")
# install.packages("janitor")"
# install.packages("readxl")
# install.packages("writexl")

library(zoo)
library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(readxl)
library(writexl)
library(tidyverse)

# Mapping to be used for Figure 1, 3, 4 of our paper
cause_mapping <- c(
  "cancer" = "Cancer",
  "heart_disease" = "Heart Disease",
  "liver_disease" = "Liver Disease",
  "respiratory_disease" = "Respiratory Disease",
  "aids_related" = "Aid related",
  "all_other_illnesses" = "All other illnesses",
  "suicide" = "Suicide",
  "drug_alcohol_intoxication" = "Drug/Alcohol Intoxication",
  "accident" = "Accident",
  "homicide_d" = "Homicide"
)

#### Cleaning for Figure 1
# Read prisoner stats data
prisoner_stats_data <- read_excel('data/raw_data/mass incarceration.xlsx')
prisoner_necessary_year_data <-  prisoner_stats_data %>% filter(Year %in% c("2006", "2007", "2008","2009", "2010",
                                                                            "2011", "2012", "2013", "2014", "2015", "2016"))
prisoner_stats_data_cleaned <-
  prisoner_necessary_year_data |>
    select("Year", "Prison") |>
    set_names("Year", "Prisoner_Count")

# General health and conditions in prisons
health_data_count_by_year <- read_csv('data/raw_data/msfp0116stt02data.csv')
health_data_by_year_clean <-
  clean_names(health_data_count_by_year) |>
    select("x1", "x2006", "x2007", "x2008","x2009", "x2010",
           "x2011", "x2012", "x2013", "x2014", "x2015", "x2016") |>
    set_names("Disease", "2006", "2007", "2008","2009", "2010",
              "2011", "2012", "2013", "2014", "2015", "2016")

# Filter and transform health data
health_data_by_year <- health_data_by_year_clean %>% filter(Disease %in% c("Cancer", "Heart disease", "Liver disease",
                                                                           "Respiratory disease", "AIDS-related", "All other illnessess", "Suicide",
                                                                           "Drug/alcohol intoxication", "Accident", "Homicide", "Other causes"))
# Transform health data to long format
health_data_by_year_long <- pivot_longer(health_data_by_year, cols = -Disease, names_to = "Year", values_to = "Count")

# Merge health data with prisoner stats data
merged_data <- merge(health_data_by_year_long, prisoner_stats_data_cleaned, by = "Year")

# Calculate proportions
health_data_by_year_long_prop <- merged_data %>%
  mutate(Proportion = Count / Prisoner_Count)

# Save cleaned data to analysis_data folder as CSV
write.csv(health_data_by_year_long_prop, file = "data/analysis_data/cleaned_health_data_by_year.csv", row.names = FALSE)


#### For Figure 2 - replicated from the original paper
pop <- c(1217352, 412783, 328239523)  # population denominators
# Read CSV file with the correct date format
x <- read.csv("data/raw_data/covidts01.csv", header = TRUE, stringsAsFactors = FALSE)
x$date <- as.Date(x$date, format = "%m/%d/%Y")  # Specify the correct date format

# Calculate rolling averages
pp <- 1e5 * zoo::rollapply((x$pp_cases[-1] - x$pp_cases[-132]), width = 7, FUN = mean) / pop[1]
sp <- 1e5 * zoo::rollapply((x$sp_cases[-1] - x$sp_cases[-132]), width = 7, FUN = mean) / pop[2]
gp <- 1e5 * zoo::rollapply((x$gp_cases[-1] - x$gp_cases[-132]), width = 7, FUN = mean) / pop[3]

# Combine data into a data frame
ts_data <- data.frame(
  date = rep(x$date[-(1:7)], 3),
  new_cases_per_100k = c(pp, sp, gp),
  group = rep(c("Incarcerated", "Prison Staff", "General Population"), each = length(pp))
)

# Save cleaned data to analysis_data folder
write.csv(ts_data, file = "data/analysis_data/cleaned_covid19_data.csv", row.names = FALSE)

#### For Figure 3
# Read the raw data
health_data_count_by_demographics <- read_csv("data/raw_data/msfp0116stt09data.csv")

# Clean the data
health_demographic_data_clean <-
  clean_names(health_data_count_by_demographics) |>
    select("x1", "cancer", "heart_disease", "liver_disease",
           "respiratory_disease", "aids_related", "all_other_illnesses", "suicide",
           "drug_alcohol_intoxication", "accident", "homicide_d")

# Rename the causes in the dataset
health_demographic_data_clean <- health_demographic_data_clean %>%
  rename_with(~ cause_mapping[.], -x1)

# Select only Male and Female
gender_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Male", "Female"))

# Reshape the data for plotting
gender_health_data_long <- pivot_longer(gender_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

# Calculate proportions within each category
gender_health_data_long <- gender_health_data_long %>%
  group_by(x1) %>%
  mutate(Proportion = Count / sum(Count))

# Save cleaned data as CSV to analysis_data folder
write_csv(gender_health_data_long, file = "data/analysis_data/cleaned_health_data_gender.csv")

#### Figure 4
# Read data
health_data_count_by_demographics <- read_csv("data/raw_data/msfp0116stt09data.csv")

# Clean and organize data
health_demographic_data_clean <-
  clean_names(health_data_count_by_demographics) |>
    select("x1", "cancer", "heart_disease", "liver_disease",
           "respiratory_disease", "aids_related", "all_other_illnesses", "suicide",
           "drug_alcohol_intoxication", "accident", "homicide_d")

# Pivot data to long format
health_demographic_data_long <- pivot_longer(health_demographic_data_clean, cols = -x1, names_to = "Cause", values_to = "Count")

# Filter data for specific demographics (e.g., "Male" and "Female")
filtered_df <- health_demographic_data_clean[health_demographic_data_clean$x1 %in% c("Male", "Female"),]

# Filter data for specific races (e.g., "White", "Black", "Hispanic", "Other")
race_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("White", "Black", "Hispanic", "Other"))

# Pivot data to long format for race
race_health_data_long <- pivot_longer(race_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

# Calculate proportions instead of counts
race_health_data_long <- race_health_data_long %>%
  group_by(x1) %>%
  mutate(Proportion = Count / sum(Count))

# Rename the causes in the dataset
race_health_data_long <- race_health_data_long %>%
  mutate(Cause = cause_mapping[as.character(Cause)])

# Save cleaned data to analysis_data folder as CSV
write.csv(race_health_data_long, file = "data/analysis_data/cleaned_health_data_race.csv", row.names = FALSE)


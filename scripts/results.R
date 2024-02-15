install.packages("opendatatoronto")
install.packages("knitr")
install.packages("lubridate")
install.packages("tidyverse")
install.packages("janitor")
install.packages("arrow")
install.packages("gridExtra")
install.packages("readxl")

library(arrow)
library(knitr)
library(janitor)
library(lubridate)
library(opendatatoronto)
library(tidyverse)
library(gridExtra)
library(readxl)

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

# prisoner's proportion:
prisoner_stats_data <- read_excel('data/mass_incarceration.xlsx')
prisoner_necessary_year_data <-  prisoner_stats_data %>% filter(Year %in% c("2006", "2007", "2008","2009", "2010", 
                                                                            "2011", "2012", "2013", "2014", "2015", "2016"))
prisoner_stats_data_cleaned <- 
  prisoner_necessary_year_data |>
  select("Year", "Prison") |>
  set_names("Year", "Prisoner_Count")

tibble(prisoner_stats_data_cleaned)

# general health and conditions in prisons
health_data_count_by_year <- read_csv('data/msfp0116stt02data.csv')
health_data_by_year_clean <-
  clean_names(health_data_count_by_year) |>
  select("x1", "x2006", "x2007", "x2008","x2009", "x2010", 
         "x2011", "x2012", "x2013", "x2014", "x2015", "x2016") |>
  set_names("Disease", "2006", "2007", "2008","2009", "2010", 
         "2011", "2012", "2013", "2014", "2015", "2016")

health_data_by_year <- health_data_by_year_clean %>% filter(Disease %in% c("Cancer", "Heart disease", "Liver disease",
                                                                     "Respiratory disease", "AIDS-related", "All other illnessess", "Suicide",
                                                                     "Drug/alcohol intoxication", "Accident", "Homicide", "Other causes"))
# transform
health_data_by_year_long <- pivot_longer(health_data_by_year, cols = -Disease, names_to = "Year", values_to = "Count")

# this includes the prisoner count for each year
merged_data <- merge(health_data_by_year_long, prisoner_stats_data_cleaned, by = "Year")

# generating proportions: health_data_by_year_long by proportion
health_data_by_year_long_prop <- merged_data %>%
  mutate(Proportion = Count / Prisoner_Count)

# can remove if we don't need this graph
ggplot(health_data_by_year_long_prop, aes(x = Disease, y = Proportion, fill = Disease)) +
  geom_bar(stat = "identity", position = "stack", size = 0.8) +  
  labs(title = "Proportions of Causes of Death by Category",
       x = "Disease",
       y = "Proportion") +
  scale_fill_discrete(labels = cause_mapping) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(health_data_by_year_long_prop, aes(x = as.numeric(Year), y = Proportion, color = Disease)) +
  geom_line(size = 1.0) +  
  labs(title = "Trends of Diseases by Year",
       x = "Year",
       y = "Proportion of cases",
       color = "Disease") +
  theme_minimal()



















health_data_count_by_demographics <- read_csv('data/msfp0116stt09data.csv')

health_demographic_data_clean <-
  clean_names(health_data_count_by_demographics) |>
  select("x1", "cancer", "heart_disease", "liver_disease", 
         "respiratory_disease", "aids_related", "all_other_illnesses", "suicide",
         "drug_alcohol_intoxication", "accident", "homicide_d") 

head(health_demographic_data_clean)

health_demographic_data_long <- pivot_longer(health_demographic_data_clean, cols = -x1, names_to = "Cause", values_to = "Count")

filtered_df <- health_demographic_data_clean[health_demographic_data_clean$x1 %in% c("Male", "Female"),]
# all variables
ggplot(health_demographic_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# genders: male and female
gender_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Male", "Female"))
# transform
gender_health_data_long <- pivot_longer(gender_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

ggplot(gender_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(gender_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_col(position = "dodge") +
  facet_wrap(~x1, scales = "free_x") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # Position the legend at the bottom
    legend.box = "horizontal",   # Display the legend items horizontally
    legend.key.height = unit(0.2, "cm"),  # Adjust the height of the legend key
    strip.text = element_text(size = 12),  # Adjust the size of facet labels
    strip.background = element_blank(),   # Remove background from facet labels
    plot.margin = margin(1, 1, 1, 1, "cm"))  # Adjust plot margins

# race demographics:
race_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("White", "Black", "Hispanic", "Other"))

# transform to long for race
race_health_data_long <- pivot_longer(race_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

ggplot(race_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(race_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_col(position = "dodge") +
  facet_wrap(~x1, scales = "free_x") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(
        legend.position = "bottom",  # Position the legend at the bottom
        legend.box = "horizontal",   # Display the legend items horizontally
        legend.key.height = unit(0.2, "cm"),  # Adjust the height of the legend key
        strip.text = element_text(size = 12),  # Adjust the size of facet labels
        strip.background = element_blank(),   # Remove background from facet labels
        plot.margin = margin(1, 1, 1, 1, "cm"))  # Adjust plot margins
# each race
# 1. black
black_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Black"))
black_health_data_long <- pivot_longer(black_health_data, cols = -x1, names_to = "Cause", values_to = "Count")
black_health_data_chart <- ggplot(black_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 2. hispanic
hispanic_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Hispanic"))
hispanic_health_data_long <- pivot_longer(hispanic_health_data, cols = -x1, names_to = "Cause", values_to = "Count")
hispanic_health_data_chart <- ggplot(hispanic_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 3. White 
white_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("White"))
white_health_data_long <- pivot_longer(white_health_data, cols = -x1, names_to = "Cause", values_to = "Count")
white_health_data_chart <- ggplot(white_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 4. Other
other_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Other"))
other_health_data_long <- pivot_longer(other_health_data, cols = -x1, names_to = "Cause", values_to = "Count")
other_health_data_chart <- ggplot(other_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# age demographics:
age_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("17 or younger", "18-24", "25-34", 
                                                                      "35-44", "45-54", "55 or older"))

# transform to long for race
age_health_data_long <- pivot_longer(age_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

ggplot(age_health_data_long, aes(x = x1, y = Count, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Causes of Death by Category",
       x = "Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# I will also be making a graph for proportions



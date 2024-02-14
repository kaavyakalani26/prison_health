# install.packages("tidyverse")
# install.packages("janitor")

library(janitor)
library(tidyverse)

health_data_count_by_demographics <- read_csv('data/msfp0116stt09data.csv')

health_demographic_data_clean <-
  clean_names(health_data_count_by_demographics) |>
    select("x1", "cancer", "heart_disease", "liver_disease",
           "respiratory_disease", "aids_related", "all_other_illnesses", "suicide",
           "drug_alcohol_intoxication", "accident", "homicide_d")

gender_health_data <- health_demographic_data_clean %>% filter(x1 %in% c("Male", "Female"))

gender_health_data_long <- pivot_longer(gender_health_data, cols = -x1, names_to = "Cause", values_to = "Count")

# Calculate proportions within each category
gender_health_data_long <- gender_health_data_long %>%
  group_by(x1) %>%
  mutate(Proportion = Count / sum(Count))

# Plot the proportions graph using geom_bar with position = "fill"
ggplot(gender_health_data_long, aes(x = x1, y = Proportion, fill = Cause)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportions of Causes of Death by Category",
       x = "Gender",
       y = "Proportion") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

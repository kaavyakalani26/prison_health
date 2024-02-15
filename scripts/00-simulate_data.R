#### Preamble ####
# Purpose: Simulating the main datasets that will be used for this paper
# Author: Kaavya Kalani, Monica Sainani, Aamishi Avarsekar
# Date: 15 February 2024
# Contact: kaavya.kalani@mail.utoronto.ca, monica.sainani@mail.utoronto.ca, aamishi.avarsekar@mail.utoronto.ca
# License: MIT
# Pre-requisites: None

# Parts of gender simulation dataset been generated using ChatGPT3.5 and the chat
# is included in the LLM usage

# Set seed for reproducibility
set.seed(123)

# Generate random data for Male
male_data <- data.frame(
  Gender = "Male",
  Cancer = rpois(1, lambda = 14000),
  `Heart_Disease` = rpois(1, lambda = 13200),
  `Liver_Disease` = rpois(1, lambda = 4800),
  `Respiratory_Disease` = rpois(1, lambda = 3100),
  `Aid_related` = rpois(1, lambda = 1800)
)

# Generate random data for Female
female_data <- data.frame(
  Gender = "Female",
  Cancer = rpois(1, lambda = 550),
  `Heart_Disease` = rpois(1, lambda = 460),
  `Liver_Disease` = rpois(1, lambda = 170),
  `Respiratory_Disease` = rpois(1, lambda = 170),
  `Aid_related` = rpois(1, lambda = 100)
)

# Create data frame
sim_data <- rbind(male_data, female_data)


# Tests written by author (no LLM used):
# None of the values must be null or missing

# Gender:
gender_test_1 <- all(sim_data$Cancer > 0) & all(sim_data$Heart_Disease > 0) &
  all(sim_data$Liver_Disease > 0) & all(sim_data$Respiratory_Disease > 0
                                        & all(sim_data$Aid_related > 0))
stopifnot(gender_test_1 == TRUE)

# Test data gender types
stopifnot(is.numeric(sim_data$Cancer))
stopifnot(is.numeric(sim_data$Heart_Disease))
stopifnot(is.numeric(sim_data$Liver_Disease))
stopifnot(is.numeric(sim_data$Respiratory_Disease))
stopifnot(is.numeric(sim_data$Aid_related))


# Simulating the death causes by race:
black_data <- data.frame(
  Race = "Black",
  Cancer = rpois(1, lambda = 14000),
  `Heart_Disease` = rpois(1, lambda = 13200),
  `Liver_Disease` = rpois(1, lambda = 60),
  `Respiratory_Disease` = rpois(1, lambda = 3100),
  `Aid_related` = rpois(1, lambda = 1800)
)

hispanic_data <- data.frame(
  Race = "Hispanic",
  Cancer = rpois(1, lambda = 1400),
  `Heart_Disease` = rpois(1, lambda = 13200),
  `Liver_Disease` = rpois(1, lambda = 900),
  `Respiratory_Disease` = rpois(1, lambda = 150),
  `Aid_related` = rpois(1, lambda = 1800)
)

asian_data <- data.frame(
  Race = "Asian",
  Cancer = rpois(1, lambda = 1230),
  `Heart_Disease` = rpois(1, lambda = 1300),
  `Liver_Disease` = rpois(1, lambda = 4200),
  `Respiratory_Disease` = rpois(1, lambda = 900),
  `Aid_related` = rpois(1, lambda = 800)
)

white_data <- data.frame(
  Race = "White",
  Cancer = rpois(1, lambda = 1400),
  `Heart_Disease` = rpois(1, lambda = 1300),
  `Liver_Disease` = rpois(1, lambda = 470),
  `Respiratory_Disease` = rpois(1, lambda = 300),
  `Aid_related` = rpois(1, lambda = 180)
)

sim_race_data <- rbind(black_data, asian_data,
                       hispanic_data, white_data)


# Tests written by author (no LLM used):
# None of the values must be null or missing

# Gender:
race_test <- all(sim_race_data$Cancer > 0) & all(sim_race_data$Heart_Disease > 0) &
  all(sim_race_data$Liver_Disease > 0) & all(sim_race_data$Respiratory_Disease > 0
                                        & all(sim_race_data$Aid_related > 0))
stopifnot(race_test == TRUE)

# Test data gender types
stopifnot(is.numeric(sim_race_data$Cancer))
stopifnot(is.numeric(sim_race_data$Heart_Disease))
stopifnot(is.numeric(sim_race_data$Liver_Disease))
stopifnot(is.numeric(sim_race_data$Respiratory_Disease))
stopifnot(is.numeric(sim_race_data$Aid_related))





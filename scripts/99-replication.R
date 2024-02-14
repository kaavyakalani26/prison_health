#### Preamble ####
# Purpose: Replicated graphs from https://pubs.aeaweb.org/doi/pdfplus/10.1257/jep.35.4.97
# Author: Kaavya Kalani, Monica Sainani, Aamishi Avarsekar
# Date: 12 February 2024
# Contact: kaavya.kalani@mail.utoronto.ca, monica.sainani@mail.utoronto.ca, aamishi.avarsekar@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Note: I will be replicating Figure 2, Figure 3 and Figure 4 from the paper


#### Workspace setup ####
# install.packages("tidyverse")
# install.packages("gdata")
# install.packages("dplyr")
# install.packages("haven")
# install.packages("ggplot2")
# install.packages("zoo")
library(tidyverse)
library(gdata)
library(dplyr)
library(haven)
library(ggplot2)
library(zoo)

# Figure 2 replication

x <- read_dta("data/spi86to16.dta")
x <- x[x$admit == 1, ]

# Create data frames for each program
drugev <- x %>% group_by(region, year) %>% summarize(xbar = weighted.mean(drugev, wt, na.rm = TRUE))
vocev <- x %>% group_by(region, year) %>% summarize(xbar = weighted.mean(vocev, wt, na.rm = TRUE))
work <- x %>% group_by(region, year) %>% summarize(xbar = weighted.mean(work, wt, na.rm = TRUE))
edev <- x %>% group_by(region, year) %>% summarize(xbar = weighted.mean(edev, wt, na.rm = TRUE))

# Combine all data frames into a single data frame
programs_data <- bind_rows(
  mutate(drugev, program = "Drug Program"),
  mutate(vocev, program = "Job Training"),
  mutate(work, program = "Work"),
  mutate(edev, program = "Education")
)

# Convert region to factor with explicit levels
programs_data$region <- factor(programs_data$region, levels = 1:4, labels = c("Northeast", "Midwest", "South", "West"))

# Create the figure using ggplot2
ggplot(programs_data, aes(x = year, y = xbar * 100, group = region, color = region, shape = region)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  facet_wrap(~program, scales = "free_y", ncol = 4) +
  theme_minimal() +
  scale_x_continuous(limits = c(1986, 2016), breaks = seq(1986, 2016, 10)) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 20)) +
  labs(x = "Year", y = "Percent", title = "Enrollment in Drug, Education, Job Training Programs, and Work Assignment,
State Prisoners, by Region") +
  scale_color_manual(values = c("skyblue2", "dodgerblue", "red", "blue"),
                     name = "Region", labels = c("Northeast", "Midwest", "South", "West")) +
  scale_shape_manual(values = c(16, 16, 16, 16), name = "Region", labels = c("Northeast", "Midwest", "South", "West"))


# Figure 3 replication

pop <- c(1217352, 412783, 328239523)  # population denominators

# Read CSV file with the correct date format
x <- read.csv("data/covidts01.csv", header = TRUE, stringsAsFactors = FALSE)
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

# Create the figure using ggplot2 with solid lines
ggplot(ts_data, aes(x = date, y = new_cases_per_100k, color = group)) +
  geom_line(size = 0.8) +  # Adjust the line thickness here (e.g., size = 0.8)
  scale_x_date(date_labels = "%B", date_breaks = "1 month", limits = c(min(ts_data$date), max(ts_data$date))) +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, 25)) +
  labs(x = NULL, y = "New cases per 100,000", title = "Seven-Day Moving Average of New Daily COVID-19 Cases per 100,000 Among
Those in Prison, Prison Staff, and in the General Population: May to September
2020") +
  theme_minimal() +
  geom_vline(xintercept = as.numeric(as.Date(c("2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01"))),
             linetype = "dashed", color = "gray") +
  annotate("text", x = as.Date(c("2020-04-01", "2020-05-01", "2020-06-01", "2020-07-01", "2020-08-01", "2020-09-01")),
           y = 150, label = c("April 2020", "May 2020", "June 2020", "July 2020", "August 2020", "September 2020"),
           vjust = -0.5, hjust = 0, color = "gray") +
  scale_color_manual(values = c("blue", "red", "green"))


# Figure 4 replication

# Read state covid data
x <- read.csv("data/all-states-history.csv", header = TRUE)

# Data from 1/22/21
x122 <- x[x$date == "2021-01-22",]

# Read state population data
pop <- read.csv("data/statepop.csv", header = FALSE)

# Matching states
ind <- match(x122$state, pop[,2])

# Adding state population and calculating state case rate
x122$pop <- pop[ind,3]
x122$ccr <- x122$positive / (x122$pop * 1e6)

# Read prison covid data
x1 <- read.csv("data/COVID-19 Cases in State and Federal Prison Systems.csv", header = TRUE)

# Data cleaning
x1 <- lapply(x1, function(i) { sub(",", "", i) })
x1 <- lapply(x1, function(i) { sub("NR", NA, i) })
x1[names(x1)[c(2:7,9)]] <- lapply(x1[names(x1)[c(2:7,9)]], as.numeric)
x1$incpop <- x1$Incarcerated.Positive / x1$Incarcerated.Case.rate.per.1.000

# Matching states
ind <- match(x122$state, x1$Prison.System)

# Creating a data frame for case rates
ccr <- data.frame(
  pop = 1000 * x122$ccr,
  prisn = x1$Incarcerated.Case.rate.per.1.000[ind],
  state = x1$state[ind]
)

# Remove NAs
ccr <- ccr[complete.cases(ccr),]

# Order by prison case rate
o1 <- order(ccr$prisn)
ccr <- ccr[o1,]

# Add row numbers to the data frame
ccr$row_number <- 1:nrow(ccr)

# Creating the figure using ggplot2
ggplot(ccr, aes(x = prisn, y = row_number)) +
  geom_point(aes(color = "Prison"), size = 2) +
  geom_point(aes(x = pop, color = "General population"), size = 2) +
  scale_color_manual(values = c("red", "blue")) +
  labs(x = "Cumulative Case Rate (per 1000)", y = NULL, title="Cumulative COVID-19 Case Rates among Those in Prison and General
Population, by State: March 2020 to January 2021") +
  theme_minimal() +
  theme(legend.position = c(0.8, 0.2)) +
  geom_hline(yintercept = 1:nrow(ccr), linetype = "dashed", color = "gray", size = 0.5, alpha = 0.5) +
  scale_x_continuous(limits = c(0, 800)) +
  scale_y_continuous(breaks = 1:nrow(ccr), labels = ccr$state, position = "right") +
  guides(color = guide_legend(title = NULL, override.aes = list(shape = c(16, 16))))

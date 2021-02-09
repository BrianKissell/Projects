### This code is not yet complete, and is only being run on the data from 100 awards.

# Import packages
library(tidyverse)
library(stringr)
library(tidyr)
library(ggthemes)
library(lubridate)

# Set theme
theme_set(theme_gdocs())

# Read in data
df_scraped_data <-  read_csv("df_scraped_data.csv") %>% drop_na(amount)

# Look over dataframe
glimpse(df_scraped_data)

# Check to make sure all are from the Department of Defense
table(df_scraped_data$agency)

# Check how many awards per branch
table(df_scraped_data$branch)

# Visualize awards per branch
ggplot(df_scraped_data, aes(x=branch)) +
  geom_bar()+
  coord_flip() +
  labs(title = "Number of Awards per Branch", x = "Branch Name")

# Convert amount to a numeric variable
df_scraped_data$amount <- str_remove_all(df_scraped_data$amount,"\\$") %>%
  str_remove_all("\\,") %>% as.numeric()

# Visualize distribution of awards
ggplot(df_scraped_data, aes(x = amount)) +
  geom_histogram()+
  labs(title = "Distibution of Award Amounts", x = "Amount of Award", y= "Count")

# How many award are in each phase?
table(df_scraped_data$phase)

# Since this is a factor of interest, I will convert it into a factor
df_scraped_data$phase <- factor(df_scraped_data$phase)

# Visualize distribution of awards by factor
ggplot(df_scraped_data, aes(x = amount, fill = phase)) +
  geom_histogram(position = "dodge")+
  labs(title = "Distibution of Award Amounts for Each Phase", x = "Amount of Award", y = "Count", fill = "Phases")

# How many awards per program?
table(df_scraped_data$program)

# Visualize awards per program
ggplot(df_scraped_data, aes(x=program)) +
  geom_bar()+
  coord_flip() +
  labs(title = "Number of Awards per Program", x = "Program Name")

# Convert award year into a date
ymd(df_scraped_data$award_year, truncated = 2L)

# Visualize awards per year separated by phase
ggplot(df_scraped_data, aes(x = award_year, fill = phase)) +
  geom_histogram() +
  facet_wrap(~phase) +
  labs(title = "Number of Awards per Year", x = "Year")

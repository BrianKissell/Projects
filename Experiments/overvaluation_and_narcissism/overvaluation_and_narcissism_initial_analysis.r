# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(lubridate)
library(naniar)
library(broom)
library(emmeans)
theme_set(theme_gdocs())

# Import data
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/overvaluation_and_narcissism/overvaluation_and_narcissism.csv")

# Check data frame and data types
glimpse(df)
summary(df)

# Clean and prepare data
df_clean <- df %>%
  rename(gender = sex)%>%
  mutate(
    condition = factor(condition, levels = c(1,2,3), labels = c("Control", "Cold", "Overvalue")), 
    gender = factor(gender, levels = c(0,1), labels = c("Male", "Female")),
    npi_total = npi_total / 40,
    date = mdy(date),
    time = hms(time)
  )

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)

# Check assumptions for analysis of variance 
# 1) The responses for each factor level have a normal population distribution. 

# Visually inspect distributions
df_clean %>%
  ggplot(aes(npi_total, color = condition)) +
    geom_histogram() +
    facet_wrap(~condition) +
    labs(x = "Narcissistic Personality Inventory")

# Run Shapiro's test
control_npi <- df_clean %>% filter(condition == "Control")
shapiro.test(control_npi$npi_total)
cold_npi <- df_clean %>% filter(condition == "Cold")
shapiro.test(cold_npi$npi_total)
overvalue_npi <- df_clean %>% filter(condition == "Overvalue")
shapiro.test(overvalue_npi$npi_total)

# 2) These distributions have the same variance.
bartlett.test(df_clean$npi_total ~ df_clean$condition)

# 3) The data are independent. Random assignment was used to select groups.

# Conduct analysis of variance
anova_1 <- aov(npi_total ~ condition, data = df_clean) %>% tidy() 
anova_1_means <- emmeans(aov(npi_total ~ condition, data = df_clean), ~condition) %>% tidy()

# Create figure to evaluate the relationship between the Narcissistic Personality Inventory, the experimental condition, and the gender of the participants.
ggplot(df_clean, aes(x = condition, y = npi_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  labs(title = "Task-feedback and the Narcissistic Personality Inventory", y = "Proportion of NPI Items Selected", x = "Task Feedback Condition", fill = "Gender of Participant") +
  scale_y_continuous( breaks = c(0.0, .25, .5, .75, 1.0))

# Check assumptions for analysis of variance for the 2X3 ANOVA
# 1) The responses for each factor level have a normal population distribution. 

# Visually inspect distributions
df_clean %>%
  ggplot(aes(npi_total, color = gender)) +
  geom_histogram() +
  facet_wrap(~condition *gender) +
  labs(x = "Narcissistic Personality Inventory")

# Run Shapiro's test
control_npi <- df_clean %>% filter(condition =="Control", gender == "Male")
shapiro.test(control_npi$npi_total)
cold_npi <- df_clean %>% filter(condition =="Cold", gender == "Male")
shapiro.test(cold_npi$npi_total)
overvalue_npi <- df_clean %>% filter(condition =="Overvalue", gender == "Male")
shapiro.test(overvalue_npi$npi_total)
control_npi <- df_clean %>% filter(condition =="Control", gender == "Female")
shapiro.test(control_npi$npi_total)
cold_npi <- df_clean %>% filter(condition =="Cold", gender == "Female")
shapiro.test(cold_npi$npi_total)
overvalue_npi <- df_clean %>% filter(condition =="Overvalue", gender == "Female")
shapiro.test(overvalue_npi$npi_total)

# 2) These distributions have the same variance.
male_npi <- df_clean %>% filter(gender == "Male")
bartlett.test(male_npi$npi_total ~ male_npi$condition )
female_npi <- df_clean %>% filter(gender == "Female")
bartlett.test(female_npi$npi_total ~ female_npi$condition )

# 3) The data are independent. Random assignment was used to select groups.

# Conduct analysis of variance with gender added.
anova_2 <- aov(npi_total ~ condition * gender, data = df_clean) %>% tidy()
anova_2_means <- emmeans(aov(npi_total ~ condition * gender, data = df_clean), ~condition | gender) %>% tidy()
anova_2_means_gender <- emmeans(aov(npi_total ~ condition * gender, data = df_clean), ~ gender) %>% tidy()

# Create figure to evaluate the relationship between the Narcissistic Personality Inventory, the experimental condition, and the gender of the participants.
ggplot(df_clean, aes(x = condition, y = npi_total, fill = gender)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("blue", "red"))+
  labs(title = "Task-feedback, Gender, and the Narcissistic Personality Inventory", y = "Proportion of NPI Items Selected", x = "Task Feedback Condition", fill = "Gender of Participant") +
  scale_y_continuous( breaks = c(0.0, .25, .5, .75, 1.0))
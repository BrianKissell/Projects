# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(lubridate)
library(naniar)
library(broom)
library(emmeans)
library(outliers)
library(ggpubr)
library(moments)
theme_set(theme_gdocs())

# Import data
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/overvaluation_and_narcissism_study_1/overvaluation_and_narcissism.csv")

# Check data frame and data types
glimpse(df)
summary(df)

# Clean and prepare data
df_clean <- df %>% rename(gender = sex) %>% 
  mutate(condition = factor(condition, levels = c(1, 2, 3), labels = c("Control", "Cold", "Overvalue")), 
         gender = factor(gender, levels = c(0, 1), labels = c("Male", "Female")), 
         npi_total = npi_total/40, date = mdy(date), time = hms(time))

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)

# Outlier Detection
boxplot(df_clean$npi_total, xlab = "Narcissitic Personality Inventory")
grubbs.test(df_clean$npi_total)

# Check assumptions for analysis of variance 1) The responses for each
# factor level have a normal population distribution.  Visually inspect
# distribution
df_clean %>% ggplot(aes(npi_total)) + 
  geom_density() + 
  stat_overlay_normal_density(color = "red",linetype = "dashed") + 
  labs(x = "Narcissistic Personality Inventory")

# Run Shapiro's test
shapiro.test(df_clean$npi_total)

# Check Skewness
skewness(df_clean$npi_total, na.rm = TRUE)

# As per tests, the distribution is not normal and needs to be
# transformed
df_clean$npi_total_sqrt <- sqrt(df_clean$npi_total)

# Visually inspect transformed distribution
df_clean %>% 
  ggplot(aes(npi_total_sqrt)) + 
  geom_density() + 
  stat_overlay_normal_density(color = "red", linetype = "dashed") + 
  labs(x = "Narcissistic Personality Inventory")

# Run Shapiro's test for transformed variable, and check for skewness
shapiro.test(df_clean$npi_total_sqrt)
skewness(df_clean$npi_total_sqrt, na.rm = TRUE)

# 2) These distributions have the same variance.
bartlett.test(df_clean$npi_total_sqrt ~ df_clean$condition)

# 3) The data are independent. Random assignment was used to select
# groups.  Conduct analysis of variance
anova_1 <- aov(npi_total_sqrt ~ condition, data = df_clean) %>% 
  tidy()
print(anova_1)

# Create figure to evaluate the relationship between the Narcissistic
# Personality Inventory, the experimental condition, and the gender of
# the participants.
ggplot(df_clean, aes(x = condition, y = npi_total_sqrt, fill = condition)) + 
  geom_violin(trim = FALSE, position = position_dodge(1), alpha = 0.5) + 
  geom_boxplot(position = position_dodge(1), width = 0.25) + 
  labs(title = "Task-feedback and the Narcissistic Personality Inventory", 
       y = "Square Root of the Proportion of NPI Items that were Selected", 
       x = "Task Feedback Condition", fill = "Gender of Participant") + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))

###################################################################################### Conduct analysis of variance with gender added.
anova_2 <- aov(npi_total_sqrt ~ condition * gender, data = df_clean) %>% 
  tidy()
print(anova_2)

# Create figure to evaluate the relationship between the Narcissistic
# Personality Inventory, the experimental condition, and the gender of
# the participants.
ggplot(df_clean, aes(x = condition, y = npi_total, fill = gender)) + 
  geom_violin(trim = FALSE, position = position_dodge(1), alpha = 0.5) + 
  geom_boxplot(position = position_dodge(1), width = 0.25) + 
  scale_fill_manual(values = c("blue", "red")) + 
  labs(title = "Task-feedback, Gender, and the Narcissistic Personality Inventory", 
       y = "Square Root of the Proportion of NPI Items that were Selected", 
       x = "Task Feedback Condition", fill = "Gender of Participant") + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1))

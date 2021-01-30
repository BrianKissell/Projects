# Load packages
library(dplyr)
library(ggplot2)
library(readr)
library(ggthemes)
library(lubridate)
library(naniar)
library(broom)
library(emmeans)
library(caret)
library(forcats)
theme_set(theme_gdocs())

# Import data
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/mindfulness_meditation_serial_associative_cognition/mindfulness_meditation_serial_associative_cognition.csv")

# Check data
glimpse(df)
summary(df)

# Clean and prepare data
df_clean <- df %>%
  mutate(
    date = mdy(date),
    time = hms(time),
    condition_mindfulness = factor(condition_mindfulness , levels = c(1,2), labels = c("Control", "Mindfulness")))

# Check data again
glimpse(df_clean)

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)

# Prepare data to test assumptions
control_df <- df_clean %>% filter(condition_mindfulness == "Control")
mindfulness_df <- df_clean %>% filter(condition_mindfulness == "Mindfulness")

############################# avoidance_of_anchoring_total ###########################################
## Check assumptions for avoidance_of_anchoring_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(avoidance_of_anchoring_total, color = condition_mindfulness)) +
  geom_histogram() +
  facet_wrap(~condition_mindfulness) +
  labs(x = "avoidance_of_anchoring_total")

# Check for normality
shapiro.test(control_df$avoidance_of_anchoring_total)
shapiro.test(mindfulness_df$avoidance_of_anchoring_total)

# Check for equal variances.
bartlett.test(df_clean$avoidance_of_anchoring_total ~ df_clean$condition_mindfulness)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition_mindfulness, y = avoidance_of_anchoring_total, fill = condition_mindfulness)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Mindfulness and avoidance_of_anchoring_total", y = "avoidance_of_anchoring_total", x = "Mindfulness Condition", fill = "Mindfulness Condition")

############################# avoidance_of_preference_anomalies_total ###########################################
## Check assumptions for avoidance_of_preference_anomalies_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(avoidance_of_preference_anomalies_total, color = condition_mindfulness)) +
  geom_histogram() +
  facet_wrap(~condition_mindfulness) +
  labs(x = "avoidance_of_preference_anomalies_total")

# Check for normality
shapiro.test(control_df$avoidance_of_preference_anomalies_total)
shapiro.test(mindfulness_df$avoidance_of_preference_anomalies_total)

# Check for equal variances.
bartlett.test(df_clean$avoidance_of_preference_anomalies_total ~ df_clean$condition_mindfulness)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition_mindfulness, y = avoidance_of_preference_anomalies_total, fill = condition_mindfulness)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Mindfulness and avoidance_of_preference_anomalies_total", y = "avoidance_of_preference_anomalies_total", x = "Mindfulness Condition", fill = "Mindfulness Condition")

############################# avoidance_of_framing_total ###########################################
## Check assumptions for avoidance_of_framing_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(avoidance_of_framing_total, color = condition_mindfulness)) +
  geom_histogram() +
  facet_wrap(~condition_mindfulness) +
  labs(x = "avoidance_of_framing_total")

# Check for normality
shapiro.test(control_df$avoidance_of_framing_total)
shapiro.test(mindfulness_df$avoidance_of_framing_total)

# Check for equal variances.
bartlett.test(df_clean$avoidance_of_framing_total ~ df_clean$condition_mindfulness)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition_mindfulness, y = avoidance_of_framing_total, fill = condition_mindfulness)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Mindfulness andavoidance_of_framing_total", y = "avoidance_of_framing_total", x = "Mindfulness Condition", fill = "Mindfulness Condition")

############################# serial_associative_cognition_total ###########################################
## Check assumptions for serial_associative_cognition_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(serial_associative_cognition_total, color = condition_mindfulness)) +
  geom_histogram() +
  facet_wrap(~condition_mindfulness) +
  labs(x = "serial_associative_cognition_total")

# Check for normality
shapiro.test(control_df$serial_associative_cognition_total)
shapiro.test(mindfulness_df$serial_associative_cognition_total)

# Check for equal variances.
bartlett.test(df_clean$serial_associative_cognition_total ~ df_clean$condition_mindfulness)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition_mindfulness, y = serial_associative_cognition_total, fill = condition_mindfulness)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Mindfulness and serial_associative_cognition_total", y = "serial_associative_cognition_total", x = "Mindfulness Condition", fill = "Mindfulness Condition")

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
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/embodied_cognition_cognitive_style_and_prayer/embodied_cognition_cognitive_style_and_prayer.csv")

# Check data
glimpse(df)
summary(df)

# Clean and prepare data
df_clean <- df %>%
  rename(gender = sex) %>%
  mutate(
    gender = factor(gender, levels = c(1,2), labels = c("Male", "Female")),
    cognitive_task = factor(cognitive_task, levels = c(1,2), labels = c("Pray", "Think")), 
    physical_position = factor(physical_position, levels = c(1,2), labels = c("Kneel", "Stand")))

# Check data again
glimpse(df_clean)

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)
df_clean %>% filter(are_na(age) == TRUE)
df_clean %>% filter(are_na(religious_denomination) == TRUE)

# Prepare data to test assumptions
kneel_pray <- df_clean %>% filter(physical_position =="Kneel", cognitive_task == "Pray")
kneel_think <- df_clean %>% filter(physical_position =="Kneel", cognitive_task == "Think")
stand_pray <- df_clean %>% filter(physical_position =="Stand", cognitive_task == "Pray")
stand_think <- df_clean %>% filter(physical_position =="Stand", cognitive_task == "Think")

############################# Need for Cognition ###########################################
## Check assumptions for need for cognition
# Visually inspect distributions
df_clean %>%
  ggplot(aes(nfc_total, color = cognitive_task)) +
  geom_histogram() +
  facet_wrap(~physical_position* cognitive_task) +
  labs(x = "Need for Cognition")

# Check for normality
shapiro.test(stand_think$nfc_total)
shapiro.test(kneel_pray$nfc_total)
shapiro.test(kneel_think$nfc_total)
shapiro.test(stand_pray$nfc_total)

# Check for equal variances.
bartlett.test(df_clean$nfc_total ~ df_clean$physical_position )
bartlett.test(df_clean$nfc_total ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = nfc_total, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, Need for Cognition", y = "Need for Cognition Score", x = "Cognitive Task", fill = "Physical Position")

################################# Faith in Intuition ######################################
## Check assumptions for faith in intuition
# Visually inspect distributions
df_clean %>%
  ggplot(aes(fii_total, color = cognitive_task)) +
  geom_histogram() +
  facet_wrap(~physical_position* cognitive_task) +
  labs(x = "Faith in Intuition")

# Check for normality
shapiro.test(stand_think$fii_total)
shapiro.test(kneel_pray$fii_total)
shapiro.test(kneel_think$fii_total)
shapiro.test(stand_pray$fii_total)

# Check for equal variances.
bartlett.test(df_clean$fii_total ~ df_clean$physical_position )
bartlett.test(df_clean$fii_total ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = fii_total, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, and Faith in Intuition", y = "Faith in Intuition", x = "Cognitive Task", fill = "Physical Position")

#################################### Cognitive Reflection Task #####################################
## Check assumptions for cognitive reflection test
# Visually inspect distributions
df_clean %>%
  ggplot(aes(crt_total, color = cognitive_task)) +
  geom_histogram() +
  facet_wrap(~physical_position* cognitive_task) +
  labs(x = "Cognitive Reflection Task")

# Check for normality
shapiro.test(stand_think$crt_total)
shapiro.test(kneel_pray$crt_total)
shapiro.test(kneel_think$crt_total)
shapiro.test(stand_pray$crt_total)

# Check for equal variances
bartlett.test(df_clean$crt_total ~ df_clean$physical_position )
bartlett.test(df_clean$crt_total ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = crt_total, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, and the Cognitive Reflection Test", y = "Cognitive Reflection Test", x = "Cognitive Task", fill = "Physical Position")

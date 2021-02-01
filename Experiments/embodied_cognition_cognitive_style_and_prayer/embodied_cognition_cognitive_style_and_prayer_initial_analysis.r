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
library(tidyr)
library(outliers)
library(ggpubr)
library(moments)
library(GGally)
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
    physical_position = factor(physical_position, levels = c(1,2), labels = c("Kneel", "Stand")),
    male = ifelse(gender == "Male", 1, 0),
    female = ifelse(gender == "Female", 1, 0),
    christian = factor(ifelse(denomination == 1, 1, 0)),
    nonaffiliated = factor(ifelse(denomination == 2, 1, 0)),
    atheist = factor(ifelse(denomination == 3, 1, 0)),
    other_religion = factor(ifelse(denomination ==4, 1, 0))
)
# Check data again
glimpse(df_clean)

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)

# Impute missing data
df_clean <- df_clean %>%
  mutate(age  = ifelse(is.na(age), mean(age, na.rm = TRUE), age),
         religious_denomination  = ifelse(is.na(religious_denomination), "NA", religious_denomination))

#Check again for missing data
any_na(df_clean)

############################# Need for Cognition ###########################################
## Check assumptions for need for cognition
# Visually inspect distributions
df_clean %>%
  ggplot(aes(nfc_total)) +
  geom_density() +
  stat_overlay_normal_density(color = "red", linetype = "dashed") +
  labs(x = "Need for Cognition")

# Run Shapiro's test
shapiro.test(df_clean$nfc_total)

# Check Skewness
skewness(df_clean$nfc_total, na.rm = TRUE)

# Check for equal variances.
bartlett.test(df_clean$nfc_total ~ df_clean$physical_position )
bartlett.test(df_clean$nfc_total ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = nfc_total, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, Need for Cognition", y = "Need for Cognition Score", x = "Cognitive Task", fill = "Physical Position")

# Due to unequal variance across groups, the assumptions of analysis of variance are not met. Thus I instead ran Welch's Anova.

df_clean %>% rstatix::welch_anova_test(nfc_total ~ cognitive_task)
df_clean %>% rstatix::welch_anova_test(nfc_total ~ physical_position)
df_clean %>% rstatix::group_by(physical_position) %>% welch_anova_test(nfc_total ~ cognitive_task)
df_clean %>% rstatix::group_by(cognitive_task) %>% welch_anova_test(nfc_total ~ physical_position )

################################# Faith in Intuition ######################################
## Check assumptions for faith in intuition
# Visually inspect distributions
df_clean %>%
  ggplot(aes(fii_total)) +
  geom_density() +
  stat_overlay_normal_density(color = "red", linetype = "dashed") +
  labs(x = "Faith in Intuition")

# Run Shapiro's test
shapiro.test(df_clean$fii_total)

# Check Skewness
skewness(df_clean$fii_total, na.rm = TRUE)

# Anchor variable at 1, and then transform the variable.
summary(df_clean$fii_total)
df_clean$fii_total_square <- (df_clean$fii_total - 18) ** 2

# Visually inspect distributions
df_clean %>%
  ggplot(aes(fii_total_square)) +
  geom_density() +
  stat_overlay_normal_density(color = "red", linetype = "dashed") +
  labs(x = "Faith in Intuition Squared")

# Run Shapiro's test
shapiro.test(df_clean$fii_total_square)

# Check Skewness
skewness(df_clean$fii_total_square, na.rm = TRUE)

# Check for equal variances
bartlett.test(df_clean$fii_total_square ~ df_clean$physical_position )
bartlett.test(df_clean$fii_total_square ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = fii_total_square, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, and Faith in Intuition", y = "Faith in Intuition", x = "Cognitive Task", fill = "Physical Position")

# Run analysis of variance
summary(aov(fii_total_square ~ cognitive_task * physical_position, data = df_clean))

#################################### Cognitive Reflection Task #####################################
## Check assumptions for cognitive reflection test
# Visually inspect distributions
df_clean %>%
  ggplot(aes(crt_total)) +
  geom_density() +
  stat_overlay_normal_density(color = "red", linetype = "dashed") +
  labs(x = "Cognitive Reflection Task")

# Check for normality
shapiro.test(df_clean$crt_total)

# Check Skewness
skewness(df_clean$crt_total, na.rm = TRUE)

# Check for equal variances
bartlett.test(df_clean$crt_total ~ df_clean$physical_position )
bartlett.test(df_clean$crt_total ~ df_clean$cognitive_task)

# Create visualization of relationship between position and task
ggplot(df_clean, aes(x = cognitive_task, y = crt_total, fill = physical_position)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green"))+
  labs(title = "Cognitive Task, Physical Position, and the Cognitive Reflection Test", y = "Cognitive Reflection Test", x = "Cognitive Task", fill = "Physical Position")

#Run Welch's ANOVA
df_clean %>% rstatix::welch_anova_test(crt_total ~ cognitive_task)
df_clean %>% rstatix::welch_anova_test(crt_total ~ physical_position)
df_clean %>% rstatix::group_by(physical_position) %>% welch_anova_test(crt_total ~ cognitive_task)
df_clean %>% rstatix::group_by(cognitive_task) %>% welch_anova_test(crt_total ~ physical_position )

summary(glm(crt_total~cognitive_task*physical_position, data = df_clean, family = "poisson"))

##### Create visualization with most of the variables
cor_df <- df_clean %>% select(age, gender, belief_in_god, certainty_of_belief_in_god, frequency_of_prayer, duration_of_prayer, frequency_of_religious_services, christian, nonaffiliated, atheist, other_religion, nfc_total, fii_total, crt_total)
ggpairs(cor_df)

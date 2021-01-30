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
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/experimental_evaluation_perceived_threat_to_identity/experimental_evaluation_perceived_threat_to_identity.csv")

# Check data
glimpse(df)
summary(df)

# Clean and prepare data
df_clean <- df %>%
  mutate(
    dem_gender = factor(dem_gender),
    condition = factor(condition))

# Check data again
glimpse(df_clean)

# Check for missing data
any_na(df_clean)
vis_miss(df_clean)
df_clean %>% filter(are_na(age) == TRUE)
df_clean %>% filter(are_na(religious_denomination) == TRUE)

# Prepare data to test assumptions
raelian_df <- df_clean %>% filter(condition =="Raelian Threat")
nonreligious_df <- df_clean %>% filter(condition =="Non-religious Threat")
religious_df <- df_clean %>% filter(condition =="Religious Threat")

############################# Perceived Threat to Identity ###########################################
## Check assumptions for perceived threat to identity
# Visually inspect distributions
df_clean %>%
  ggplot(aes(perceived_threat_to_identity_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "Perceived Threat to Identity")

# Check for normality
shapiro.test(raelian_df$perceived_threat_to_identity_total)
shapiro.test(nonreligious_df$perceived_threat_to_identity_total)
shapiro.test(religious_df$perceived_threat_to_identity_total)

# Check for equal variances.
bartlett.test(df_clean$perceived_threat_to_identity_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = perceived_threat_to_identity_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and Perceived Threat to Identity", y = "Perceived Threat to Identity", x = "Threat Condition", fill = "Threat Condition")

############################# Importance of Identity ###########################################
## Check assumptions for perceived threat to identity
# Visually inspect distributions
df_clean %>%
  ggplot(aes(importance_of_identity_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "Importance of Identity")

# Check for normality
shapiro.test(raelian_df$importance_of_identity_total)
shapiro.test(nonreligious_df$importance_of_identity_total)
shapiro.test(religious_df$importance_of_identity_total)

# Check for equal variances.
bartlett.test(df_clean$importance_of_identity_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = importance_of_identity_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and Importance of Identity", y = "Importance of Identity", x = "Threat Condition", fill = "Threat Condition")

############################# conspiracy_mentality_total ###########################################
## Check assumptions for conspiracy_mentality_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(conspiracy_mentality_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "conspiracy_mentality_total")

# Check for normality
shapiro.test(raelian_df$conspiracy_mentality_total)
shapiro.test(nonreligious_df$conspiracy_mentality_total)
shapiro.test(religious_df$conspiracy_mentality_total)

# Check for equal variances.
bartlett.test(df_clean$conspiracy_mentality_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = conspiracy_mentality_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and conspiracy_mentality_total", y = "conspiracy_mentality_total", x = "Threat Condition", fill = "Threat Condition")

############################# conservative_political_beliefs_total ###########################################
## Check assumptions for conservative_political_beliefs_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(conservative_political_beliefs_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "conservative_political_beliefs_total")

# Check for normality
shapiro.test(raelian_df$conservative_political_beliefs_total)
shapiro.test(nonreligious_df$conservative_political_beliefs_total)
shapiro.test(religious_df$conservative_political_beliefs_total)

# Check for equal variances.
bartlett.test(df_clean$conservative_political_beliefs_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = conservative_political_beliefs_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and conservative_political_beliefs_total", y = "conservative_political_beliefs_total", x = "Threat Condition", fill = "Threat Condition")

############################# need_for_closure_total ###########################################
## Check assumptions for need_for_closure_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(need_for_closure_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "need_for_closure_total")

# Check for normality
shapiro.test(raelian_df$need_for_closure_total)
shapiro.test(nonreligious_df$need_for_closure_total)
shapiro.test(religious_df$need_for_closure_total)

# Check for equal variances.
bartlett.test(df_clean$need_for_closure_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = need_for_closure_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and need_for_closure_total", y = "need_for_closure_total", x = "Threat Condition", fill = "Threat Condition")

############################# cognitive_reflection_test_total ###########################################
## Check assumptions for cognitive_reflection_test_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(cognitive_reflection_test_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "cognitive_reflection_test_total")

# Check for normality
shapiro.test(raelian_df$cognitive_reflection_test_total)
shapiro.test(nonreligious_df$cognitive_reflection_test_total)
shapiro.test(religious_df$cognitive_reflection_test_total)

# Check for equal variances.
bartlett.test(df_clean$cognitive_reflection_test_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = cognitive_reflection_test_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "Threat and cognitive_reflection_test_total", y = "cognitive_reflection_test_total", x = "Threat Condition", fill = "Threat Condition")

############################# belief_in_fake_news_total ###########################################
## Check assumptions for belief_in_fake_news_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(belief_in_fake_news_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "belief_in_fake_news_total")

# Check for normality
shapiro.test(raelian_df$belief_in_fake_news_total)
shapiro.test(nonreligious_df$belief_in_fake_news_total)
shapiro.test(religious_df$belief_in_fake_news_total)

# Check for equal variances.
bartlett.test(df_clean$belief_in_fake_news_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = belief_in_fake_news_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "belief_in_fake_news_total", y = "belief_in_fake_news_total", x = "Threat Condition", fill = "Threat Condition")

############################# belief_in_real_news_total ###########################################
## Check assumptions for belief_in_real_news_total
# Visually inspect distributions
df_clean %>%
  ggplot(aes(belief_in_real_news_total, color = condition)) +
  geom_histogram() +
  facet_wrap(~condition) +
  labs(x = "belief_in_real_news_total")

# Check for normality
shapiro.test(raelian_df$belief_in_real_news_total)
shapiro.test(nonreligious_df$belief_in_real_news_total)
shapiro.test(religious_df$belief_in_real_news_total)

# Check for equal variances.
bartlett.test(df_clean$belief_in_real_news_total ~ df_clean$condition)

# Create visualization of relationship 
ggplot(df_clean, aes(x = condition, y = belief_in_real_news_total, fill = condition)) +
  geom_violin(trim = FALSE, position = position_dodge(1), alpha=.5)+
  geom_boxplot(position=position_dodge(1), width = .25 ) +
  scale_fill_manual(values=c("purple", "green", "yellow"))+
  labs(title = "belief_in_real_news_total", y = "belief_in_real_news_total", x = "Threat Condition", fill = "Threat Condition")

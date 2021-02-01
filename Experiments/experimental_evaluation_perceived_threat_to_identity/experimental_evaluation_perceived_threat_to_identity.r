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
library(ggpubr)
library(moments)
theme_set(theme_gdocs())

# Import data
df <- read_csv("C:/Users/Brian/kissell_projects/kissell_projects/Personal_Website/Projects/Experiments/experimental_evaluation_perceived_threat_to_identity/experimental_evaluation_perceived_threat_to_identity.csv")

# Check data
glimpse(df)
summary(df)

# Check for missing data
any_na(df)
vis_miss(df)
n_miss(df)
prop_miss(df)
miss_var_summary(df)
miss_case_summary(df)

# Impute missing data
df_clean <- df %>%
  mutate(dem_orientation_4_TEXT  = ifelse(is.na(dem_orientation_4_TEXT), dem_orientation, dem_orientation_4_TEXT),
         dem_religion_12_TEXT  = ifelse(is.na(dem_religion_12_TEXT), dem_religion, dem_religion_12_TEXT),
         dem_party_4_TEXT  = ifelse(is.na(dem_party_4_TEXT), dem_party, dem_party_4_TEXT),
         dem_ethnicity_8_TEXT  = ifelse(is.na(dem_ethnicity_8_TEXT), dem_ethnicity, dem_ethnicity_8_TEXT),
         pti_ii_other_label  = ifelse(is.na(pti_ii_other_label), "NA", pti_ii_other_label),
         Real2_2 = ifelse(is.na(Real2_2), mean(Real2_2, na.rm = TRUE), Real2_2),
         Real3_2 = ifelse(is.na(Real3_2), mean(Real3_2, na.rm = TRUE), Real3_2),
         Real6_2 = ifelse(is.na(Real6_2), mean(Real6_2, na.rm = TRUE), Real6_2),
         Fake5_2 = ifelse(is.na(Fake5_2), mean(Fake5_2, na.rm = TRUE), Fake5_2),
         belief_in_real_news_total = (Real1_2 + Real2_2 + Real3_2 + Real4_2 + Real5_2 + Real6_2 + Real7_2 + Real8_2 + Real9_2 + Real10_2 + Real11_2 + Real12_2)/12,
         belief_in_fake_news_total = (Fake1_2 + Fake2_2 + Fake3_2 + Fake4_2 + Fake5_2 + Fake6_2 + Fake7_2 + Fake8_2 + Fake9_2 + Fake10_2 + Fake11_2 + Fake12_2)/12
         )

# Clean and prepare variables
df_clean <- df_clean %>%
  mutate(
    dem_gender = factor(dem_gender),
    condition = factor(condition),
    non_religious_threat = ifelse(condition == "Non-religious Threat", 1, 0),
    raelian_threat = ifelse(condition == "Raelian Threat", 1, 0),
    religious_threat = ifelse(condition == "Religious Threat", 1, 0),
    dem_employment = factor(dem_employment),
    student = ifelse(dem_employment == "A student", 1, 0),
    dem_income = factor(dem_income),
    income_less_than_39999 = ifelse(dem_income %in% c("Less than $10,000", "$10,000 to 19,999", "$20,000 to 29,999", "$30,000 to $39,999"), 1, 0),
    income_between_40000_and_79999 = ifelse(dem_income %in% c("$40,000 to $49,999", "$50,000 to $59,999", "$60,000 to $69,999", "$70,000 to $79,999", "$80,000 to $89,999"), 1, 0),
    income_more_than_90000 = ifelse(dem_income %in% c("$90,000 to $99,999", "$100,000 to $149,999", "$150,000 or more"), 1, 0),
    dem_education = factor(dem_education),
    no_bachelor_degree = ifelse(dem_education %in% c("1 or more years of college, no degree", "Associate degree (for example: AA, AS)", "High school graduate - high school diploma or the equivalent (for example: GED)", "Some college credit, but less than 1 year"), 1, 0),
    dem_religion = factor(dem_religion),
    christian = ifelse(dem_religion %in% c("Catholic", "Christian", "Protestant"), 1, 0),
    atheist = ifelse(dem_religion %in% c("Agnostic", "Atheist"), 1, 0),
    other_religion = ifelse(dem_religion %in% c("Buddhist", "Hindu", "Jewish", "Muslim", "Other"), 1, 0),
    religious = ifelse(dem_religious =="Yes", 1, 0),
    dem_party = factor(dem_party),
    democrat = ifelse(dem_party == "Democrat", 1, 0),
    republican = ifelse(dem_party == "Republican", 1, 0),
    other_political_party = ifelse(dem_party %in% c("Independent", "Other"), 1, 0),
    dem_ethnicity = factor(dem_ethnicity),
    ethnicity_white = ifelse(dem_ethnicity == "White", 1, 0),
    dem_marriage = factor(dem_marriage),
    single_never_married = ifelse(dem_marriage == "Never married", 1, 0),
    dem_orientation = factor(dem_orientation),
    sexual_orientation_straight = ifelse(dem_orientation == "Heterosexual or straight", 1, 0)
    )

# Normalize variables
df_clean$perceived_threat_to_identity_total <- scale(df_clean$perceived_threat_to_identity_total)
df_clean$conspiracy_mentality_total <- scale(df_clean$conspiracy_mentality_total)
df_clean$conservative_political_beliefs_total <- scale(df_clean$conservative_political_beliefs_total)
df_clean$need_for_closure_total <- scale(df_clean$need_for_closure_total)
df_clean$importance_of_identity_total <- scale(df_clean$importance_of_identity_total)
df_clean$cognitive_reflection_test_total <- scale(df_clean$cognitive_reflection_test_total)
df_clean$belief_in_fake_news_total <- scale(df_clean$belief_in_fake_news_total)
df_clean$belief_in_real_news_total <- scale(df_clean$belief_in_real_news_total)

# Double chek missing data
any_na(df_clean)
vis_miss(df_clean)
n_miss(df_clean)
prop_miss(df_clean)
miss_var_summary(df_clean)
miss_case_summary(df_clean)

# Check data again
glimpse(df_clean)




# Prepare data to test assumptions
raelian_df <- df_clean %>% filter(condition =="Raelian Threat")
nonreligious_df <- df_clean %>% filter(condition =="Non-religious Threat")
religious_df <- df_clean %>% filter(condition =="Religious Threat")

############################# Perceived Threat to Identity ###########################################
## Check assumptions for perceived threat to identity
# Visually inspect distributions
df_clean %>%
  ggplot(aes(perceived_threat_to_identity_total)) +
  geom_density() +
  stat_overlay_normal_density(color = "red", linetype = "dashed") +
  labs(x = "Perceived Threat to Identity")

# Check for normality
shapiro.test(df_clean$perceived_threat_to_identity_total)

#Check for skewness
skewness(df_clean$perceived_threat_to_identity_total)

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




df_vars <- df_clean %>% select(non_religious_threat, raelian_threat, religious_threat, student, income_less_than_39999, income_between_40000_and_79999, income_more_than_90000, no_bachelor_degree, christian, atheist, other_religion, religious, democrat, republican, other_political_party, ethnicity_white, single_never_married, sexual_orientation_straight,perceived_threat_to_identity_total, conspiracy_mentality_total, conservative_political_beliefs_total, need_for_closure_total, importance_of_identity_total, cognitive_reflection_test_total, belief_in_fake_news_total, belief_in_real_news_total)

summary(lm(perceived_threat_to_identity_total ~ ., df_vars))
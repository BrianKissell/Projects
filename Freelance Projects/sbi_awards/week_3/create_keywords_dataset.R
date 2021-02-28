# Import packages
library(readr)
library(dplyr)
library(wordcloud)
library(RWeka)
library(tm)
library(RColorBrewer)
library(stringr)
library(tidyr)


######################################################
########### Clean and Prepare Text from DF ###########
######################################################
text_from_df <- function(df, text){
  text_clean <- tolower(df[[text]])
  text_clean <- str_replace_all(text_clean, "[\\(\\)0-9\\:]", " ")
  text_clean <- str_replace_all(text_clean, "[-–,!@#$%^&*–]", " ")
  text_clean <- str_trim(text_clean)
  text_clean
}

######################################################
################# Create Long String #################
######################################################
# Set up function to create long string of titles from a certain year
create_string_by_year <- function(dataframe, year, text_column){ 
  df_for_year <- dataframe %>% filter(award_year == year) 
  df_for_year$text_clean <- text_from_df(df_for_year, text_column)
  paste(df_for_year$text_clean, collapse = ". ")
}


#######################################################
# Read in data
df <- read_csv("df_processed.csv")
combined_keywords <- read_csv("combined_keywords.csv")

# Only include data in phase 1
df <- df %>% filter(phase == "Phase I")

# Find the unique words in the list of keywords
unique_keywords <- combined_keywords %>%
  group_by(keywords) %>%
  slice_max(order_by = tf_idf, n = 1)

# Remove words that are clearly junk
remove_junk <- c("Ã.â..â.œ", "appliquÃ.", "n.a", "x", "X", "X.", "X..", "X..1")
unique_keywords <- unique_keywords %>% filter(!(keywords %in% remove_junk))

# Loop through unique_keywords, and extract how many times a word was used
word_count <- c()
for(i in unique_keywords$keywords){
  the_count <- sum(str_count(df$text, pattern = i))
  word_count <- c(word_count, the_count)
  print(i)
}

# Loop through unique_keywords, and extract the number of awards it appeared in
award_used_keyword <- c()
for(i in unique_keywords$keywords){
  awards_used_words <- sum(str_detect(df$text, pattern = i))
  award_used_keyword <- c(award_used_keyword, awards_used_words)
  print(i)
}

# Loop through unique_keywords, and extract the number of awards that it appeared in which were in phase 2
number_phase_2 <- c()
for(i in unique_keywords$keywords){
  number_of_phase_2_used_words <- sum(
    ifelse(
      str_detect(df$text, pattern = i),
      if_else(df$phases == "Phase I - Obtained Phase II", 1, 0), 
      0
    )
  )
  number_phase_2 <- c(number_phase_2, number_of_phase_2_used_words)
  print(i)
}

# Loop through unique_keywords, and extract the amount of money for awards that used the keyword
award_amount_total <- c()
for(i in unique_keywords$keywords){
  award_amount_total_per_keyword <- sum(
    ifelse(
      str_detect(df$text, pattern = i),
      df$amount, 
      0
    )
  )
  award_amount_total <- c(award_amount_total, award_amount_total_per_keyword)
  print(i)
}

# Add new variables to the dataframe
unique_keywords$word_count <- word_count
unique_keywords$award_used_keyword <- award_used_keyword
unique_keywords$number_phase_2 <- number_phase_2
unique_keywords$award_amount_total <- award_amount_total

# Use new variables to create more variables
unique_keywords <- unique_keywords %>%
  mutate(
    # Calculate the percentage of awards that used the word were in phase 2
    percent_phase_2 = ((number_phase_2 / award_used_keyword) * 100),
    # Get the average award amount per keyword
    mean_amount_per_keyword = award_amount_total / award_used_keyword,
    
    word_length = str_length(keywords))

# Perform a median split
unique_keywords$more_phase_2 <- ifelse(
  unique_keywords$percent_phase_2 >= quantile(unique_keywords$percent_phase_2, .75, rm.na = TRUE), "Group 4", 
  ifelse(unique_keywords$percent_phase_2 >= quantile(unique_keywords$percent_phase_2, .5, rm.na = TRUE), "Group 3",
  ifelse(unique_keywords$percent_phase_2 >= quantile(unique_keywords$percent_phase_2, .25, rm.na = TRUE), "Group 2", 
         "Group 1")))

# Remove these variable as they are somewhat irrelant now
unique_keywords <- unique_keywords %>% select(-c(year, tf_idf))

# Save the keywords data frame.
write_csv(unique_keywords, "unique_keywords.csv")


```

```{r, fig.width = 10, fig.height = 5}
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)

plot <- unique_keywords %>%
  filter(word_length > 4) %>%
  arrange(desc(award_amount_total)) %>%
  mutate(keywords = factor(keywords, keywords)) %>%  
  ggplot(aes(x = award_used_keyword, y = word_count, size = award_amount_total, fill = factor(more_phase_2), label = keywords)) +
  geom_point(alpha=0.5, shape = 21, color = "black") +
  scale_size(range = c(.1, 24), name="Award Amount ($)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="bottom") +
  ylab("Number Keyword was Used") +
  xlab("Number of Awards with Keyword") +
  theme(legend.position = "none")  
  
ggplotly(plot)
```

```{r}
ggplot(unique_keywords, aes(x = more_phase_2, y = mean_amount_per_keyword))+
  geom_boxplot()

summary(aov(mean_amount_per_keyword ~ more_phase_2, data = unique_keywords))
```

```{r, fig.width = 10, fig.height = 5}
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)

plot <- unique_keywords %>%
  arrange(desc(award_amount_total)) %>%
  mutate(keywords = factor(keywords, keywords)) %>%  
  ggplot(aes(x = award_used_keyword, y = word_count, size = mean_amount_per_keyword, fill = factor(more_phase_2), label = keywords)) +
  geom_point(alpha=0.5, shape = 21, color = "black") +
  scale_size(range = c(.1, 24), name="Mean Award Amount ($)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="bottom") +
  ylab("Number Keyword was Used") +
  xlab("Number of Awards with Keyword") +
  theme(legend.position = "none")  
  
ggplotly(plot)
```

```{r, fig.width = 10, fig.height = 5}
# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)

plot <- unique_keywords %>%
  arrange(desc(award_amount_total)) %>%
  mutate(keywords = factor(keywords, keywords)) %>%  
  ggplot(aes(x = award_used_keyword, y = word_count, size = award_amount_total, fill = factor(more_phase_2), label = keywords)) +
  geom_point(alpha=0.5, shape = 21, color = "black") +
  scale_size(range = c(.1, 24), name="Award Amount ($)") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE) +
  theme(legend.position="bottom") +
  ylab("Number Keyword was Used") +
  xlab("Number of Awards with Keyword") +
  theme(legend.position = "none")  
  
ggplotly(plot)
```




```{r}
library(dplyr)
library(ggplot2)

obtained_keywords %>%
  ggplot(aes(x = award_used_keyword, y = mean_amount_per_keyword, size = word_count, fill = factor(more_phase_2), label = list_of_keywords)) +
  geom_point(shape = 21, colour = "black") +
  #geom_text()+
  theme(legend.position = "") +
  labs(title = "Keywords for the SBIR Data Set", x = "Number of Awards with Keyword", y = "Mean Award Amount", size = "Word Count", fill = "More in Phase 2")
```


```{r}
obtained_keywords %>%
  ggplot(aes(x = award_used_keyword, y = award_amount_total, size = word_count, fill = factor(more_phase_2), label = list_of_keywords)) +
  geom_point(shape = 21, colour = "black") +
  geom_text()+
  theme(legend.position = "") +
  labs(title = "Keywords for the SBIR Data Set", x = "Number of Awards with Keyword", y = "Total Award Amount", size = "Word Count", fill = "More in Phase 2")
```






# Libraries
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(plotly)
library(readr)
library(ggthemes)

#Set theme
theme_set(theme_gdocs())

# Read in data
unique_keywords <- read_csv("unique_keywords.csv")

#Create Bubble Plot
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

# Create Plotly Image
ggplotly(plot)


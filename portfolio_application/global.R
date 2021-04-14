################################################################################
# Load Packages
################################################################################

library(shiny)
library(wordcloud)
library(RWeka)
library(tm)
library(memoise)
library(readr)
library(dplyr)
library(stringr)
library(shiny)
library(ggplot2)
library(ggthemes)
library(ggdark)
library(tidytext)
library(tidyr)
library(gt)
library(shinythemes)
library(hrbrthemes)
library(gt)
library(ggcorrplot)
library(timevis)
library(readxl)
library(metafor)
library(metaviz)
library(gcookbook)
library(DT)
library(quanteda.dictionaries)
library(forcats)
library(plotly)

theme_set(theme_classic())

################################################################################
# Tab 1 - My Research
################################################################################

research_projects_df <- read_excel("data.xlsx")
research_projects_df$end = NA

################################################################################
# Tab 2 - Data Visualization
################################################################################

# Load Meta-Analysis Data
citation <- c(
  "Boysen & Vogel, 2007_1a", 
  "Boysen & Vogel, 2007_1b", 
  "Boysen G. A., 2008",
  "Lord, Ross, & Lepper, 1979", 
  "Lord, Preston, & Lepper, 1984_1", 
  "Lord, Preston, & Lepper, 1984_2", 
  "Munro, et al., 2002",
  "Munro & Ditto, 1997_1a", 
  "Munro & Ditto, 1997_1b", 
  "McHoskey, 1995_1a",
  "McHoskey, 1995_1b", 
  "Miller, McHoskey, Bane, & Dowd, 1993_1a", 
  "Miller, McHoskey, Bane, & Dowd, 1993_1b", 
  "Miller, McHoskey, Bane, & Dowd, 1993_2a",
  "Miller, McHoskey, Bane, & Dowd, 1993_2b",
  "Miller, McHoskey, Bane, & Dowd, 1993_3a",
  "Miller, McHoskey, Bane, & Dowd, 1993_3b",
  "Munro., 2010",
  "Greitemeyer, 2014",
  "Munro, Leary, & Lasane, 2004",
  "Plous, 1991_1a",
  "Plous, 1991_1b",
  "Plous, 1991_1c",
  "Cohen, Aronson, & Steele, 2000_1a",
  "Boysen, 2011_1a",
  "Boysen, 2011_1b"
)

g_biased_assimilation <- c(
  1.361949822, 0.844695601, 0.634646033, 1.624615736, 0.841614407, 
  1.360429782, 0.792117515, 0.549955212, 0.412369956, 1.422735135, 
  0.330913604, 0.851919293, 0.348555994, 1.757679814, 0.705318681,
  0.933574194, 0.411394834, 0.482905686, 0.934981486, 0.670144478, 
  1.421062595, 1.560173529, 0.716345679, 0.595595219, 0.340088312, 
  0.344528252
)

variance_of_g_biased_assimilation <- c(
  0.031135795, 0.033831190, 0.00336134, 0.053998301, 0.052526975, 
  0.059785292, 0.015373039, 0.050216163, 0.023788392, 0.216965772, 
  0.086941514, 0.067846476, 0.062675438, 0.074551227, 0.047665177, 
  0.111072300, 0.061612887, 0.052431807, 0.006936082, 0.047242365, 
  0.056638187, 0.056669732, 0.0081451, 0.062284015, 0.045378313, 
  0.038362871
)

g_polarization <- c(
  1.311966942, 0.836793822, 0.685070736, 1.235348911, 0.702843679, 
  0.592752749, 1.045099699, 0.374889008, 0.604668054, 1.133637618,
  0.400002508, 0.964885303, 0.422475248, 0.493242691, 0.334480000,
  0.735483871, -0.08900369, 0.547892784, 0.307050549, 0.553210226,
  1.328400000, 1.410400000, 1.140444444, 0.615967059, 0.380129693, 
  0.369348583)

variance_of_g_polarization <- c(
  0.057241992, 0.064968382, 0.006754429, 0.017765444, 0.051184334,
  0.050293927, 0.037690683, 0.003099098, 0.003979693, 0.212545175,
  0.086745122, 0.071251817, 0.061954710, 0.061534346, 0.045592778,
  0.106937140, 0.060439308, 0.052794388, 0.006385457, 0.023182498,
  0.110230299, 0.108386116, 0.016290200, 0.062502976, 0.045253300,
  0.038547705
)

extremity_level_of_sample <- c(
  1, 2, 3, 1, 1, 1, 3, 1, 3, 1, 2, 1, 2, 1, 2, 1, 2, 3, 3, 3, 1, 1, 1, 3, 3, 3
)

N <- c(
  92, 66, 196, 48, 40, 40, 70, 81, 174, 103, 106, 171, 166, 
  109, 115, 40, 69, 77, 666, 88, 43, 45, 63, 72, 89, 105
)

topic <- c(
  3, 3, 4, 1, 1, 1, 4, 3, 3, 4, 4, 1, 1, 1, 1, 4, 4, 3, 2, 3, 2, 2, 2, 1, 3, 4
)

sample_selection <- c(
  0, 0, 0, 1, 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0
)

is_composite_biased_assimilation <- c(
  1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 1, 1
)

is_composite_polarization <- c(
  0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1
)

d1 <- data.frame(
  citation, 
  g_biased_assimilation, 
  variance_of_g_biased_assimilation, 
  g_polarization, 
  variance_of_g_polarization, 
  topic, 
  sample_selection, 
  extremity_level_of_sample, 
  is_composite_biased_assimilation, 
  is_composite_polarization, 
  N
)

# Convert variables to factors
d1$topic <- factor(
  d1$topic, 
  levels = c(1, 2, 3, 4),
  labels = c("Capital Punishment", "Technology", "Homosexuality", "Other"))

d1$sample_selection <- factor(
  d1$sample_selection,
  levels = c(1, 0), 
  labels = c("Biased Selection", "Unbiased Selection")
)

d1$is_composite_biased_assimilation <- factor(
  d1$is_composite_biased_assimilation,
  levels = c(1, 0),
  labels = c("Composite (BA)", "Single (BA)")
)

d1$is_composite_polarization <- factor(
  d1$is_composite_polarization, 
  levels = c(1, 0), 
  labels = c("Composite (P)", "Single (P)")
)

d1$extremity_level_of_sample <- factor(
  d1$extremity_level_of_sample,
  levels = c(1, 2, 3), 
  labels = c("Extreme", "Moderate", "Mixed")
)

###############################################################################

# Create function that for creating forest plots
create_forest_plot <- function(df, effect = c("g_biased_assimilation", "g_polarization"), moderator = c("topic", "sample_selection", "is_composite", "extremity_level_of_sample")){
  
  # Depending on which variable is selected, select the appropriate composite and variance.
  data <- df
  if(effect == "g_biased_assimilation"){
    data$is_composite <- data[["is_composite_biased_assimilation"]]
    data$variance_of_g <- data[["variance_of_g_biased_assimilation"]]
  }
  
  if(effect == "g_polarization"){
    data$is_composite <- data[["is_composite_biased_assimilation"]]
    data$variance_of_g <- data[["variance_of_g_polarization"]]
  }
  
  # Order the data by the selected variable, and calculate the standard error and upper and lower ci
  data$citation_plot <- reorder(data[["citation"]], data[[effect]], FUN = mean)
  data$se = sqrt(data[["variance_of_g_biased_assimilation"]])
  data$lowerci = (-1.96 * data[["se"]]) + data[[effect]]
  data$upperci = (1.96 * data[["se"]]) + data[[effect]]
  
  # Create a label for the selected variable
  moderator_label <- str_to_title(str_replace_all(moderator, "_", "")) 
  
  plot <- ggplot(data, aes(y = .data[["citation_plot"]], x = .data[[effect]], xmin = lowerci, xmax = upperci, color = factor(.data[[moderator]])))+
    geom_point(shape = 16, size = 4) +
     #add the CI error bars
    geom_errorbarh(height = .1)+
    # Specify the limits of the x-axis and relabel it to something more meaningful
    scale_x_continuous(limits = c(-.5,2), name = 'Corrected Standardized Mean Difference (g)')+
    # Give y-axis a meaningful label
    labs(color = moderator_label, y = "Reference") +
    # Add a vertical dashed line indicating an effect size of zero, for reference
    geom_vline(xintercept = 0, color = 'red', linetype = 'dashed')+
    # Create sub-plots (i.e., facets) based on levels of setting
    # And allow them to have their own unique axes (so authors don't redundantly repeat)
    facet_grid(.data[[moderator]]~., scales =  'free', space = 'free') + 
    # Set the theme to go well with a black background.
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom")
  plot
}

# Create a function that visualizes the meta regression, and colors the data based on the selected moderator variable
create_meta_regression_plot <- function(data, moderator){
  data <- data
  if(moderator == "is_composite"){
    data$is_composite <- data[["is_composite_biased_assimilation"]]
  }
  
  ggplot(data, aes(x = g_biased_assimilation, y = g_polarization, color = .data[[moderator]])) +
    geom_point(aes(color = .data[[moderator]]), size = 4) +
    geom_smooth(method = lm, alpha = .2, color = 'white') +
    labs(title = "Biased Assimilation and Attitude Polarization", x = "Biased Assimilation", y = "Polarization", color = moderator) + 
    scale_y_continuous(limits=c(min(g_polarization), max(g_polarization))) +
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom")
  
}

################################################################################
# Tab 3 - Natural Language Processing
################################################################################

text_moore_lab <- read_excel("text_moore_lab.xlsx")

# Create a function to select the first text
which_text1 <- function(text1, selected_text1){
  if(text1 != ""){
    first_text <- text1
  } else {
    first_text <- text_moore_lab[[selected_text1]]
  }
  first_text
}

# Create a function to select the second text
which_text2 <- function(text2, selected_text2){
  if(text2 != ""){
    second_text <- text2
  } else {
    second_text <- text_moore_lab[[selected_text2]]
  }
  second_text
}

# Clean the text
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en")))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}

# Create DTM
create_dtm_m <- function(text1, text2, sparse = .999, ngrams = 1){
  # Combine text
  text <- c(text1, text2)
  # Create a function that will tokenize ngrams
  tokenizer <- function(x){
    NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
  }
  vector_source <- VectorSource(text)
  corpus <- VCorpus(vector_source)
  clean_corp <- clean_corpus(corpus)
  if(ngrams > 1){
    dtm <- DocumentTermMatrix(clean_corp, control = list(tokenize = tokenizer))
  } else {
    dtm <- DocumentTermMatrix(clean_corp)
  }
  dtm_rm_sparse <- removeSparseTerms(dtm, sparse = sparse)
  dtm_m <- as.matrix(dtm_rm_sparse)
  dtm_m
}

# Create Function to make a word plot from a dtm
plot_words <- function(dtm_m, color = "black", title = "Word Plot With Selected Text"){
  freq <- colSums(dtm_m)
  term_frequency <- sort(freq, decreasing = TRUE)
  words <- as.factor(names(term_frequency[1:20]))
  n = as.integer(term_frequency[1:20])
  words_tibble <- tibble(words, n)
  word_counts <- words_tibble %>%
    mutate(words2 = fct_reorder(words, n))
  # Frequency of words plot
  ggplot(word_counts, aes(x = words2, y = n)) +
    geom_col(show.legend = FALSE, fill = color) +
    coord_flip() +
    labs(title = title, x = "Words", y = "Count") +    
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom")
}

# Create Commonality Plot
commonality_comparison_plots <- function(text1, name_a, text2, name_b, type = "Commonality"){
  all_text <- c(text1, text2)
  all_text_v <- VectorSource(all_text)
  all_corpus <- VCorpus(all_text_v)
  all_clean <- clean_corpus(all_corpus)
  all_tdm <- TermDocumentMatrix(all_clean)
  par(bg="grey10")
  if(type == "Commonality"){
    all_m <- as.matrix(all_tdm)
    commonality.cloud(all_m, max.words = 100, colors = "steelblue1")
  } else {
    colnames(all_tdm)  <- c(name_a, name_b)
    all_m <- as.matrix(all_tdm)
    comparison.cloud(all_m, colors = c("orange", "light blue"), max.words = 50, title.colors = c("white", "grey"), title.bg.colors = "black")
  }
}

# Function to Extract Sentiment data
extract_data_from_both_texts <- function(text1, text2){
  text <- c(text1, text2)
  nrc_data <- tibble(liwcalike(text, dictionary = data_dictionary_NRC)) %>% 
    select(docname, words_per_sentence = WPS, word_count = WC, sixltr= Sixltr, 
           dic = Dic, all_punc = AllPunc, period = Period, comma = Comma, 
           colon = Colon, semiC = SemiC, qmark = QMark, exclam = Exclam, 
           dash = Dash, quote = Quote, apostro = Apostro, parenth = Parenth, 
           other_punctuation = OtherP, nrc_anger = anger, 
           nrc_anticipation = anticipation, nrc_disgust = disgust, 
           nrc_fear = fear, nrc_joy = joy, nrc_negative = negative,
           nrc_positive = positive, nrc_sadness = sadness,
           nrc_surprise = surprise, nrc_trust = trust)
  
  mfd_data <- tibble(liwcalike(text, dictionary = data_dictionary_MFD)) %>%
    mutate(care = care.virtue, fairness = fairness.virtue, loyalty = loyalty.virtue, authority = authority.virtue, sanctity = sanctity.virtue) %>%
    select(docname, care, fairness, loyalty, authority, sanctity)
  
  afinn_data <- tibble(liwcalike(text, dictionary = data_dictionary_AFINN)) %>%
    select(docname, afinn_negative = negative, afinn_positive = positive)
  
  output <- nrc_data %>%
    inner_join(mfd_data, on = "docname") %>%
    inner_join(afinn_data, on = "docname")
  
  output <- output %>% 
    mutate(mfd_total = care + fairness + loyalty + authority + sanctity, polarity = afinn_positive - afinn_negative)
  
  output
}


# Create function to extract keywords
get_keywords <- function(first_text, second_text){
  tf_idf_text <- data.frame("text_name" = c("Text 1", "Text 2"), "text" = c(first_text, second_text))
  word_counts <- tf_idf_text %>%
    group_by(text_name) %>%
    unnest_tokens(word, text) %>%
    count(text_name, word, sort = TRUE) %>%
    group_by(text_name) %>%
    bind_tf_idf(word, text_name, n) %>%
    slice_max(order_by = tf_idf, n = 5)
  word_counts <- word_counts %>%
    mutate(words2 = fct_reorder(word, n))
  word_counts %>%
    ggplot(aes(x = words2, y = n, fill = text_name)) +
    geom_col() +
    coord_flip() +
    labs(title = "", x = "Words", y = "Count", fill = "Text Name") +    
    dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
    theme(plot.title = element_text(family = "Fira Sans Condensed"),
          plot.background = element_rect(fill = "grey10"),
          panel.background = element_blank(),
          panel.grid.major = element_line(color = "grey30", size = 0.2),
          panel.grid.minor = element_line(color = "grey30", size = 0.2),
          legend.background = element_blank(),
          axis.ticks = element_blank(),
          legend.key = element_blank(),
          legend.position = "bottom")
}

################################################################################
# Tab 4 - Data Collection, Data Scraping, and Psychometrics
################################################################################









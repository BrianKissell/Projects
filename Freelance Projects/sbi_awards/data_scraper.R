# Load packages
library(tidyverse)
library(rvest)
library(stringr)
library(tidyr)

# Import the links from the csv file I have stored on my google drive.
award_links <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBHEwhc-nPaTE7Kt9LiTJTev2_-Z0Mgw3Voi2XfFyS8EnHDtmMVynT7mb_VqUNO7wEbl21hq-N0seo/pub?output=csv")

# Initiate the data frame
df_scraped_data <- tibble(
  agency = c(""),
  branch = c(""),
  contract = c(""),
  agency_tracking_number = c(""),
  amount = c(""),
  phase = c(""),
  program = c(""),
  solicitation_topic_code = c(""),
  solicitation_number = c(""),
  solicitation_year = c(""),
  award_year = c(""),
  award_start_date_proposal_award_date = c(""),
  award_end_date_contract_end_date = c(""),
  duns = c(""),
  hubzone_owned = c(""),
  woman_owned = c(""),
  socially_and_economically_disadvantaged = c(""),
  link = c(""),
  recipient_information = c(""),
  abstract = c("")
) 

# Initiate a count, just so I can see where it is in the process.
count = 0

# Create the list of links that we will iterate over.
award_links_full <- award_links$award_links_full
award_links_full <- sample(award_links_full, 100)

# Extract the data for every award.
for(award_link in award_links_full){
  
  # Add in a pause to avoid using too many resources
  Sys.sleep(2)
  
  # Create html
  award_link_html <- read_html(award_link)
  
  # Collect and clean up the labels, so that they can be used as variable names
  labels <- award_link_html %>% html_nodes('.open-label') %>% html_text() %>% 
    str_replace(pattern = ":", "") %>%
    str_replace_all(pattern = '[(]', "") %>%
    str_replace_all(pattern = '[)]', "") %>%
    str_replace_all(pattern = " ", "_") %>%
    tolower()
  
  # Collect the values that will go with each label.
  values <- award_link_html %>% html_nodes('.open-description') %>% html_text()
  
  # Create a dataframe, which will be used in the process.
  labels_and_values <- tibble(labels, values)
  
  # Turn the labels variable into the column names
  df_data <- pivot_wider(data = labels_and_values, names_from = labels, values_from = values)
  
  # Add the link to the dataframe
  df_data$link <- paste(award_link)
  
  # Collect and add the recepient information (This will need to be cleaned up, if we want to use it)
  df_data$recipient_information <- award_link_html %>% html_nodes('.col-md-4 div') %>% html_text() %>% str_c(collapse = TRUE)
  
  # Collect and add the abstract
  df_data$abstract <- award_link_html %>% html_nodes('.abstract-wrapper') %>% html_text() %>% str_c(collapse = TRUE) %>% str_trim()
  
  # Add the data to the initiated dataframe
  df_scraped_data = add_row(df_scraped_data, df_data)
  
  # Print and change the count, so I know where we are in the process
  print(count)
  count = count + 1
}
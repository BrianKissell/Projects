# Load packages
library(tidyverse)
library(rvest)
library(stringr)
library(tidyr)
library(lubridate)

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

# Create function to collect data labels from a certain css selector, and then clean them so that they can be used as variable names.
create_scraped_labels <- function(css_selector){
  award_link_html %>% 
    html_nodes(css_selector) %>% 
    html_text() %>% 
    str_replace(pattern = ":", "") %>%
    str_replace_all(pattern = '[(]', "") %>%
    str_replace_all(pattern = '[)]', "") %>%
    str_replace_all(pattern = " ", "_") %>%
    tolower()
}

# Create function to collect the values from a css_selector that will go with each scraped label.
create_scraped_values <- function(css_selector){
  award_link_html %>% 
    html_nodes(css_selector) %>% 
    html_text()
}

# Create a function that takes a css_selector for scraped labels and a css_selector for scraped values. Then combine the labels and values in a data frame.
create_labels_values_dataframe <- function(labels_css_selector, values_css_selector){
  
  # Collect and clean up the labels, so that they can be used as variable names
  labels <- create_scraped_labels(labels_css_selector)
  
  # Collect the values that will go with each label.
  values <- create_scraped_values(values_css_selector)
  
  # Create a dataframe, which will be used in the process.
  labels_and_values <- tibble(labels, values) 
  
  # Turn the labels variable into the column names
  pivot_wider(data = labels_and_values, names_from = labels, values_from = values)
}

# Create function for reading html. It is written so that it will retry if the connection fails.
foo <- NULL
read_html_try <- function(x,tries) {
  message(paste("x is",x,"remaining tries",tries))
  withRestarts(
    tryCatch({
      read_html(x)
    },
    error=function(e) { invokeRestart("retry")}),
    retry = function() { 
      message("Retrying")
      Sys.sleep(10)
      stopifnot(tries > 0)
      read_html_try(x,tries-1)
    }
  )
}

######
# Initiate a count, just so I can see where it is in the process.
count = 0

# Create the list of links that we will iterate over.
award_links_full <- award_links$award_links_full
#award_links_full <- award_links$award_links_full[c(80339:85191)]


# Extract the data for every award.
for(award_link in award_links_full){
  
  # Add in a pause to avoid using too many resources
  Sys.sleep(1)
  
  # Create html
  #award_link_html <- read_html(award_link)
  award_link_html <- read_html_try(award_link,10)
  
  # Collect and clean up the labels, so that they can be used as variable names
  # Collect the values that will go with each label.
  # Create a dataframe, which will be used in the process.
  # Turn the labels variable into the column names
  df_data <- create_labels_values_dataframe(labels_css_selector = '.open-label', values_css_selector = '.open-description')
  
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

write_csv(df_scraped_data, "df_scraped_data.csv")

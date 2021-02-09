## Scraper to obtain links for every award from www.sbir.gov for the Department of Defense.
# Load packages
library(tidyverse)
library(rvest)
library(stringr)

# Create html from sbir
html <- read_html("https://www.sbir.gov/sbirsearch/award/all/?f%5B0%5D=im_field_agencies%3A105755&f%5B1%5D=im_field_agencies%3A105729")

# Obtain the number of search results
n_search_results <- html %>% html_nodes('.col-sm-5') %>% 
  html_text() %>% 
  str_extract("[0-9]{3,10}") %>%
  as.integer()

# Number of pages that we will need to scroll through.
n_pages_of_results <- ceiling(n_search_results / 10)-1

# Initiate a vector that will store the pages.
links_search_pages <- c()

# Create a link for every search page.
for (page_number in seq(0, n_pages_of_results)){
  
  link_search_page <- paste("https://www.sbir.gov/sbirsearch/award/all/?page=",page_number,"&f%5B0%5D=im_field_agencies%3A105755&f%5B1%5D=im_field_agencies%3A105729", sep = "")
  
  # Store each new link in the initialized vector. 
  links_search_pages <- c(links_search_pages, link_search_page)
}

#Check out structure of the links_search_pages
str(links_search_pages)

############# Collect the name and link for every award. ####################

# Initialize links and names of links.
names_of_award_page <- c("")
links_of_award_page <- c("")

# Initialize a count, so I can see where we are in the process.
count = 0

# Extract the name and links for every award.
for(link in links_search_pages){
  
  # Add in a pause to avoid using too many resources.
  Sys.sleep(.5)
  
  # Create html
  link_html <- read_html(link)
  
  # Grab name of award
  name_of_award_page <- link_html %>% html_nodes('.title a') %>% html_text()
  
  # Grab link for the award
  link_of_award_page <- link_html %>% html_nodes('.title a') %>% html_attr('href')
  
  # Add name to initialized vector
  names_of_award_page <- c(names_of_award_page, name_of_award_page)
  
  # Add link to initialized vector
  links_of_award_page <- c(links_of_award_page, link_of_award_page)
  
  # Print a count, so I can see where we are in the process.
  count = count + 1
  print(count)
}

# Construct a data frame with all names and links.
links_df <- tibble(
  award_links = links_of_award_page,
  award_names = names_of_award_page
)

# Create a variable with the entire url.
links_df$award_links_full <- c("")
links_df$award_links_full <- str_c("https://www.sbir.gov", links_df$award_links)

# Create a csv
write_csv(links_df, "award_links.csv")
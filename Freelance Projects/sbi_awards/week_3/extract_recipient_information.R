# Install Packages
library(tidyverse)
library(naniar)
library(lubridate)

# Read in full data set, which I will shape into different tables.
df <- read_csv("df_processed.csv")

# Create the award page number from the links, which will be the unique identifier
df$award_page_number <- str_extract(df$link, "\\d+")

# Remove all commas
df$recipient_information <- str_replace_all(sbir_text$recipient_information, ",", "")

# Create a new dataframe with the page number (the rest can be added)
sbir_recipient_information <- df %>% select(award_page_number)

# Since this string is a mess, I will have to work through it one step at a time.
# First remove the leading text
string1 <- str_replace(df$recipient_information, "Principal InvestigatorTRUE\n\\s+", "")
string1 <- str_replace(string1, "Name\\:\\s", "")

# Extract the name, and then clean it up
name <- str_extract(string1, ".+Title\\:\\s")
sbir_recipient_information$name <- str_remove(name, "Title\\:\\s")
sbir_recipient_information$name <- tidyr::replace_na(sbir_recipient_information$name, "Missing")

# Remove the above section
string2 <- str_replace(string1, ".+Title\\:\\s", "")

# Extract the title, and then clean it up
title <- str_extract(string2, ".+Phone\\:\\s")
sbir_recipient_information$title <- str_remove(title, "Phone\\:\\s")
sbir_recipient_information$title <- tidyr::replace_na(sbir_recipient_information$title, "Missing")

# Remove the above section
string3 <- str_replace(string2, ".+Phone\\:\\s", "")

# Extract the phone number, and then clean it up
phone <- str_extract(string3, ".+Email\\:\\s")
sbir_recipient_information$phone <- str_remove(phone, "Email\\:\\s")
sbir_recipient_information$phone <- tidyr::replace_na(sbir_recipient_information$phone, "Missing")

# Remove the above section
string4 <- str_replace(string3, ".+Email\\:\\s", "")

# Extract the email, and then clean it up
email <- str_match(string4, "^.+TRUE")
sbir_recipient_information$email <- str_remove(email, "TRUE.+")
sbir_recipient_information$email <- tidyr::replace_na(sbir_recipient_information$email, "Missing")

# Remove the above section
string5 <- str_replace(string4, ".+\\\n\\s+Name\\:\\s", "")

# Extract the business_contact_name, and then clean it up
business_contact_name <- str_extract(string5, ".+Phone\\:\\s")
sbir_recipient_information$business_contact_name <- str_remove(business_contact_name, "Phone\\:\\s")
sbir_recipient_information$business_contact_name <- tidyr::replace_na(sbir_recipient_information$business_contact_name, "Missing")

# Remove the above section
string6 <- str_replace(string5, ".+Phone\\:\\s", "")

# Extract the business_contact_phone, and then clean it up
business_contact_phone <- str_extract(string6, ".+Email\\:")
sbir_recipient_information$business_contact_phone <- str_remove(business_contact_phone, "Email\\:")
sbir_recipient_information$business_contact_phone <- tidyr::replace_na(sbir_recipient_information$business_contact_phone, "Missing")

# Remove the above section
string7 <- str_replace(string6, ".+Email\\:\\&nbsp", "")

# Extract the business_contact_email, and then clean it up
business_contact_email <- str_match(string7, "^.+TRUE")
sbir_recipient_information$business_contact_email <- str_remove(business_contact_email, "TRUE.+")
sbir_recipient_information$business_contact_email <- tidyr::replace_na(sbir_recipient_information$business_contact_email, "Missing")

# Remove the above section
string8 <- str_replace(string7, ".+TRUE", "")
string9 <- str_replace(string8, "\\\n\\s+\\\n\\s+Name\\:\\s", "")

# Extract the research_institution , and then clean it up
research_institution <- str_match(string9, "^.+Contact\\:\\s")
sbir_recipient_information$research_institution <- str_remove(research_institution, "Contact\\:\\s")
sbir_recipient_information$research_institution <- tidyr::replace_na(sbir_recipient_information$research_institution, "Missing")

# Remove the above section
string10 <- str_replace(string9, "^.+Contact\\:\\s", "")

# Extract the research_institution_name, and then clean it up
research_institution_name <- str_extract(string10, ".+Address\\:\\s")
sbir_recipient_information$research_institution_name <- str_remove(research_institution_name, "Address\\:\\s")
sbir_recipient_information$research_institution_name <- tidyr::replace_na(sbir_recipient_information$research_institution_name, "Missing")

# Remove the above section
string11 <- str_replace(string10, ".+Address\\:\\s", "")

# Extract the research_institution_address, and then clean it up
research_institution_address <- str_extract(string11, ".+Phone\\:\\s")
sbir_recipient_information$research_institution_address <- str_remove(research_institution_address, "Phone\\:\\s")
sbir_recipient_information$research_institution_address <- tidyr::replace_na(sbir_recipient_information$research_institution_address, "Missing")

# Extract Zip code
sbir_recipient_information$research_institution_zip_code <- str_extract(sbir_recipient_information$research_institution_address, "\\d+$")
sbir_recipient_information$research_institution_zip_code <- tidyr::replace_na(sbir_recipient_information$research_institution_zip_code, "Missing")

# Remove the above section
string12 <- str_replace(string11, ".+Phone\\:\\s", "")

# Extract the research_institution_phone, and then clean it up
research_institution_phone <- str_extract(string12, ".+Type\\:\\s")
sbir_recipient_information$research_institution_phone <- str_remove(research_institution_phone, "Type\\:\\s")
sbir_recipient_information$research_institution_phone <- tidyr::replace_na(sbir_recipient_information$research_institution_phone, "Missing")

# Remove the above section
string13 <- str_replace(string12, ".+Type\\:\\s", "")

# Extract the research_institution_type, and then clean it up
research_institution_type <- str_extract(string13, ".+\\\n.+\\\n.+Name\\:\\s")
sbir_recipient_information$research_institution_type <- str_remove(research_institution_type, "\\\n.+Name\\:\\s")
sbir_recipient_information$research_institution_type <- tidyr::replace_na(sbir_recipient_information$research_institution_type, "Missing")

# Remove the above section
string14 <- str_replace(string13, ".+\\\n.+\\\n.+Name\\:\\s", "")

# Extract the research_institution_2, and then clean it up
research_institution_2 <- str_match(string14, "^.+Contact\\:\\s")
sbir_recipient_information$research_institution_2 <- str_remove(research_institution_2, "Contact\\:\\s")
sbir_recipient_information$research_institution_2 <- tidyr::replace_na(sbir_recipient_information$research_institution_2, "Missing")

# Remove the above section
string15 <- str_replace(string14, "^.+Contact\\:\\s", "")

# Extract the research_institution_name_2, and then clean it up
research_institution_name_2 <- str_extract(string15, ".+Address\\:\\s")
sbir_recipient_information$research_institution_name_2 <- str_remove(research_institution_name_2, "Address\\:\\s")
sbir_recipient_information$research_institution_name_2 <- tidyr::replace_na(sbir_recipient_information$research_institution_name_2, "Missing")

# Remove the above section
string16 <- str_replace(string15, ".+Address\\:\\s", "")

# Extract the research_institution_address_2, and then clean it up
research_institution_address_2 <- str_extract(string16, ".+Phone\\:\\s")
sbir_recipient_information$research_institution_address_2 <- str_remove(research_institution_address_2, "Phone\\:\\s")
sbir_recipient_information$research_institution_address_2<- tidyr::replace_na(sbir_recipient_information$research_institution_address_2, "Missing")

# Extract Zip Code
sbir_recipient_information$research_institution_zip_code_2 <- str_extract(sbir_recipient_information$research_institution_address_2, "\\d+$")
sbir_recipient_information$research_institution_zip_code_2 <- tidyr::replace_na(sbir_recipient_information$research_institution_zip_code_2, "Missing")

# Remove the above section
string17 <- str_replace(string16, ".+Phone\\:\\s", "")

# Extract the research_institution_phone_2, and then clean it up
research_institution_phone_2 <- str_extract(string17, ".+Type\\:\\s")
sbir_recipient_information$research_institution_phone_2 <- str_remove(research_institution_phone_2, "Type\\:\\s")
sbir_recipient_information$research_institution_phone_2 <- tidyr::replace_na(sbir_recipient_information$research_institution_phone_2, "Missing")

# Remove the above section
string18 <- str_replace(string17, ".+Type\\:\\s", "")
head(string18)

# Extract the research_institution_type_2, and then clean it up
research_institution_type_2 <- str_extract(string18, ".+")
sbir_recipient_information$research_institution_type_2 <- str_replace(string18, "\\\n.+\\\n.+\\\n.+\\\n.+", "Missing")

# Check dataframe
glimpse(sbir_recipient_information)

# Write the table for recipient information
write_csv(sbir_recipient_information, "sbir_recipient_information.csv")



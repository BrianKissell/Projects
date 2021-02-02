# Import packages
import pandas as pd
import re

# Import csv file directly from fivethirtyeight.
df_ = pd.read_csv("https://projects.fivethirtyeight.com/congress-tracker-data/csv/averages.csv")

# Only select members of the 116th congress.
trump_approval_116 = df_[df_['congress'] == 116]

# Import the twitter handles for 116th congress.
congress_twitter = pd.read_csv("congress_twitter_handles.csv")

# Clean up the names and create a variable that contains the last name.
congress_twitter['name'] = congress_twitter['name'].str.strip()
congress_twitter['name'] = [re.sub("\s?j|Jr\.?", "", name) for name in congress_twitter['name']]
congress_twitter['name'] = [re.sub("\s[A-Z]\.\s", " ", name) for name in congress_twitter['name']]
congress_twitter['last_name'] = [name.split()[-1] for name in congress_twitter['name']]

# Add the twitter handles to the trump approval data set.
joined = pd.merge(trump_approval_116, congress_twitter, on = "last_name")

# Create the csv file
joined.to_csv("congress_twitter_trump_approval.csv")
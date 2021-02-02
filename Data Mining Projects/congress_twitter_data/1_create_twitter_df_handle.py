# Import packages
from urllib.request import urlopen
from io import open
import re
import pandas as pd

# Read the text file that contains the twitter handles for each congress member.
file = open('116th-Congress-Twitter-Handles.txt', encoding="utf-8")
text = file.read()

# Remove commas from the text.
text = re.sub(r"\,", "", text)

# Use regular expression to extract the name, state, and twitter handle for each congress person.
congress_data = re.findall(r'([A-Z]{2}\s?\-?\s?(?:[0-9]+)?)\s?(?:At Large )?(?:Delegate\s)?((?:[A-z\.\-\"\'\,úíéáÃº­©¡]+\s){1,5}|(?:N\/A))((?:@[A-z\.\-0-9]+)|N\/A)', text)

# Use this information to create a dataframe.
df_congress = pd.DataFrame(congress_data, columns = ["state","name","twitter_handle"])  

# Save the dataframe as a csv file.
df_congress.to_csv("congress_twitter_handles.csv")
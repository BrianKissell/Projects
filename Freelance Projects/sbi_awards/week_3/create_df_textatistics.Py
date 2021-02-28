# Import packages
import pandas as pd
from textatistic import Textatistic
import sys

# Read in data
df1 = pd.read_csv("prepare_for_modeling_with_text_counts.csv") 

# Define function to extraxt textatistics information
def extract_textatistics(index = 0):
  textatistics= Textatistic(df1.text.iloc[index])
  sent_count = textatistics.sent_count
  char_count = textatistics.char_count
  notdalechall_count = textatistics.notdalechall_count
  sybl_count = textatistics.sybl_count
  word_count = textatistics.word_count
  dalechall_score = textatistics.dalechall_score
  flesch_score = textatistics.flesch_score
  fleschkincaid_score = textatistics.fleschkincaid_score
  gunningfog_score = textatistics.gunningfog_score
  smog_score = textatistics.smog_score
  
  # Initiate new variables from the textatistic package  
  df_textatistic = pd.DataFrame({'sent_counts':sent_count, 'char_counts':char_count, 'notdalechall_counts':notdalechall_count, 'sybl_counts':sybl_count, 'word_counts':word_count, 'dalechall_scores':dalechall_score, 'flesch_scores':flesch_score, 'fleschkincaid_scores':fleschkincaid_score, 'gunningfog_scores':gunningfog_score, 'smog_scores':smog_score}, index=[index])
  return df_textatistic

# Initiate dataframe by running the first one, and then loop through the rest
df_textatistics = extract_textatistics(0)
for index in list(range(1, len(df1.text)-1)):
  try:
    df_textatistics = df_textatistics.append(extract_textatistics(index))
  except ZeroDivisionError:
    print("Cannot divide by zero")
    print(index)

# Create an index
df_textatistics['index'] = df_textatistics.index

# Write the csv file
df_textatistics.to_csv("df_textatistics.csv")

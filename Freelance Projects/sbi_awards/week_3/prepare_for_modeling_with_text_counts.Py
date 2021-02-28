#Import packages
import pandas as pd

df = pd.read_csv("df_processed.csv") 
df.columns
print(df.info())

# Dummy Code the categorical variables
df1 = pd.get_dummies(df, columns=['branch', 'phases', 'program', 'hubzone_owned', 'woman_owned', 'ses_disadvantage'])

# Create count of the number of characters in each title
df1['title_char_count'] = df1['award_names'].apply(len)

# Print the average character count
print(df1['title_char_count'].mean())

# Function that returns number of words in a string
def count_words(string):
	# Split the string into words
    words = string.split()
    # Return the number of words
    return len(words)

# Create a word_count for each title
df1['title_word_count'] = df1['award_names'].apply(count_words)

# Print the average word count of the talks
print(df1['title_word_count'].mean())

# Create count of the number of characters in all text for each award
df1['text_char_count'] = df1['text'].apply(len)

# Print the average character count of all words
print(df1['text_char_count'].mean())

# Create a word_count for all words in the text
df1['text_word_count'] = df1['text'].apply(count_words)

# Print the average word count of the text for each award
print(df1['text_word_count'].mean())

# Add the index
df1['index'] = list(range(0, len(df1.text)))

# Write the csv file
df1.to_csv("prepare_for_modeling_with_text_counts.csv")

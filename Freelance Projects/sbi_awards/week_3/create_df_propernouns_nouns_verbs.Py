# Import packages
from spacy.lang.en.stop_words import STOP_WORDS as stopwords
import spacy
import pandas as pd

# Read in data
df_spacy = pd.read_csv("1_df_text_and_phases.csv")

# Load the en_core_web_sm model
nlp = spacy.load('en_core_web_sm')

###########################################
# Function to preprocess text
def preprocess(text):
  	# Create Doc object
    doc = nlp(text, disable=['ner', 'parser'])
    # Generate lemmas
    lemmas = [token.lemma_ for token in doc]
    # Remove stopwords and non-alphabetic characters
    a_lemmas = [lemma for lemma in lemmas if lemma.isalpha() and lemma not in stopwords]
    return ' '.join(a_lemmas)

#############################################
# Returns number of proper nouns
def proper_nouns(text, model=nlp):
  	# Create doc object
    doc = model(text)
    # Generate list of POS tags
    pos = [token.pos_ for token in doc]
    # Return number of proper nouns
    return pos.count('PROPN')

#############################################
# Returns number of other nouns
def nouns(text, model=nlp):
  	# Create doc object
    doc = model(text)
    # Generate list of POS tags
    pos = [token.pos_ for token in doc]
    # Return number of other nouns
    return pos.count('NOUN')

#############################################
# Returns number of other verbs
def verbs(text, model=nlp):
  	# Create doc object
    doc = model(text)
    # Generate list of POS tags
    pos = [token.pos_ for token in doc]
    # Return number of verbs
    return pos.count('VERB')

#############################################

# Apply preprocess to df_spacy['text']
df_spacy['text'] = df_spacy.text.apply(preprocess)

# Create a new csv with only text and phases
df_spacy_only_text_and_phases = df_spacy[["award_page_number", "phases", "text"]]
df_spacy_only_text_and_phases.to_csv("df_spacy_only_text_and_phases.csv")

# Extract number of proper nouns
df_spacy['num_propn'] = df_spacy['text'].apply(proper_nouns)

# Extract number of nouns
df_spacy['num_noun'] = df_spacy['text'].apply(nouns)

# Extract number of verbs
df_spacy['num_verb'] = df_spacy['text'].apply(verbs)

# Compute mean of proper nouns
no_phase_2_propn_mean = df_spacy[df_spacy['phases'] == 'Phase I - No Phase II']['num_propn'].mean()
got_phase_2_propn_mean = df_spacy[df_spacy['phases'] == 'Phase I - Obtained Phase II']['num_propn'].mean()

# Print results
print("Mean no. of proper nouns in awards with no phase 2 and a phase 2 are %.2f and %.2f respectively"%(no_phase_2_propn_mean, got_phase_2_propn_mean))

# Compute mean of nouns
no_phase_2_noun_mean = df_spacy[df_spacy['phases'] == 'Phase I - No Phase II']['num_noun'].mean()
got_phase_2_noun_mean = df_spacy[df_spacy['phases'] == 'Phase I - Obtained Phase II']['num_noun'].mean()

# Print results
print("Mean no. of normal nouns in awards with no phase 2 and a phase 2 are %.2f and %.2f respectively"%(no_phase_2_noun_mean, got_phase_2_noun_mean))

# Compute mean of verbs
no_phase_2_verb_mean = df_spacy[df_spacy['phases'] == 'Phase I - No Phase II']['num_verb'].mean()
got_phase_2_verb_mean = df_spacy[df_spacy['phases'] == 'Phase I - Obtained Phase II']['num_verb'].mean()

# Print results
print("Mean no. of normal nouns in awards with no phase 2 and a phase 2 are %.2f and %.2f respectively"%(no_phase_2_verb_mean, got_phase_2_verb_mean))

# Write out a csv file with these new variables
df_spacy.to_csv("df_propernouns_nouns_verbs.csv")

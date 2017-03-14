# A test of universals in wh words


The full dataset contains 430,000 entries from 314 languages, taken from the IDS, the WOLD and Sprakbanken. 

https://github.com/seannyD/UniversalsInWHWords/blob/master/Processing/CleanedAndSimplifiedData/Alldata_simple.csv

I've also written a script that restricts the data to non-creole, non-reconstructed, non-dialect-level distinctions, and only concepts that are well covered.  e.g. someone went crazy over Nakh-Daghestanian languages and documented 10 varieties of the same language, which is quite unbalanced considering the rest of the coverage.  This dataset is 1000 concepts in 230 languages:

https://github.com/seannyD/UniversalsInWHWords/blob/master/Analysis/RestrictionsApplied.R

The following script will produce a matrix where rows are meanings and columns are languages:

```
#(in Analysis/)

source("PermutationTools.r")

alldata<-read.csv("../Processing/CleanedAndSimplifiedData/Alldata_simple.csv", stringsAsFactors=F)

d = data.frame.to.matrix(alldata)
```

#  Data format

word: Original transcription.  Note that there can be multiple words per entry, separated by ";"

word.clean: original transcription with characters normalised

word.simple: simplified orthography, paying attention only to place and manner of articulation (no tones, no vowel length, no aspiration/nasality/palatalisation).  We were mainly interested in consonants, so vowels are very simplified.

meaning: the original meaning (can differ for the same meaning ID)

meaning.id: the original meaning ID (see WOLD/IDS)

meaning.id.fixed: we fixed and normalised some meaning IDs.

domain: meaning domain.

analyzability: For WOLD data, whether the word can be analysed into sub-parts

Source: source of the data.  Some languages were covered in more than one database, we prioritised WOLD since it has analysability data.


# Workflow for producing the data:

## Process raw WOLD data:

Collect_new_WOLD_data.R
addTranscriptions_new.R

Collect_new_IDS_data.R

Collect_Spraakbanken_data.R

## Merge WOLD, IDS AND SB
List_merge.R

## Simplify data
simplifyData.R    -->> Processing/CleanedAndSimplifiedData/Alldata_simple.csv



## End up with these files:

Alldata_simple.csv
RAW_data/Data_clean_up2.csv (manually created)
RAW_data/Grammars.csv (manually created)

## Get languages in analysis by running:
Analysis/RestrictionsApplied.R

## Then 
Processing/addGeoDataToLangList.R


# Analysis

# Select data:
Analysis/RestrictionsApplied.R
grammars.R
makeDataVariables.R

# (these three are included in most RunAnalysis* files)

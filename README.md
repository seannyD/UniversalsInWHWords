#Process raw WOLD data:

extractWOLDdata2.r
addTranscriptions.r
Collect_WOLD_data.R

Collect_IDS_data.R

Collect_Spraakbanken_data.R

# Merge WOLD, IDS AND SB
List_merge.R

# Simplify data
simplifyData.R    -->> Processing/CleanedAndSimplifiedData/Alldata_simple.csv



# End up with these files:

Alldata_simple.csv
RAW_data/Data_clean_up2.csv (manually created)
RAW_data/Grammars.csv (manually created)

####
# Analysis

#Select data:
Analysis/RestrictionsApplied.R
grammars.R
makeDataVariables.R

# (these three are included in most RunAnalysis/* files)

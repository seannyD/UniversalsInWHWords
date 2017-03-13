#Process raw WOLD data:

Collect_new_WOLD_data.R
addTranscriptions_new.R

Collect_new_IDS_data.R

Collect_Spraakbanken_data.R

# Merge WOLD, IDS AND SB
List_merge.R

# Simplify data
simplifyData.R    -->> Processing/CleanedAndSimplifiedData/Alldata_simple.csv



# End up with these files:

Alldata_simple.csv
RAW_data/Data_clean_up2.csv (manually created)
RAW_data/Grammars.csv (manually created)

# Get languages in analysis by running:
Analysis/RestrictionsApplied.R
# Then 
Processing/addGeoDataToLangList.R


####
# Analysis

#Select data:
Analysis/RestrictionsApplied.R
grammars.R
makeDataVariables.R

# (these three are included in most RunAnalysis* files)

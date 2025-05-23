All data and code are part of the 2025 manuscript to compare fecal glucocorticoid levels and metabolism across mammal species.

The code is partitioned such that the script CleanAndTree uses the open tree of life and various commands to clean raw data, this script is dependant on an OTL API key. 
If you do not want to recreate the tree and cleaned data, a tree and clean data set is in each hormone folder for use in other scripts. 
MasterScript runs various hormone configurtations through WorkingScript, PhyloSigScript, and AICScript, as if those were functions. 
PubFiguresScript uses much of the output from WorkingScript to select and order figured destined for the publication. 

There are no raw data in the FGCAnalysis folder because those data are made by combining the cortisol and corticosterone clean data based on dominance. 
Haase data do not belong to us, those are data from Haase et al. 2016 


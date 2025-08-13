All data and code are part of the 2025 manuscript to compare fecal glucocorticoid levels and metabolism across mammal species.

The code is partitioned such that the script CleanAndTree uses the open tree of life and various commands to clean raw data, this script is dependent on an OTL API key. 
If you do not want to recreate the tree and cleaned data, a tree and clean data set is in each hormone folder for use in other scripts. 
MasterScript runs various hormone configurtations through WorkingScript, PhyloSigScript, and AICScript, as if those were functions. 
PubFiguresScript uses much of the output from WorkingScript to select and order figured destined for the publication. 

There are no raw data in the FGCAnalysis folder because those data are made by combining the cortisol and corticosterone clean data based on dominance. 

Haase data are from:
Haase CG, Long AK, Gillooly JF. Energetics of stress: linking plasma cortisol levels to metabolic rate in mammals. Biol Lett. 2016 Jan;12(1):20150867. doi: 10.1098/rsbl.2015.0867

Myhrvold data are from :
P. Myhrvold, Nathan; Baldridge, Elita; Chan, Benjamin; Sivam, Dhileep; L. Freeman, Daniel; Ernest, S. K. Morgan (2016): An amniote life-history database to perform comparative analyses with birds, mammals, and reptiles. http://esapubs.org/archive/ecol/E096/269/

Turbill data are from: 
Turbill C, Ruf T (2010) Senescence Is More Important in the Natural Lives of Long- Than Short-Lived Mammals. PLOS ONE 5(8): e12019. https://doi.org/10.1371/journal.pone.0012019

AZA data are from: 
Survival Statistics Library [Internet]. Chicago (IL): Lincoln Park Zoo/AZA Population Management Center; 2008 - [accessed September 1, 2017 Sept 1]. Available from https://www.aza.org/speciessurvival-statistics



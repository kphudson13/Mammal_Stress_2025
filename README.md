# Mammal Stress 2025
Data and code are part of the 2025 manuscript to compare fecal glucocorticoid levels and metabolism across mammal species.  
Much of the analysis was exploratory and not included in the publication. 
The sections labelled 'Mean lifespan' for either glucocorticoid are the analysis we used, however, lifespan was not included in the publiction.  

## Usage
The code is partitioned such that the script CleanAndTree uses the open tree of life and various commands to clean raw data, this script is dependent on an OTL API key.   
If you do not want to recreate the tree and cleaned data, a tree and clean data set is in each hormone folder for use in other scripts.  
MasterScript runs various hormone configurtations through WorkingScript and AICScript, as if those were functions.  
PubFiguresScript uses much of the output from WorkingScript to select and order figured destined for the publication.  
There are no raw data in the FGCAnalysis folder because those data are made by combining the cortisol and corticosterone clean data based on dominance.  

## Credits
Haase data are from:
Haase CG, Long AK, Gillooly JF. Energetics of stress: linking plasma cortisol levels to metabolic rate in mammals. Biol Lett. 2016 Jan;12(1):20150867. doi: 10.1098/rsbl.2015.0867

                   _.-````'-,_
         _,.,_ ,-'`           `'-.,_
       /)     (                   '``-.
      ((      ) )                      `\
        \)    (_/                        )\
        |       /)           '    ,'    / \
        `\    ^'            '     (    /  ))
          |      _/\ ,     /    ,,`\   (  "`
          \Y,   |   \  \  | ````| / \_ \
            `)_/      \  \  )    ( >  ( >
                       \( \(     |/   |/
          mic & dwb  /_(/_(    /_(  /_(

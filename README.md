# Mammal Stress 2026
Data and code are part of the 2026 manuscript [Measuring Physiological Stress in Mammals: How Metabolism Influences Fecal Glucocorticoid Levels](https://doi.org/10.1111/jzo.70166)

The sections labelled 'Mean lifespan' for either glucocorticoid are the analysis we used, however, lifespan was not included in the publiction.  

## Usage
The code is partitioned such that the script CleanAndTree uses the open tree of life, NCBI and various commands to clean raw data, this script is dependent on an NCBI API key.   
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

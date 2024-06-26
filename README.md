# Kawasaki Disease and Autoimmune Disorders: exploratory analysis temporal trends in the children population
Analysis of the Kawasaki and autoimmune disease trends in the children population.

## Data
Analysis developed with the aim of using the 2.2 4CE consortium data

## Repo organization
- `R/`: contains the files carrying out the analysis
- `kawasaki-disease-exploration/`: contains the R code to get the counts and generate table 1 for Kawasaki disease patients
- `public-data/`: mapping of ICD codes to the disease of interest

## How to run this code?
First, clone the repository: git clone https://github.com/covidclinical/Phase2.2.kawasaki_ad

Then open the folder `kawasaki-disease-exploration/` and open the file `temporalVariabilityExploration.Rmd` and go to the section “Variables that need to be checked/modified by each site”:
- change the folder_4ce_files to the directory where your phase 2.2 AllAdm cohort data is located
- determine the obfuscation threshold: 
    - obfuscation = FALSE if no obfuscation
    - the numeric value of the obfuscation threshold if any; e.g. obfuscation = 3
    Make sure you comment the existing obfuscation line, set up as FALSE
- change the dateFormat to the one followed in your site (e.g., if your date looks like 03-AUG-20 follows the format "%d-%b-%y"). Further details about how to specificy the format below:

%d day as a number (0-31)	01-31
%a abbreviated weekday (e.g., Mon )
%A unabbreviated weekday (e.g, Monday)
%m	month (00-12)	00-12
%b abbreviated month (e.g, Jan)
%B unabbreviated month (e.g, January)
%y 2-digit year (e.g., 07)
%Y 4-digit year (e.g., 2007)


After all these changes are done, run the `temporalVariabilityExploration.Rmd`. 
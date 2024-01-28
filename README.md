# 3-Generation Family Medical Histories of Mental, Neurologic, Cardiometabolic, Birth Defect, Asthma, Allergy, and Autoimmune Conditions Associated with Autism
Diana Schendel, Linda Ejlskov, Morten Overgaard, Zeal Jinwala, Viktor Kim, Erik Parner, Amy Kalkbrenner, Christine Ladd-Acosta, M Danielle Fallin, Sherlly Xie, Preben Bo Mortensen, Brian Lee
medRxiv 2023.11.03.23298042; 
doi: https://doi.org/10.1101/2023.11.03.23298042

Description: Our study aimed to perform a comprehensive assessment of associations between autism and 3-generation family histories of 90 mental, neurologic, cardiometabolic, birth defect, asthma, allergy, and autoimmune conditions. The assessment comprised separate estimates of association with autism overall; separate estimates by sex and intellectual disability (ID) status; as well as separate estimates of the co-occurrence of each of the 90 disorders in autistic persons. Additionally, we aimed to provide interactive catalogues of results to facilitate results visualization and further hypothesis-generation.

In brief, we conducted a population-based, registry cohort study comprised of all live births in Denmark, 1980-2012, of Denmark-born parents, and with birth registry information (1,697,231 births), and their 3-generation family member types (20 types). All cohort members were followed from birth through April 10, 2017 for an ASD diagnosis. All participants (cohort members and each family member) were followed from birth through April 10, 2017 for each of 90 diagnoses, emigration or death. Adjusted hazard ratios (aHR) were estimated for ASD overall; by sex; or accounting for ID via separate Cox regression models for each diagnosis-family member type combination, adjusting for birth year, sex, birth weight, gestational age, parental ages at birth, and number of family member types of index person. aHRs were also calculated for sex-specific co-occurrence of each disorder, for ASD overall and considering ID.  

A catalogue of all study results displayed via interactive heat maps and the downloadable results data files can be accessed here: https://ncrr-au.shinyapps.io/asd-riskatlas/ . The downloadable data files contain the summary data needed to generate each aHR and the corresponding 95% confidence interval reported in the study results tables and figures: specifically, the log (HR), standard error, sample size (n of autistic persons with the specific diagnosis-family member type combination) per aHR (risk) estimate. 

Additional interactive graphic summaries of the results are also here: https://public.tableau.com/views/ASDPlots_16918786403110/e-Figure5 

This repository contains data and code to generate the RShiny app which describes the study design and provides interactive heatmaps of results (adjusted hazard ratios (aHR) calculated via Cox regression models). The data management scripts in the repository incorporate the aHR results tables to create heatmaps for each set of results. The full set of aHR results tables (5 tables in total of all summary data needed to generate each aHR and the corresponding 95% confidence interval) are also available in this repository and can be downloaded from the RShiny webapage. The app.R file is the master file that is used to build and design the interactive features of the RShiny webpage, incorporating the data management scripts to include tables and figures on the webpage. The RShiny webpage can be accessed here: https://ncrr-au.shinyapps.io/asd-riskatlas/ 

Corresponding author: Diana Schendel, A.J. Drexel Autism Institute, Drexel University, 3020 Market Street,
Suite 560, Philadelphia, PA, USA. Email: des348@drexel.edu Ph: +1 770-317-3008

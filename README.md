# PHP2550 Practical Data Analysis: Final Portfolio
## Description

## Project 1
### Overview

## Project 2
### Overview

## Project 3
### Overview
In this project, we analyzed the transportability of a cardiovascular disease (CVD) event model, which was fitted using Framingham data, to the extensive national NHANES dataset. We assessed this by using the Brier Score, both when individual-level data were available and when they were unavailable, through simulation.

### Abstract
Background: Assessing transportability is of importance when users of prediction models are interested in applying model-derived predictions to some different target populations. In recent years, several methods have been developed to evaluate the performance of prediction models in a target population (Paul 2023; Steingrimsson et al. 2023). In this study, we are interested in transporting the model for cardiovascular disease events constructed from Framingham Heart Study data to the more extensive National Health and Nutrition Examination Survey data.  

Methods: When the target population's covariates were available, we utilized multiple imputation to address missing data and then calculated the Brier score in NHANES data with both the original Framingham model and the tailored model, straitified by sex.  Additionally, when only summary data were available from the target population, simulation studies are conducted by drawing random samples using the summary statistics of the target population, and the Brier score was then calculated.   

Results: The Brier score of original Framingham model averages 0.1085 for men and 0.0412 for women, and the Brier score of tailored model averages 0.1056 for men and 0.0441 for women in NHANES population, which are about 50% of the Brier score validated on Framingham data themselves. The results of Brier score from simulation studies are close to the Brier score calculated using individual-level data directly.  

Conclusions: The results indicate the excellent performance of transporting the model constructed from Framingham data to the national NHANES data. Moreover, the simulation studies indicate when only summary data are available, the transportability analysis is workable and promising.   

Keywords: Transportablity, Cardiovascular Disease Events, Framingham Heart Study, NHANES

### Acknowledgement
This project is a collaboration with Dr. Jon Steingrimsson in the Department of Biostatistics, Brown University School of Public Health. The instructor is Dr. Alice Paul from Department of Biostatistics, Brown University School of Public Health.

### Data Availability
This data for this project is all publicly available. The data is obtained from R packages riskCommunicator_1.0.1 and nhanesA_0.7.4.

### Environment Version Information
R_4.1.2
tidyverse_1.3.2
kableExtra_1.3.4
gtsummary_1.7.2
latex2exp_0.9.6
ggpubr_0.4.0
mice_3.15.0
psych_2.2.9
riskCommunicator_1.0.1
nhanesA_0.7.4
tableone_0.13.2
caret_6.0-93
MASS_7.3-58.2
fitdistrplus_1.1-8

### Guideline for this repository
Files are in the `Project 3 Simulation Studies` folder. The `report` folder includes the updated report `Project3_updated.pdf` and the code for generating this report called `Project3_updated.Rmd`. The `code` folder includes the pre-processing code with exploratory data analysis called `project3_updated.R`, the code for calculating the Brier score when the individual-level data of NHANES are available called `evaluation_updated.R`, and the code for simulation studies called `simulation_updated.R`. The references `references.bib` in BibTeX can be found in the `references` folder. The pre-processed data can be found in the `pre-processed data` folder. The poster for this project is in the `poster` folder. 

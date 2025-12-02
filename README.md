# Associations between maternal micronutrients, hormones, and immune status during pregnancy and child growth in rural Bangladesh: a prospective cohort study 

A reproducible analysis for studying associations between pregnancy biomarkers and child growth outcomes.  

## Overview  

This repository contains data processing, statistical analysis, and figure/table generation scripts in R for the WASH-EDD substudy under the WASH-Benefits Trial. 

## Repository Structure  

| Folder | Purpose |
|--------|---------|
| `src/` | Core scripts: data cleaning, variable derivation, main analyses |
| `table scripts/` | Scripts to generate tables for results |
| `figure_scripts/` | Scripts to generate figures |
| `results/` | Output files: processed data, summary tables, model outputs, figures |
| `0_config.R` | Configuration script |

## Usage 

To reproduce the analyses:

### Clone the repository
```sh
git clone https://github.com/washb-eed-substudies/pregnancy-stress-growth.git
cd pregnancy-stress-growth
```

### Run the configuration
Rscript 0_config.R

### Run data processing & analyses in the `src\` folder
01 - generates the dataset from the master dataset
02 - fit a generalized additive model (GAM) without adjustment for covariates
03 - fits a GAM after adjustment for covariates
04 - applies Benjamini-Hochberg FDR correction for multiple timepoints

### Generate tables and figures
e.g., run scripts within `table scripts/` and `figure_scripts/`

### Citation

Chen B, Mertens AN, Lin CH, et al. Associations Between Micronutrient Status, Hormones, and Immune Status During Pregnancy and Child Growth in Rural Bangladesh: A Prospective Cohort Study. Current Developments in Nutrition. Published online November 8, 2025:107596. doi:10.1016/j.cdnut.2025.107596



# Supplementary Material for "A Joint Equivalence and Difference (JED) Test for Practical Use in Controlled Trials"

This repository contains supplementary material for the JED paper. The supplementary material includes the dataset used in the numerical example (Section 3) and the R code for generating the tables (Section 4) in the JED paper.

## Repository Structure

The repository is organized into two main folders:

### `Data`

This folder contains the dataset `Triple Hop Distances (in).xlsx` for the numerical example (Section 3).

### `R` 

This folder contains the R code used to calculate the JED power, the R code used to calculate the p-value, and the R code to generate the tables in Section 4.

The R script `Rcode_function_JEDpower.R` contains the R code for function `jed_power` to calculate the JED power as the equation shown in section 4.

The R script `Rcode_function_pvalue.R` contains the R code for function `jed_pvalues` to calculate the p-value as the equation shown in section 3.

The R markdown `Rcode_for_tables.Rmd` contains the R code for generating table 1, table 2, and table 3. 

To run these R scripts, you will need to library R package `tidyverse`, `knitr`, `kableExtra`, and `gridExtra`. 

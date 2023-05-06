
# Supplementary Material for "A Joint Equivalence and Difference (JED) Test for Practical Use in Controlled Trials"

This repository contains the supplementary material for the JED paper. The supplementary material includes the dataset used in the numerical example (Section 3), R code for generating the tables (Section 4), and the figure shown in the JED paper.

## Repository Structure

The repository is organized into three main folders:

### `Data`

This folder Contains the dataset `Triple Hop Distances (in).xlsx` for the numerical example (Section 3).

### `Figure`

This folder contains Figure 1 (`Figure1.pdf`) as shown in the paper.

### `R` 

This folder contains the R code used to generate the tables in Section 4 and any additional analysis.

The R script `R code for function to calculate JED power.R` contains the R code for function `jed_power` to calculate the JED power as the equation shown in section 4.

The R script `R code for tables (section4).R` contains the R code for table 1 and table 2. To run this R scripts, you will need to libaray R package `tidyverse`, `knitr`, `kableExtra`, and `gridExtra`. 

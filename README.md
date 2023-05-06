
# Supplementary Material for "A Joint Equivalence and Difference (JED) Test for Practical Use in Controlled Trials"

This repository contains supplementary material for the JED paper. The supplementary material includes the dataset used in the numerical example (Section 3), the R code for generating the tables (Section 4), and the figure shown in the JED paper.

## Repository Structure

The repository is organized into three main folders:

### `Data`

This folder contains the dataset `Triple Hop Distances (in).xlsx` for the numerical example (Section 3).

### `Figure`

This folder contains Figure 1 (`Figure1.pdf`) as shown in the paper.

### `R` 

This folder contains the R code used to calculate the JED power and the R code to generate the tables in Section 4.

The R script `R code for a function to calculate JED power.R` contains the R code for function `jed_power` to calculate the JED power as the equation shown in section 4.

The R script `R code for tables (section 4).R` contains the R code for generating table 1 and table 2. 

To run these R scripts, you will need to library R package `tidyverse`, `knitr`, `kableExtra`, and `gridExtra`. 

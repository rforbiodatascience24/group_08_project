# Project Contributors

This repository is for the final project of group 8 in R for bio data science. Group 8 has the below mentioned contributors:

Student ID - GitHub username

s215073 - DTU-MVH

s210857 - vitusthomas

s215086 - ceciliemoellerjensen

s215107 - amandavinterberg

s201523 - juliehaugaard

## Project Description

In this project we have analyzed gene expression of a metastatic breast cancer treatment that combines pembrolizumab (anti-PD1 antibody), exemestane (nonsteroidal aromatase inhibitor), and leuprolide (gonadotropin-releasing hormone agonist). We discovered deferentially expressed genes (DEGs) between pre and post treatment patients to elicit gene level treatment efficacy. We further explored gene expression levels of 4 different gene classes: Endogenous, Household, Positive, Negative. As well as looking into how the DEGs are spread between the classes.

We have adhered to using tidyverse functions when applicable and have followed the KISS (Keep It Stupid Simple) coding philosophy.

## Presentation

Direct link to our presentation can be found here:

<https://raw.githack.com/rforbiodatascience24/group_08_project/main/doc/presentation.html>

## Data Availability

Data can be found in the NCBI GEO database under the accesion: GSE261815. Data retrieval is done automatically in the source code through URLs to the files. Link to files provided below:

<https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE261815>

## Required Packages

The following packages are required:

-   `tidyverse`
-   `ggplot2`
-   `dplyr`
-   `here`
-   `stringr`
-   `quarto`
-   `patchwork`
-   `ggrepel`
-   `readr`
-   `rlang`

## Installation

Install the required packages with the following code in R:

`install.packages(c("tidyverse", "ggplot2", "dplyr", "here", "stringr", "quarto", "patchwork", "broom", "ggrepel", "readr", "rlang"))`

## Using the project code

To generate result files and run the data analysis, navigate to the directory of the project, then the R folder and run the `00_all.qmd`

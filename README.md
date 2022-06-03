## Data preparation

This repository shows the process of preparing the data for creating a statistical model in the R programming language.

The dataset concerns the parameters used in the beer brewing process. 11 variables were arbitrarily selected from a dataset containing 29 variables. These variables are to be used to build a statistical model that examines the effect of selected variables on alcohol by volume.

The process outlined includes:
- descriptive analysis of selected variables, determination of the measurement scale and visualization
- imputation of missing data
- outliers identification
- analysis of correlation between variables
- data sampling

## Used technology
- [R version 4.1.3](https://cran.r-project.org/src/base/R-4/)
- [RStudio](https://www.rstudio.com/)

## Used libraries
```r
library(dplyr)
library(ggplot2)
library(VIM)
library(gridExtra)
library(corrplot)
```

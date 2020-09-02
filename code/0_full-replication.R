# This code replicates all the data and output for the paper 
# Understanding economic openness: A review of existing measures 
# Available at: https://doi.org/10.1007/s10290-020-00391-1

rm(list = ls())
library(countrycode)
library(haven)
library(data.table)
library(tidyverse)
library(labelled)
library(cowplot)
library(magrittr)
library(ggpubr)
library(grid)
library(gridExtra)
library(foreign)
library(reshape)
library(viridis)
library(readr)
library(reshape2)
library(plm)
library(feather)
library(lmtest)
library(stargazer)
library(xtable)
library(CADFtest)
library(punitroots)
library(here)
if (!require(icaeDesign)){devtools::install_github(repo = "graebnerc/icaeDesign")}

# Create the data set ----
source("code/1_make_data.R")
  
# Make the trend figures for both main paper and appendix ----
source("code/2_make_trend_figures.R")

# Create the correlation matrices ----
source("code/3_correl-matrices.R")
  
# Conduct the regressions ----
source("code/4_0_make_regs.R")
  
# Analyze the regressions ----
source("code/5_analyze_regs.R")

# Appendix: Create the descriptive statistics ----
source("code/appendix_1_make-desc-stats.R")

# Appendix: Derive the comparison of de-facto vs. de-jure openness ----
source("code/appendix_3_rankings.R")

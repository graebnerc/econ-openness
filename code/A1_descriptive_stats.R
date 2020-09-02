rm(list = ls())
library(tidyverse)
library(data.table)
library(xtable)
library(here)
library(plm)
library(countrycode)

classification_variables <- c("ComplexityGroup", "IncomeGroup", "period")

# Descriptive statistics for the full dataset (table 1 of the appendix)--------
full_data_set <- fread(here("data/openness_1y.csv")) %>%
  dplyr::select(-one_of(classification_variables)) 

full_data_set_descriptives <- full_data_set %>%
  tidyr::pivot_longer(cols = -one_of("ccode", "Year"), 
               names_to = "Indicator", values_to = "Value") %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::group_by(Indicator) %>%
  dplyr::summarise(
    Observations=n(),
    Countries=n_distinct(ccode),
    Year_min=min(Year), 
    Year_max=max(Year)
    ) %>%
  dplyr::ungroup()

print(xtable(
  full_data_set_descriptives, label = "tab:descriptives", 
  caption = "Descriptive statistics for all indicators included in the data set."), 
  floating=FALSE, booktabs=TRUE, include.rownames = FALSE, 
  tabular.environment = "longtable", caption.placement = "top",
  file = here("appendix/descriptives1.tex"))

# Information about the countries in the full data (table 2 of the appendix)----

country_descriptives <- full_data_set %>%
  tidyr::pivot_longer(cols = -one_of("ccode", "Year"), 
                      names_to = "Indicator", values_to = "Value") %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::group_by(ccode) %>%
  dplyr::summarise(
    Observations=n(),
    Year_min=min(Year), 
    Year_max=max(Year)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Country=countrycode::countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::select(Country, Observations, Year_min, Year_max)

print(xtable(
  country_descriptives, label = "tab:countries", 
  caption = "Countries included in the full data set."), 
  floating=FALSE, booktabs=TRUE, include.rownames = FALSE, 
  tabular.environment = "longtable", caption.placement = "top",
  file = here("appendix/descriptives_full_country.tex"))

# Descriptive information about the reduced data set (table 3 in the appendix)----

control_vars_1y <- " + log(hc) + pop_growth + inflation + log(inv_share)"
expl_regression <- fread(here("data/reduced_reg_sample_1y.csv")) %>%
    dplyr::filter(UQ(as.name("Trade_to_GDP"))>0) %>%
  plm(as.formula(
    paste0("GDP_pc_growth~log(Trade_to_GDP)", control_vars_1y)
    ), 
    data=., 
    index = c("ccode","Year"), 
    model="within", 
    effect="twoways")


reg_data_descriptives <- data.frame(
  table(index(expl_regression$model)$ccode)) %>%
  dplyr::rename(Country=Var1, Observations=Freq) %>%
  dplyr::mutate(Country=countrycode(Country, "iso3c", "country.name"))

reg_data_descriptives <- rbind(
  reg_data_descriptives, 
  data.frame(Country="Total", 
             "Observations"=sum(reg_data_descriptives$Observations)))

print(xtable(
  reg_data_descriptives, label = "tab:red_data", 
  caption = "The countries used in the regressions of the main paper."), 
  floating=FALSE, booktabs=TRUE, include.rownames = FALSE, 
  tabular.environment = "longtable", caption.placement = "top",
  file = here("appendix/descriptives_red_data.tex"))

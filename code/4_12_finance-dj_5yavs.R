# Conductions regressions  ----------------------------------------------------

# KAOPEN
reg_1 <- plm(as.formula(
  paste0("GDP_pc_growth~log(chinn_ito_normed)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual") 

# HF_fin
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(HF_fin)", control_vars_5y)
  ), 
  data=mutate(
    original_data_ext_5_year, HF_fin=ifelse(HF_fin==0.0, NA, HF_fin)
    ),
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# CAPITAL
reg_3 <- plm(as.formula(
  paste0("GDP_pc_growth~log(CAPITAL)", control_vars_5y)
  ), 
  data=mutate(
    original_data_ext_5_year, CAPITAL=ifelse(CAPITAL==0.0, NA, CAPITAL)
    ), 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# Adding robust standard errors -----------------------------------------------

finance_de_jure_results_log_5y <- list()
finance_de_jure_results_log_5y[["KAOPEN"]] <- reg_1
finance_de_jure_results_log_5y[["KAOPEN_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_5y[["HF_fin"]] <- reg_2
finance_de_jure_results_log_5y[["HF_fin_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_5y[["CAPITAL"]] <- reg_3
finance_de_jure_results_log_5y[["CAPITAL_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

# Save information about used data---------------------------------------------

data5y <- data.frame(table(index(reg_1$model)$ccode)) %>%
  dplyr::rename(Country=Var1, Observations=Freq) %>%
  dplyr::mutate(Country=countrycode(Country, "iso3c", "country.name"))

data5y <- rbind(data5y, 
                  data.frame(Country="Total", 
                             "Observations"=sum(data5y$Observations)))
print(xtable(data5y, 
             label = "The countries used for the 5-year avergage regressions."), 
      type="html", file="output/dataset_5y.html", include.rownames=FALSE)

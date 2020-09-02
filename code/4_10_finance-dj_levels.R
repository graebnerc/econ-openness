# Conductions regressions  ----------------------------------------------------

# KAOPEN
open_var <- "chinn_ito_normed"
reg_1 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

# HF_fin
open_var <- "HF_fin"
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

# CAPITAL
open_var <- "CAPITAL"
reg_3 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

# Adding robust standard errors -----------------------------------------------

finance_de_jure_results_log <- list()
finance_de_jure_results_log[["KAOPEN"]] <- reg_1
finance_de_jure_results_log[["KAOPEN_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log[["HF_fin"]] <- reg_2
finance_de_jure_results_log[["HF_fin_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log[["CAPITAL"]] <- reg_3
finance_de_jure_results_log[["CAPITAL_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

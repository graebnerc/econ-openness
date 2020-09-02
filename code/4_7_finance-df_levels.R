# Conductions regressions  ----------------------------------------------------

# 5.1. Make annual regressions ----

#LMF open
open_var <- "LMF_open" # LMF_open
reg_3 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

open_var <- "LMF_open_gdp" # LMF_open
reg_31 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

#LMF EQ
open_var <- "LMF_EQ"
reg_4 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

open_var <- "LMF_EQ_gdp"
reg_41 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

#FDI in
open_var <- "UNC_FDI_in_stock_GDP"
reg_5 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

#FDI out
open_var <- "UNC_FDI_out_stock_GDP"
reg_6 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")

# Adding robust standard errors -----------------------------------------------

finance_de_facto_results_log <- list()
finance_de_facto_results_log[["LMF_open"]] <- reg_3
finance_de_facto_results_log[["LMF_open_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["LMF_open_gdp"]] <- reg_31
finance_de_facto_results_log[["LMF_open_gdp_coeftest"]] <- coeftest(
  reg_31, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["LMF_EQ"]] <- reg_4
finance_de_facto_results_log[["LMF_EQ_coeftest"]] <- coeftest(
  reg_4, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["LMF_EQ_gdp"]] <- reg_41
finance_de_facto_results_log[["LMF_EQ_gdp_coeftest"]] <- coeftest(
  reg_41, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["UNC_FDI_in_stock_GDP"]] <- reg_5
finance_de_facto_results_log[["UNC_FDI_in_stock_GDPcoeftest"]] <- coeftest(
  reg_5, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["UNC_FDI_out_stock_GDP"]] <- reg_6
finance_de_facto_results_log[["UNC_FDI_out_stock_GDP_coeftest"]] <- coeftest(
  reg_6, vcov.=function(x) vcovHC(x, type="sss"))

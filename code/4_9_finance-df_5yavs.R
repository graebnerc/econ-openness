# Conductions regressions  ----------------------------------------------------

#LMF open
reg_3<-plm(as.formula(
  paste0("GDP_pc_growth~log(LMF_open)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

reg_31<-plm(as.formula(
  paste0("GDP_pc_growth~log(LMF_open_gdp)", control_vars_5y)
), 
data=original_data_ext_5_year, 
index = c("ccode","Year"), 
model="within", 
effect="individual")

#LMF EQ
reg_4<-plm(as.formula(
  paste0("GDP_pc_growth~log(LMF_EQ)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

reg_41<-plm(as.formula(
  paste0("GDP_pc_growth~log(LMF_EQ_gdp)", control_vars_5y)
), 
data=original_data_ext_5_year, 
index = c("ccode","Year"), 
model="within", 
effect="individual")

#FDI in
reg_5<-plm(as.formula(
  paste0("GDP_pc_growth~log(UNC_FDI_in_stock_GDP)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#FDI out
reg_6<-plm(as.formula(
  paste0("GDP_pc_growth~log(UNC_FDI_out_stock_GDP)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# Adding robust standard errors -----------------------------------------------

finance_de_facto_results_log_5y <- list()
finance_de_facto_results_log_5y[["LMF_open"]] <- reg_3
finance_de_facto_results_log_5y[["LMF_open_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["LMF_open_gdp"]] <- reg_31
finance_de_facto_results_log_5y[["LMF_open_gdp_coeftest"]] <- coeftest(
  reg_31, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["LMF_EQ"]] <- reg_4
finance_de_facto_results_log_5y[["LMF_EQ_coeftest"]] <- coeftest(
  reg_4, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["LMF_EQ_gdp"]] <- reg_41
finance_de_facto_results_log_5y[["LMF_EQ_gdp_coeftest"]] <- coeftest(
  reg_41, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["UNC_FDI_in_stock_GDP"]] <- reg_5
finance_de_facto_results_log_5y[["UNC_FDI_in_stock_GDP_coeftest"]] <- coeftest(
  reg_5, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["UNC_FDI_out_stock_GDP"]] <- reg_6
finance_de_facto_results_log_5y[["UNC_FDI_out_stock_GDP_coeftest"]] <- coeftest(
  reg_6, vcov.=function(x) vcovHC(x, type="sss"))

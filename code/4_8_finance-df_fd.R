# Conductions regressions  ----------------------------------------------------

#LMF open
open_var <- "LMF_open"
reg_3_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

open_var <- "LMF_open_gdp"
reg_31_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="fd")

#LMF EQ
open_var <- "LMF_EQ"
reg_4_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

open_var <- "LMF_EQ_gdp"
reg_41_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="fd")

#FDI in
open_var <- "UNC_FDI_in_stock_GDP"
reg_5_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

#FDI out
open_var <- "UNC_FDI_out_stock_GDP"
reg_6_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Adding robust standard errors -----------------------------------------------

finance_de_facto_results_log_fd <- list()
finance_de_facto_results_log_fd[["LMF_open"]] <- reg_3_fd
finance_de_facto_results_log_fd[["LMF_open_coeftest"]] <- coeftest(
  reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["LMF_open_gdp"]] <- reg_31_fd
finance_de_facto_results_log_fd[["LMF_open_gdp_coeftest"]] <- coeftest(
  reg_31_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["LMF_EQ"]] <- reg_4_fd
finance_de_facto_results_log_fd[["LMF_EQ_coeftest"]] <- coeftest(
  reg_4_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["LMF_EQ_gdp"]] <- reg_41_fd
finance_de_facto_results_log_fd[["LMF_EQ_gdp_coeftest"]] <- coeftest(
  reg_41_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["UNC_FDI_in_stock_GDP"]] <- reg_5_fd
finance_de_facto_results_log_fd[["UNC_FDI_in_stock_GDP_coeftest"]] <- coeftest(
  reg_5_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["UNC_FDI_out_stock_GDP"]] <- reg_6_fd
finance_de_facto_results_log_fd[["UNC_FDI_out_stock_GDP_coeftest"]] <- coeftest(
  reg_6_fd, vcov.=function(x) vcovHC(x, type="sss"))

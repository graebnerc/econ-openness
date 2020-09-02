# Conductions regressions  ----------------------------------------------------

# KAOPEN
open_var <- "chinn_ito_normed"
reg_1_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# HF_fin
open_var <- "HF_fin"
reg_2_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# CAPITAL
open_var <- "CAPITAL"
reg_3_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Adding robust standard errors -----------------------------------------------

finance_de_jure_results_log_fd <- list()
finance_de_jure_results_log_fd[["KAOPEN"]] <- reg_1_fd
finance_de_jure_results_log_fd[["KAOPEN_coeftest"]] <- coeftest(
  reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_fd[["HF_fin"]] <- reg_2_fd
finance_de_jure_results_log_fd[["HF_fin_coeftest"]] <- coeftest(
  reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_fd[["CAPITAL"]] <- reg_3_fd
finance_de_jure_results_log_fd[["CAPITAL_coeftest"]] <- coeftest(
  reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))

# Conductions regressions  ----------------------------------------------------
#KOF de jure
open_var <- "KOF_dejure"
reg_1_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

#FTI index
open_var <- "FTI_original_ipo"
reg_2_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Tariff_WITS_ipo
open_var <- "Tariff_WITS_ipo"
reg_7_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# HF_trade
open_var <- "HF_trade"
reg_8_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Adding robust standard errors -----------------------------------------------
trade_de_jure_results_log_fd <- list()
trade_de_jure_results_log_fd[["kof_dejure"]] <- reg_1_fd
trade_de_jure_results_log_fd[["kof_dejure_coeftest"]] <- coeftest(
  reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["FTI"]] <- reg_2_fd
trade_de_jure_results_log_fd[["FTI_coeftest"]] <- coeftest(
  reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["Tariff_WITS_ipo"]] <- reg_7_fd
trade_de_jure_results_log_fd[["Tariff_WITS_ipo_coeftest"]] <- coeftest(
  reg_7_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["HF_trade"]] <- reg_8_fd
trade_de_jure_results_log_fd[["HF_trade_coeftest"]] <- coeftest(
  reg_8_fd, vcov.=function(x) vcovHC(x, type="sss"))

# Trade_to_GDP
open_var <- "Trade_to_GDP"
reg_1_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Alcala
open_var <- "Alcala"
reg_2_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# Lietal
open_var <- "Lietal"
reg_3_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# CTS
open_var <- "CTS"
reg_4_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# KOF de facto
open_var <- "KOF_defacto"
reg_5_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# TOI
open_var <- "TOI"
reg_6_fd <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
  ), 
  data=filter_data(open_var), 
  index = c("ccode","Year"), 
  model="fd")

# 1.4. List : annual trade de facto ----
trade_de_facto_results_log_fd <- list()
trade_de_facto_results_log_fd[["trade_to_GDP"]] <- reg_1_fd
trade_de_facto_results_log_fd[["trade_to_GDP_coeftest"]] <- coeftest(
  reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["alcala"]] <- reg_2_fd
trade_de_facto_results_log_fd[["alcala_coeftest"]] <- coeftest(
  reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["lietal"]] <- reg_3_fd
trade_de_facto_results_log_fd[["lietal_coeftest"]] <- coeftest(
  reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["cts"]] <- reg_4_fd
trade_de_facto_results_log_fd[["cts_coeftest"]] <- coeftest(
  reg_4_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["kof_defacto"]] <- reg_5_fd
trade_de_facto_results_log_fd[["kof_defacto_coeftest"]] <- coeftest(
  reg_5_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["toi"]] <- reg_6_fd
trade_de_facto_results_log_fd[["toi_coeftest"]] <- coeftest(
  reg_6_fd, vcov.=function(x) vcovHC(x, type="sss"))


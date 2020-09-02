# 2.1. Make 5y regressions ----

#trade to GDP
reg_1<-plm(as.formula(
  paste0("GDP_pc_growth~log(Trade_to_GDP)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#Alcala
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(Alcala)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#Lietal
reg_3 <- plm(as.formula(
  paste0("GDP_pc_growth~log(Lietal)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#CTS
reg_4 <- plm(as.formula(
  paste0("GDP_pc_growth~log(CTS)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#KOF de facto
reg_5 <- plm(as.formula(
  paste0("GDP_pc_growth~log(KOF_defacto)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#TOI
reg_6 <- plm(as.formula(
  paste0("GDP_pc_growth~log(TOI)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# 2.2. Make 5-year list : trade de facto ----
trade_de_facto_results_log_5y <- list()
trade_de_facto_results_log_5y[["trade_to_GDP"]] <- reg_1
trade_de_facto_results_log_5y[["trade_to_GDP_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["alcala"]] <- reg_2
trade_de_facto_results_log_5y[["alcala_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["lietal"]] <- reg_3
trade_de_facto_results_log_5y[["lietal_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["cts"]] <- reg_4
trade_de_facto_results_log_5y[["cts_coeftest"]] <- coeftest(
  reg_4, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["kof_defacto"]] <- reg_5
trade_de_facto_results_log_5y[["kof_defacto_coeftest"]] <- coeftest(
  reg_5, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["toi"]] <- reg_6
trade_de_facto_results_log_5y[["toi_coeftest"]] <- coeftest(
  reg_6, vcov.=function(x) vcovHC(x, type="sss"))

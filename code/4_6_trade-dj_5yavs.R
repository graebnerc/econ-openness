# Conductions regressions  ----------------------------------------------------

#KOF de jure
reg_1 <- plm(as.formula(
  paste0("GDP_pc_growth~log(KOF_dejure)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

#FTI index
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(FTI_original_ipo)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# Tariff_WITS_ipo
reg_7 <- plm(as.formula(
  paste0("GDP_pc_growth~log(Tariff_WITS_ipo)", control_vars_5y)
  ), 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# HF_trade
reg_8 <- plm(as.formula(
  paste0("GDP_pc_growth~log(HF_trade)", control_vars_5y)
  ), 
  data=mutate(original_data_ext_5_year, 
              HF_trade=ifelse(HF_trade==0.0, NA, HF_trade)), 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")

# Adding robust standard errors -----------------------------------------------

trade_de_jure_results_log_5y <- list()
trade_de_jure_results_log_5y[["kof_dejure"]] <- reg_1
trade_de_jure_results_log_5y[["kof_dejure_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["FTI"]] <- reg_2
trade_de_jure_results_log_5y[["FTI_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["Tariff_WITS_ipo"]] <- reg_7
trade_de_jure_results_log_5y[["Tariff_WITS_ipo_coeftest"]] <- coeftest(
  reg_7, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["HF_trade"]] <- reg_8
trade_de_jure_results_log_5y[["HF_trade_coeftest"]] <- coeftest(
  reg_8, vcov.=function(x) vcovHC(x, type="sss"))

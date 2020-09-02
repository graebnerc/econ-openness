# 3.1. Make annual regressions ----

#KOF de jure
open_var <- "KOF_dejure"
reg_1 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

#FTI index
open_var <- "FTI_original_ipo"
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# Tariff_WITS_ipo
open_var <- "Tariff_WITS_ipo"
reg_7 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# HF_trade
open_var <- "HF_trade"
reg_8 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# 3.2. Make annual reg list : trade de jure ----
trade_de_jure_results_log <- list()
trade_de_jure_results_log[["kof_dejure"]] <- reg_1
trade_de_jure_results_log[["kof_dejure_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["FTI"]] <- reg_2
trade_de_jure_results_log[["FTI_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["Tariff_WITS_ipo"]] <- reg_7
trade_de_jure_results_log[["Tariff_WITS_ipo_coeftest"]] <- coeftest(
  reg_7, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["HF_trade"]] <- reg_8
trade_de_jure_results_log[["HF_trade_coeftest"]] <- coeftest(
  reg_8, vcov.=function(x) vcovHC(x, type="sss"))

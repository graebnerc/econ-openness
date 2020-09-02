# This file conducts all regressions for the paper
# Regression section 1-12 conducts the regressions underlying table 8 of
# the main paper. Section 13 replicates the regressions for table 9.
rm(list = ls()) 

#reg_data <- readRDS("data/regdata.rds")
reg_data <- fread(here("data/openness_1y.csv"))
reg_data_5y <- fread(here("data/openness_5y.csv"))

reg_data_5y_first_year_gdp <- reg_data  %>%
  dplyr::filter(Year %in% unique(reg_data_5y$first_year)) %>%
  dplyr::select(one_of("ccode", "Penn_GDP_PPP", "Year")) %>%
  dplyr::rename(initial_GDP_pc = Penn_GDP_PPP)

reg_data_5y <- left_join(reg_data_5y, reg_data_5y_first_year_gdp, 
                         by = c("ccode", "first_year" = "Year"))

# Define common data set=======================================================

list_of_dependent_variables <- c(
  "Trade_to_GDP", "Alcala", "Lietal", "CTS", "KOF_defacto", "TOI", "KOF_dejure", 
  "FTI_original_ipo", "Tariff_WITS_ipo", "HF_trade", "KAOPEN", "HF_fin", #"CAPITAL", 
  "LMF_open_gdp", "LMF_EQ_gdp", "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP"
)
# for all of these variables: remove na, and those values for which log is not defined

# Jaehrliche Daten
lietal_min <- abs(min(reg_data$Lietal, na.rm = T))
minimum_obs_1y <- 10
reg_data_sub <- reg_data %>%
  filter_at(vars(list_of_dependent_variables), all_vars(!is.na(.))) %>%
  dplyr::filter(
    hc>0,
    inv_share>0, 
    LMF_EQ>0, LMF_EQ_gdp>0, 
    UNC_FDI_in_stock_GDP>0, 
    UNC_FDI_out_stock_GDP>0) %>%
  dplyr::mutate(chinn_ito_normed=chinn_ito_normed+0.1, 
                Lietal=Lietal+lietal_min+0.1) %>%
  group_by(ccode) %>%
  dplyr::filter(n()>=minimum_obs_1y) %>%
  ungroup()
  
obs_per_country <- reg_data_sub %>%
  dplyr::group_by(ccode) %>%
  summarise(Observations=n()) %>%
  dplyr::ungroup() %>%
  mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  select(Country, Observations)
obs_per_country

# 5-Jahreszeiträume Daten
lietal_min <- abs(min(reg_data_5y$Lietal, na.rm = T))
minimum_obs_5y <- 3
reg_data_sub_5y <- reg_data_5y %>%
  filter_at(vars(list_of_dependent_variables), all_vars(!is.na(.))) %>%
  dplyr::filter(
    hc>0,
    inv_share>0, 
    initial_GDP_pc>0,
    LMF_EQ>0, LMF_EQ_gdp>0, 
    UNC_FDI_in_stock_GDP>0, 
    UNC_FDI_out_stock_GDP>0) %>%
  dplyr::mutate(
    chinn_ito_normed=chinn_ito_normed+0.1,
    Lietal=Lietal+lietal_min+0.1) %>%
  group_by(ccode) %>%
  dplyr::filter(n()>=minimum_obs_5y) %>%
  ungroup()

obs_per_country_5y <- reg_data_sub_5y %>%
  dplyr::group_by(ccode) %>%
  summarise(Observations=n()) %>%
  dplyr::ungroup() %>%
  mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  select(Country, Observations)
obs_per_country_5y

original_data_ext<- reg_data_sub
fwrite(original_data_ext, here("data/reduced_reg_sample_1y.csv"))
original_data_ext_5_year <- reg_data_sub_5y
fwrite(original_data_ext_5_year, here("data/reduced_reg_sample_5y.csv"))
# Remove all countries with less than X observations
## Würde ich erstmal weg lassen, weil das ja schon über die abhängigen V geht
# check how much is missing
# remove all countries with less than 10 observations in any single of the variables

control_vars_1y <- " + log(hc) + pop_growth + inflation + log(inv_share)"
control_vars_5y <- " + log(initial_GDP_pc) + log(hc) + pop_growth + inflation + log(inv_share)"

# ESTIMATIONS =================================================================
filter_data <- function(var_name){
  # Ensures that dependent variable are greater than zero
  dat_final <- reg_data_sub %>%
    dplyr::filter(UQ(as.name(var_name)) > 0)
  return(dat_final)
}

# 1. Trade de facto - annual in levels ========================================
source("code/4_1_trade-df_levels.R")

# 2. Trade de facto - annual in FD ============================================
source("code/4_2_trade-df_fd.R")

# 3. Trade de facto - 5 year averages in levels ===============================
source("code/4_3_trade-df_5yavs.R")

# 4. Trade de jure - annual in levels =========================================
source("code/4_4_trade-dj_levels.R")

# 5. Trade de jure - annual in FD =============================================
source("code/4_5_trade-dj_fd.R")

# 6. Trade de jure - 5 year averages in levels ================================
source("code/4_6_trade-dj_5yavs.R")

# 7. Finance de facto - annual in levels ======================================
source("code/4_7_finance-df_levels.R")

# 8. Finance de facto - annual in fd ==========================================
source("code/4_8_finance-df_fd.R")

# 9. Finance de facto - 5 year averages in levels =============================
source("code/4_9_finance-df_5yavs.R")

# 10. Finance de jure - annual in levels ======================================
source("code/4_10_finance-dj_levels.R")

# 11. Finance de jure - annual in fd ==========================================
source("code/4_11_finance-dj_fd.R")

# 12. Finance de jure - 5 year averages in levels =============================
source("code/4_12_finance-dj_5yavs.R")

# 13. Full specification ======================================================
full_spec <- list()

full_spec_l1 <- as.formula(paste0(
  "GDP_pc_growth~log(KOF_econ)", 
  control_vars_1y))

full_spec_l2_djdf <- as.formula(paste0(
  "GDP_pc_growth~log(KOF_defacto)+log(KOF_dejure)", 
  control_vars_1y))

full_spec_l2_trfin <- as.formula(paste0(
  "GDP_pc_growth~log(KOF_trade)+log(KOF_finance)", 
  control_vars_1y))

full_spec_l3_kofs <- as.formula(paste0(
  "GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
  control_vars_1y))

full_spec_l3_inds <- as.formula(paste0(
  "GDP_pc_growth~log(Trade_to_GDP)+log(Tariff_WITS_ipo)+log(LMF_open)+log(KAOPEN)", 
  control_vars_1y))

full_spec_l3_inds <- as.formula(paste0(
  "GDP_pc_growth~log(Trade_to_GDP)+log(Tariff_WITS_ipo)+log(LMF_open_gdp)+log(KAOPEN)", 
  control_vars_1y))


# Full specification with only KOF_econ ----
full_reg_1_1y <- plm(
  full_spec_l1, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")
full_spec[["full_spec_kof_1y"]] <- full_reg_1_1y
full_spec[["full_spec_kof_1y_coeftest"]] <- coeftest(
  full_reg_1_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_1_1y_fd <- plm(
  full_spec_l1, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="fd")
full_spec[["full_spec_kof_1y_fd"]] <- full_reg_1_1y_fd
full_spec[["full_spec_kof_1y_fd_coeftest"]] <- coeftest(
  full_reg_1_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_1_5y <- plm(
  full_spec_l1, 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")
full_spec[["full_spec_kof_5y"]] <- full_reg_1_5y
full_spec[["full_spec_kof_5y_coeftest"]] <- coeftest(
  full_reg_1_5y, vcov.=function(x) vcovHC(x, type="sss"))

# Full specification with KOF de jure and KOF de facto ----
full_reg_2dfdj_1y <- plm(
  full_spec_l2_djdf, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")
full_spec[["full_spec_kof_dfdj_1y"]] <- full_reg_2dfdj_1y
full_spec[["full_spec_kof_dfdj_1y_coeftest"]] <- coeftest(
  full_reg_2dfdj_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_2dfdj_1y_fd <- plm(
  full_spec_l2_djdf, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="fd")
full_spec[["full_spec_kof_dfdj_1y_fd"]] <- full_reg_2dfdj_1y_fd
full_spec[["full_spec_kof_dfdj_1y_fd_coeftest"]] <- coeftest(
  full_reg_2dfdj_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_2dfdj_5y <- plm(
  full_spec_l2_djdf, 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")
full_spec[["full_spec_kof_dfdj_5y"]] <- full_reg_2dfdj_5y
full_spec[["full_spec_kof_dfdj_5y_coeftest"]] <- coeftest(
  full_reg_2dfdj_5y, vcov.=function(x) vcovHC(x, type="sss"))

# Full specification with KOF trade and KOF finance ----
full_reg_2trfin_1y <- plm(
  full_spec_l2_trfin, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")
full_spec[["full_spec_kof_trfin_1y"]] <- full_reg_2trfin_1y
full_spec[["full_spec_kof_trfin_1y_coeftest"]] <- coeftest(
  full_reg_2trfin_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_2trfin_1y_fd <- plm(
  full_spec_l2_trfin, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="fd")
full_spec[["full_spec_kof_trfin_1y_fd"]] <- full_reg_2trfin_1y_fd
full_spec[["full_spec_kof_trfin_1y_fd_coeftest"]] <- coeftest(
  full_reg_2trfin_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_2trfin_5y <- plm(
  full_spec_l2_trfin, 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")
full_spec[["full_spec_kof_trfin_5y"]] <- full_reg_2trfin_5y
full_spec[["full_spec_kof_trfin_5y_coeftest"]] <- coeftest(
  full_reg_2trfin_5y, vcov.=function(x) vcovHC(x, type="sss"))

# Full specification with KOF de jure/de facto for trade/finance ----
full_reg_l3_kofs_1y <- plm(
  full_spec_l3_kofs, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")
full_spec[["full_spec_kof_trfin_dfdj_1y"]] <- full_reg_l3_kofs_1y
full_spec[["full_spec_kof_trfin_dfdj_1y_coeftest"]] <- coeftest(
  full_reg_l3_kofs_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_l3_kofs_1y_fd <- plm(
  full_spec_l3_kofs, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="fd")
full_spec[["full_spec_kof_trfin_dfdj_1y_fd"]] <- full_reg_l3_kofs_1y_fd
full_spec[["full_spec_kof_trfin_dfdj_1y_fd_coeftest"]] <- coeftest(
  full_reg_l3_kofs_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_l3_kofs_5y <- plm(
  full_spec_l3_kofs, 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")
full_spec[["full_spec_kof_trfin_dfdj_5y"]] <- full_reg_l3_kofs_5y
full_spec[["full_spec_kof_trfin_dfdj_5y_coeftest"]] <- coeftest(
  full_reg_l3_kofs_5y, vcov.=function(x) vcovHC(x, type="sss"))

# Full specification with de jure/de facto measures for trade/finance----
full_reg_l3_inds_1y <- plm(# TODO HERE ERRORS
  full_spec_l3_inds, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="within", 
  effect="twoways")
full_spec[["full_spec_other_trfin_dfdj_1y"]] <- full_reg_l3_inds_1y
full_spec[["full_spec_other_trfin_dfdj_1y_coeftest"]] <- coeftest(
  full_reg_l3_inds_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_l3_inds_1y_fd <- plm(# TODO HERE ERRORS
  full_spec_l3_inds, 
  data=original_data_ext, 
  index = c("ccode","Year"), 
  model="fd")
full_spec[["full_spec_other_trfin_dfdj_1y_fd"]] <- full_reg_l3_inds_1y_fd
full_spec[["full_spec_other_trfin_dfdj_1y_fd_coeftest"]] <- coeftest(
  full_reg_l3_inds_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_reg_l3_inds_5y <- plm(# TODO HERE ERRORS
  full_spec_l3_inds, 
  data=original_data_ext_5_year, 
  index = c("ccode","Year"), 
  model="within", 
  effect="individual")
full_spec[["full_spec_other_trfin_dfdj_5y"]] <- full_reg_l3_inds_5y
full_spec[["full_spec_other_trfin_dfdj_5y_coeftest"]] <- coeftest(
  full_reg_l3_inds_5y, vcov.=function(x) vcovHC(x, type="sss"))

# 14. Save regression files ----

regressions_list <- list()
regressions_list[["trade_de_facto_results_log"]] <- trade_de_facto_results_log
regressions_list[["trade_de_facto_results_log_fd"]] <- trade_de_facto_results_log_fd
regressions_list[["trade_de_facto_results_log_5y"]] <- trade_de_facto_results_log_5y
regressions_list[["trade_de_jure_results_log"]] <- trade_de_jure_results_log
regressions_list[["trade_de_jure_results_log_fd"]] <- trade_de_jure_results_log_fd
regressions_list[["trade_de_jure_results_log_5y"]] <- trade_de_jure_results_log_5y
regressions_list[["finance_de_facto_results_log"]] <- finance_de_facto_results_log
regressions_list[["finance_de_facto_results_log_fd"]] <- finance_de_facto_results_log_fd
regressions_list[["finance_de_facto_results_log_5y"]] <- finance_de_facto_results_log_5y
regressions_list[["finance_de_jure_results_log"]]<- finance_de_jure_results_log
regressions_list[["finance_de_jure_results_log_fd"]]<- finance_de_jure_results_log_fd
regressions_list[["finance_de_jure_results_log_5y"]]<- finance_de_jure_results_log_5y
regressions_list[["full_spec"]] <- full_spec
saveRDS(regressions_list, "output/sec5_regressions.rds")


rm(list = ls()) #clear list
# 
# #load packages
# library(plyr)
# library(plm)
# library(foreign)
# library(countrycode)
# library(feather)
# library(tidyverse)
# library(lmtest)

make_bivariate_plots <- FALSE # set to true to make the plots (takes a bit more time)

reg_data <- readRDS("data/regdata.rds")

original_data_ext <- reg_data$original_data_ext %>%
  #dplyr::mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(#!is.na(Country),
         hc>0,
         inv_share>0)

original_data_ext$Country <- countrycode(original_data_ext$ccode, 'iso3c', 'country.name') #convert OECD name codes to three letter country codes (iso3n)

original_data_ext_5_year <- reg_data$original_data_ext_5_year %>%
  #dplyr::mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(#!is.na(Country),
         hc>0,
         initial_GDP_pc>0,
         inv_share>0)

control_vars_1y <- " + log(hc) + pop_growth + inflation + log(inv_share)"
control_vars_5y <- " + log(initial_GDP_pc) + log(hc) + pop_growth + inflation + log(inv_share)"

# Check availability - filtered ----
avail <- list()
for (i in names(original_data_ext_5_year)){
  avail[[i]][["n_obs"]] <- sum(!is.na(original_data_ext_5_year[i]))
  avail[[i]][["share_nonna"]] <- sum(!is.na(original_data_ext_5_year[i]))/nrow(original_data_ext_5_year[i])
}
availability <- as.data.frame(avail)
availability$measure <- c("n_obs", "share_nonna")
overview_availability <- availability %>%
  gather(variab, val, -measure) %>%
  spread(measure, val)
#

# Check availability - un-filtered ----
original_data_ext_5_year_unfilter <- reg_data$original_data_ext_5_year
avail_un <- list()
for (i in names(original_data_ext_5_year_unfilter)){
  avail_un[[i]][["n_obs_unfilter"]] <- sum(!is.na(original_data_ext_5_year_unfilter[i]))
  avail_un[[i]][["share_nonna_unfilter"]] <- sum(!is.na(original_data_ext_5_year_unfilter[i]))/nrow(original_data_ext_5_year_unfilter[i])
}
availability_unfilter <- as.data.frame(avail_un)
availability_unfilter$measure <- c("n_obs_unfilter", "share_nonna_unfilter")
overview_availability_unfilter <- availability_unfilter %>%
  gather(variab, val, -measure) %>%
  spread(measure, val)

availability_complete <- left_join(overview_availability, overview_availability_unfilter, by=c("variab"))

#

# 1. Trade de facto - annual ----
filter_data <- function(var_name){
  # Ensures that dependent variable are greater than zero
  dat_final <- original_data_ext %>%
    dplyr::filter(UQ(as.name(var_name)) > 0)
  return(dat_final)
}
# 1.1. Make annual regressions ----

open_var <- "Trade_to_GDP"
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# Alcala
open_var <- "Alcala"
reg_2<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# Lietal
open_var <- "Lietal"
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# CTS
open_var <- "CTS"
reg_4<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# KOF de facto
open_var <- "KOF_defacto"
reg_5<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# TOI
open_var <- "TOI"
reg_6<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# 1.2. List : annual trade de facto ----
trade_de_facto_results_log <- list()
trade_de_facto_results_log[["trade_to_GDP"]] <- reg_1
trade_de_facto_results_log[["trade_to_GDP_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["alcala"]] <- reg_2
trade_de_facto_results_log[["alcala_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["lietal"]] <- reg_3
trade_de_facto_results_log[["lietal_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["cts"]] <- reg_4
trade_de_facto_results_log[["cts_coeftest"]] <- coeftest(reg_4, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["kof_defacto"]] <- reg_5
trade_de_facto_results_log[["kof_defacto_coeftest"]] <- coeftest(reg_5, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["toi"]] <- reg_6
trade_de_facto_results_log[["toi_coeftest"]] <- coeftest(reg_6, vcov.=function(x) vcovHC(x, type="sss"))

# 1.3. FD specification -----

# Trade_to_GDP
open_var <- "Trade_to_GDP"
reg_1_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd") # , effect="twoways" not defined

# Alcala
open_var <- "Alcala"
reg_2_fd<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# Lietal
open_var <- "Lietal"
reg_3_fd<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# CTS
open_var <- "CTS"
reg_4_fd<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# KOF de facto
open_var <- "KOF_defacto"
reg_5_fd<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# TOI
open_var <- "TOI"
reg_6_fd<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# 1.4. List : annual trade de facto ----
trade_de_facto_results_log_fd <- list()
trade_de_facto_results_log_fd[["trade_to_GDP"]] <- reg_1_fd
trade_de_facto_results_log_fd[["trade_to_GDP_coeftest"]] <- coeftest(reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["alcala"]] <- reg_2_fd
trade_de_facto_results_log_fd[["alcala_coeftest"]] <- coeftest(reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["lietal"]] <- reg_3_fd
trade_de_facto_results_log_fd[["lietal_coeftest"]] <- coeftest(reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["cts"]] <- reg_4_fd
trade_de_facto_results_log_fd[["cts_coeftest"]] <- coeftest(reg_4_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["kof_defacto"]] <- reg_5_fd
trade_de_facto_results_log_fd[["kof_defacto_coeftest"]] <- coeftest(reg_5_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_fd[["toi"]] <- reg_6_fd
trade_de_facto_results_log_fd[["toi_coeftest"]] <- coeftest(reg_6_fd, vcov.=function(x) vcovHC(x, type="sss"))


# 2. Trade de facto - 5 year averages ----

# 2.1. Make 5y regressions ----

#trade to GDP
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(Trade_to_GDP)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#Alcala
reg_2<-plm(as.formula(paste0("GDP_pc_growth~log(Alcala)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#Lietal
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(Lietal)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#CTS
reg_4<-plm(as.formula(paste0("GDP_pc_growth~log(CTS)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#KOF de facto
reg_5<-plm(as.formula(paste0("GDP_pc_growth~log(KOF_defacto)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#TOI
reg_6<-plm(as.formula(paste0("GDP_pc_growth~log(TOI)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")


# 2.2. Make 5-year list : trade de facto ----
trade_de_facto_results_log_5y <- list()
trade_de_facto_results_log_5y[["trade_to_GDP"]] <- reg_1
trade_de_facto_results_log_5y[["trade_to_GDP_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["alcala"]] <- reg_2
trade_de_facto_results_log_5y[["alcala_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["lietal"]] <- reg_3
trade_de_facto_results_log_5y[["lietal_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["cts"]] <- reg_4
trade_de_facto_results_log_5y[["cts_coeftest"]] <- coeftest(reg_4, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["kof_defacto"]] <- reg_5
trade_de_facto_results_log_5y[["kof_defacto_coeftest"]] <- coeftest(reg_5, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log_5y[["toi"]] <- reg_6
trade_de_facto_results_log_5y[["toi_coeftest"]] <- coeftest(reg_6, vcov.=function(x) vcovHC(x, type="sss"))

# 3. Trade de jure - annual ----

# 3.1. Make annual regressions ----

#KOF de jure
open_var <- "KOF_dejure"
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

#FTI index
open_var <- "ln_FTI_Index_ipo"
reg_2<-plm(as.formula(paste0("GDP_pc_growth~ln_FTI_Index_ipo", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="within", effect="twoways")

# Tariff_WITS_ipo
open_var <- "Tariff_WITS_ipo"
reg_7<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# HF_trade
open_var <- "HF_trade"
reg_8<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# 3.2. Make annual reg list : trade de jure ----
trade_de_jure_results_log <- list()
trade_de_jure_results_log[["kof_dejure"]] <- reg_1
trade_de_jure_results_log[["kof_dejure_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["FTI"]] <- reg_2
trade_de_jure_results_log[["FTI_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["Tariff_WITS_ipo"]] <- reg_7
trade_de_jure_results_log[["Tariff_WITS_ipo_coeftest"]] <- coeftest(reg_7, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log[["HF_trade"]] <- reg_8
trade_de_jure_results_log[["HF_trade_coeftest"]] <- coeftest(reg_8, vcov.=function(x) vcovHC(x, type="sss"))

# 3.3. FD specification ----

#KOF de jure
open_var <- "KOF_dejure"
reg_1_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

#FTI index
open_var <- "ln_FTI_Index_ipo"
reg_2_fd <- plm(as.formula(paste0("GDP_pc_growth~", open_var, control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# Tariff_WITS_ipo
open_var <- "Tariff_WITS_ipo"
reg_7_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# HF_trade
open_var <- "HF_trade"
reg_8_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")


# 3.4. List: FD trade de jure ----
trade_de_jure_results_log_fd <- list()
trade_de_jure_results_log_fd[["kof_dejure"]] <- reg_1_fd
trade_de_jure_results_log_fd[["kof_dejure_coeftest"]] <- coeftest(reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["FTI"]] <- reg_2_fd
trade_de_jure_results_log_fd[["FTI_coeftest"]] <- coeftest(reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["Tariff_WITS_ipo"]] <- reg_7_fd
trade_de_jure_results_log_fd[["Tariff_WITS_ipo_coeftest"]] <- coeftest(reg_7_fd, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_fd[["HF_trade"]] <- reg_8_fd
trade_de_jure_results_log_fd[["HF_trade_coeftest"]] <- coeftest(reg_8_fd, vcov.=function(x) vcovHC(x, type="sss"))

# 4. Trade de jure - 5y ----

# 4.1. Make 5y regressions ----
#KOF de jure
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#FTI index
reg_2<-plm(as.formula(paste0("GDP_pc_growth~ln_FTI_Index_ipo", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

# Tariff_WITS_ipo
reg_7<-plm(as.formula(paste0("GDP_pc_growth~log(Tariff_WITS_ipo)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

# HF_trade
reg_8<-plm(as.formula(paste0("GDP_pc_growth~log(HF_trade)", control_vars_5y)), data=mutate(original_data_ext_5_year, HF_trade=ifelse(HF_trade==0.0, NA, HF_trade)), index = c("ccode","Year"), model="within", effect="individual")

# 4.2. Make 5 year reg list : trade de jure ----
trade_de_jure_results_log_5y <- list()
trade_de_jure_results_log_5y[["kof_dejure"]] <- reg_1
trade_de_jure_results_log_5y[["kof_dejure_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["FTI"]] <- reg_2
trade_de_jure_results_log_5y[["FTI_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["Tariff_WITS_ipo"]] <- reg_7
trade_de_jure_results_log_5y[["Tariff_WITS_ipo_coeftest"]] <- coeftest(reg_7, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_jure_results_log_5y[["HF_trade"]] <- reg_8
trade_de_jure_results_log_5y[["HF_trade_coeftest"]] <- coeftest(reg_8, vcov.=function(x) vcovHC(x, type="sss"))



# 5. Finance de facto - annual ----

# 5.1. Make annual regressions ----

#LMF open
open_var <- "LMF_open"
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

#LMF EQ
open_var <- "LMF_EQ"
reg_4<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

#FDI in
open_var <- "UNC_in_GDP"
reg_5<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

#FDI out
open_var <- "UNC_out_GDP"
reg_6<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# 5.2. Make annual reg list : finance de facto ----
finance_de_facto_results_log <- list()
finance_de_facto_results_log[["LMF_open"]] <- reg_3
finance_de_facto_results_log[["LMF_open_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["LMF_EQ"]] <- reg_4
finance_de_facto_results_log[["LMF_EQ_coeftest"]] <- coeftest(reg_4, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["UNC_in_GDP"]] <- reg_5
finance_de_facto_results_log[["UNC_in_GDP_coeftest"]] <- coeftest(reg_5, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log[["UNC_out_GDP"]] <- reg_6
finance_de_facto_results_log[["UNC_out_GDP_coeftest"]] <- coeftest(reg_6, vcov.=function(x) vcovHC(x, type="sss"))


# 5.3. FD specification ----

#LMF open
open_var <- "LMF_open"
reg_3_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

#LMF EQ
open_var <- "LMF_EQ"
reg_4_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

#FDI in
open_var <- "UNC_in_GDP"
reg_5_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

#FDI out
open_var <- "UNC_out_GDP"
reg_6_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# 5.2. List: FD finance de facto ----
finance_de_facto_results_log_fd <- list()
finance_de_facto_results_log_fd[["LMF_open"]] <- reg_3_fd
finance_de_facto_results_log_fd[["LMF_open_coeftest"]] <- coeftest(reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["LMF_EQ"]] <- reg_4_fd
finance_de_facto_results_log_fd[["LMF_EQ_coeftest"]] <- coeftest(reg_4_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["UNC_in_GDP"]] <- reg_5_fd
finance_de_facto_results_log_fd[["UNC_in_GDP_coeftest"]] <- coeftest(reg_5_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_fd[["UNC_out_GDP"]] <- reg_6_fd
finance_de_facto_results_log_fd[["UNC_out_GDP_coeftest"]] <- coeftest(reg_6_fd, vcov.=function(x) vcovHC(x, type="sss"))

# 6. Finance de facto - 5y ----

# 6.1. Make regressions ----

#LMF open
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(LMF_open)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#LMF EQ
reg_4<-plm(as.formula(paste0("GDP_pc_growth~log(LMF_EQ)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#FDI in
reg_5<-plm(as.formula(paste0("GDP_pc_growth~log(UNC_in_GDP)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#FDI out
reg_6<-plm(as.formula(paste0("GDP_pc_growth~log(UNC_out_GDP)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

#FDI out
reg_6<-plm(as.formula(paste0("GDP_pc_growth~log(UNC_out_GDP)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

# 6.2. Make 5y reg list ----

finance_de_facto_results_log_5y <- list()
finance_de_facto_results_log_5y[["LMF_open"]] <- reg_3
finance_de_facto_results_log_5y[["LMF_open_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["LMF_EQ"]] <- reg_4
finance_de_facto_results_log_5y[["LMF_EQ_coeftest"]] <- coeftest(reg_4, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["UNC_in_GDP"]] <- reg_5
finance_de_facto_results_log_5y[["UNC_in_GDP_coeftest"]] <- coeftest(reg_5, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_facto_results_log_5y[["UNC_out_GDP"]] <- reg_6
finance_de_facto_results_log_5y[["UNC_out_GDP_coeftest"]] <- coeftest(reg_6, vcov.=function(x) vcovHC(x, type="sss"))

# 7. Finance de jure - annual ----

# 7.1. Make annual regressions ----

# KAOPEN

open_var <- "KAOPEN"
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# HF_fin
open_var <- "HF_fin"
reg_2<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# CAPITAL
open_var <- "CAPITAL"
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="within", effect="twoways")

# 7.2. Annual reg list: finance de jure ----
finance_de_jure_results_log <- list()
finance_de_jure_results_log[["KAOPEN"]] <- reg_1
finance_de_jure_results_log[["KAOPEN_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log[["HF_fin"]] <- reg_2
finance_de_jure_results_log[["HF_fin_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log[["CAPITAL"]] <- reg_3
finance_de_jure_results_log[["CAPITAL_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))


# 7.3. FD specification ----

# KAOPEN
open_var <- "KAOPEN"
reg_1_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd") # KAOPEN geht nicht mit log

# HF_fin
open_var <- "HF_fin"
reg_2_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# CAPITAL
open_var <- "CAPITAL"
reg_3_fd <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)), data=filter_data(open_var), index = c("ccode","Year"), model="fd")

# 7.4. List: FD finance de jure ----
finance_de_jure_results_log_fd <- list()
finance_de_jure_results_log_fd[["KAOPEN"]] <- reg_1_fd
finance_de_jure_results_log_fd[["KAOPEN_coeftest"]] <- coeftest(reg_1_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_fd[["HF_fin"]] <- reg_2_fd
finance_de_jure_results_log_fd[["HF_fin_coeftest"]] <- coeftest(reg_2_fd, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_fd[["CAPITAL"]] <- reg_3_fd
finance_de_jure_results_log_fd[["CAPITAL_coeftest"]] <- coeftest(reg_3_fd, vcov.=function(x) vcovHC(x, type="sss"))


# 8. Finance de jure - 5y averages ----

# 8.1. Make 5y regressions ----

# KAOPEN
reg_1<-plm(as.formula(paste0("GDP_pc_growth~log(KAOPEN)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual") # KAOPEN geht nicht mit log

# HF_fin
reg_2<-plm(as.formula(paste0("GDP_pc_growth~log(HF_fin)", control_vars_5y)), data=mutate(original_data_ext_5_year, HF_fin=ifelse(HF_fin==0.0, NA, HF_fin)), index = c("ccode","Year"), model="within", effect="individual")

# CAPITAL
reg_3<-plm(as.formula(paste0("GDP_pc_growth~log(CAPITAL)", control_vars_5y)), data=mutate(original_data_ext_5_year, CAPITAL=ifelse(CAPITAL==0.0, NA, CAPITAL)), index = c("ccode","Year"), model="within", effect="individual")

# 8.2. 5 year reg list: finance de jure ----
finance_de_jure_results_log_5y <- list()
finance_de_jure_results_log_5y[["KAOPEN"]] <- reg_1
finance_de_jure_results_log_5y[["KAOPEN_coeftest"]] <- coeftest(reg_1, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_5y[["HF_fin"]] <- reg_2
finance_de_jure_results_log_5y[["HF_fin_coeftest"]] <- coeftest(reg_2, vcov.=function(x) vcovHC(x, type="sss"))

finance_de_jure_results_log_5y[["CAPITAL"]] <- reg_3
finance_de_jure_results_log_5y[["CAPITAL_coeftest"]] <- coeftest(reg_3, vcov.=function(x) vcovHC(x, type="sss"))


# 9. Complete specification ----


# Set up full regression with variables from all dimensions

# without WITS

full_reg_1_1y <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="within", effect="twoways")

full_reg_1_1y_fd <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="fd")

full_reg_1_5y <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

# with UNC_in

full_reg_1_1y_unc_in <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(UNC_in_GDP)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="within", effect="twoways")

full_reg_1_1y_unc_in_fd <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(UNC_in_GDP)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="fd")

full_reg_1_5y_unc_in <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(UNC_in_GDP)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

# with LMF_open

full_reg_1_1y_lmf_open <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(LMF_open)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="within", effect="twoways")

full_reg_1_1y_lmf_open_fd <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(LMF_open)", control_vars_1y)), data=original_data_ext, index = c("ccode","Year"), model="fd")

full_reg_1_5y_lmf_open <- plm(as.formula(paste0("GDP_pc_growth~log(KOF_dejure)+log(KOF_defacto)+log(KAOPEN)+log(LMF_open)", control_vars_5y)), data=original_data_ext_5_year, index = c("ccode","Year"), model="within", effect="individual")

full_spec <- list()
full_spec[["full_reg_1_1y"]] <- full_reg_1_1y
full_spec[["full_reg_1_1y_coeftest"]] <- coeftest(full_reg_1_1y, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_1y_fd"]] <- full_reg_1_1y_fd
full_spec[["full_reg_1_1y_fd_coeftest"]] <- coeftest(full_reg_1_1y_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_5y"]] <- full_reg_1_5y
full_spec[["full_reg_1_5y_coeftest"]] <- coeftest(full_reg_1_5y, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_1y_unc_in"]] <- full_reg_1_1y_unc_in
full_spec[["full_reg_1_1y_unc_in_coeftest"]] <- coeftest(full_reg_1_1y_unc_in, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_1y_unc_in_fd"]] <- full_reg_1_1y_unc_in_fd
full_spec[["full_reg_1_1y_unc_in_fd_coeftest"]] <- coeftest(full_reg_1_1y_unc_in_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_5y_unc_in"]] <- full_reg_1_5y_unc_in
full_spec[["full_reg_1_5y_unc_in_coeftest"]] <- coeftest(full_reg_1_5y_unc_in, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_1y_lmf_open"]] <- full_reg_1_1y_lmf_open
full_spec[["full_reg_1_5y_lmf_open_coeftest"]] <- coeftest(full_reg_1_1y_lmf_open, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_1y_lmf_open_fd"]] <- full_reg_1_1y_lmf_open_fd
full_spec[["full_reg_1_5y_lmf_open_fd_coeftest"]] <- coeftest(full_reg_1_1y_lmf_open_fd, vcov.=function(x) vcovHC(x, type="sss"))

full_spec[["full_reg_1_5y_lmf_open"]] <- full_reg_1_5y_lmf_open
full_spec[["full_reg_1_5y_lmf_open_coeftest"]] <- coeftest(full_reg_1_5y_lmf_open, vcov.=function(x) vcovHC(x, type="sss"))

# 10. Save regression files ----

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
saveRDS(regressions_list, "output/regressions.rds")


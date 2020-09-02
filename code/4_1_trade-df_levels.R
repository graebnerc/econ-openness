# Conductions regressions  ----------------------------------------------------

open_var <- "Trade_to_GDP"
reg_1 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# Alcala
open_var <- "Alcala"
reg_2 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# Lietal
open_var <- "Lietal"
reg_3 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# CTS
open_var <- "CTS"
reg_4 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# KOF de facto
open_var <- "KOF_defacto"
reg_5 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
), 
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# TOI
open_var <- "TOI"
reg_6 <- plm(as.formula(
  paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)
),
data=filter_data(open_var), 
index = c("ccode","Year"), 
model="within", 
effect="twoways")

# Adding robust standard errors -----------------------------------------------

trade_de_facto_results_log <- list()
trade_de_facto_results_log[["trade_to_GDP"]] <- reg_1
trade_de_facto_results_log[["trade_to_GDP_coeftest"]] <- coeftest(
  reg_1, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["alcala"]] <- reg_2
trade_de_facto_results_log[["alcala_coeftest"]] <- coeftest(
  reg_2, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["lietal"]] <- reg_3
trade_de_facto_results_log[["lietal_coeftest"]] <- coeftest(
  reg_3, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["cts"]] <- reg_4
trade_de_facto_results_log[["cts_coeftest"]] <- coeftest(
  reg_4, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["kof_defacto"]] <- reg_5
trade_de_facto_results_log[["kof_defacto_coeftest"]] <- coeftest(
  reg_5, vcov.=function(x) vcovHC(x, type="sss"))

trade_de_facto_results_log[["toi"]] <- reg_6
trade_de_facto_results_log[["toi_coeftest"]] <- coeftest(
  reg_6, vcov.=function(x) vcovHC(x, type="sss"))

# Save information about used data---------------------------------------------

data1y <- data.frame(table(index(reg_1$model)$ccode)) %>%
  dplyr::rename(Country=Var1, Observations=Freq) %>%
  dplyr::mutate(Country=countrycode(Country, "iso3c", "country.name"))

data1y <- rbind(data1y, 
                data.frame(Country="Total", 
                           "Observations"=sum(data1y$Observations)))
print(xtable(data1y, 
             label = "The countries used for the standard level regressions."), 
      type="html", file="output/dataset_1y.html", include.rownames=FALSE)

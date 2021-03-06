# Creates the figures for section 3 of the main paper and the corresponding appendix
# Correspondance: claudius@claudius-graebner.com


rm(list=ls())
# library(cowplot)
# library(haven)
# library(ggpubr)
# library(gridExtra)
# library(tidyverse)
# library(data.table)
# library(foreign)
# library(reshape)
# library(viridis)
source("code/2_make_trend_figures_functions.R")
# Set up data ----

reg_data_old <- readRDS("data/regdata.rds")
openess_complete <- reg_data %>% 
  dplyr::mutate(Country=countrycode::countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::mutate(Alcala=Alcala/1000000)

reg_data <- fread(here("data/openness_1y.csv")) %>% 
  dplyr::mutate(Country=countrycode::countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::mutate(Alcala=Alcala/1000000)

reg_data <- fread(here("data/openness_1y.csv"))

# Section 3: Trends ----



##### MAIN PAPER #####
selected_group <- "ComplexityGroup"
# Fig 3.1: Trade openness ----

trade_gdp <- get_plot(y_var_code = "Trade_to_GDP", 
                       title_string = "Trade as % of GDP", 
                       y_label = "Per cent of GDP", 
                       x_label = "Year", 
                       kind_group = selected_group)

trade_gdp

#  WITS
trade_wits <- get_plot(y_var_code = "Tariff_WITS", 
                       title_string = "WITS-based index", 
                       y_label = "100-tariff rate", 
                       x_label = "Year", 
                       kind_group = selected_group)
trade_wits

# Mitte rechts: Alcala

trade_alcala <- get_plot(y_var_code = "Alcala", 
                         title_string = "Real trade share", 
                         y_label = "Trade to GDP (in billion PPP)", 
                         x_label = "Year", 
                         kind_group = selected_group)
trade_alcala

trade_lietal <- get_plot(y_var_code = "Lietal", 
                         title_string = "Adjusted trade share", 
                         y_label = "Adjusted trade share", 
                         x_label = "Year", 
                         kind_group = selected_group)
trade_lietal

# FTI

# trade_fti <- get_plot(y_var_code = "FTI_Index", 
#                       title_string = "Freedom to Trade Internationally Index (Fraser Institute)", 
#                       y_label = "Freedom to Trade Internationally Index", 
#                       x_label = "Year", 
#                       kind_group = selected_group)
# trade_fti
# 
# 
# # HF_trade
# 
# trade_hf <- get_plot(y_var_code = "HF_trade", 
#                      title_string = "Trade Freedom Index (Heritage Foundation)", 
#                      y_label = "Trade Freedom Index", 
#                      x_label = "Year", 
#                      kind_group = selected_group)
# trade_hf


# Final plot ----

full_plot_trade_openness <- ggarrange(ggarrange(trade_gdp + theme(legend.position = "none"), trade_lietal + theme(legend.position = "none"), ncol = 2, labels=c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                      ggarrange(trade_alcala, trade_wits, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "bottom"), 
                                      nrow = 2) 

filename <- "output/trend_figures/fig_3_1_trade.pdf"
ggsave(filename, plot = full_plot_trade_openness, width = 10, height = 8)


# Fig 3.2: Financial openness ----

# Reihe 1: OUtward/inward FDI in % of GDP

inward_fdi_gdp <- get_plot(y_var_code = "UNC_in_GDP", 
                           title_string = "Inward FDI stocks (% GDP)", 
                           y_label = "Inward FDI stocks in % GDP", 
                           x_label = "Year", 
                           kind_group = selected_group)
inward_fdi_gdp

outward_fdi_gdp <- get_plot(y_var_code = "UNC_out_GDP", 
                            title_string = "Outward FDI stocks (% GDP)", 
                            y_label = "Outward FDI stocks in % GDP", 
                            x_label = "Year", 
                            kind_group = selected_group)
outward_fdi_gdp


# Reihe 2: LMF_open und die Variante mit (OPEN_pv)

# LMF Open

finance_lmf_open <- get_plot(y_var_code = "LMF_open", 
                             title_string = "Foreign assets and liabilities (LMF_open)", 
                             y_label = "Stocks in % of GDP", 
                             x_label = "Year", 
                             kind_group = selected_group)
finance_lmf_open

# LMF_EQ

finance_lmf_eq <- get_plot(y_var_code = "LMF_EQ", 
                             title_string = "Portfolio Equity Assets and Liabilities (LMF_EQ)", 
                             y_label = "Stocks in % of GDP", 
                             x_label = "Year", 
                             kind_group = selected_group)
finance_lmf_eq

# Reihe 3: Chin-Ito und CAPITAL

# KAOPEN

finance_kaopen <- get_plot(y_var_code = "KAOPEN", 
                           title_string = "Chinn-Ito Index of capital account openness", 
                           y_label = "Chinn-Ito Index", 
                           x_label = "Year", 
                           kind_group = selected_group)
finance_kaopen

# CAPITAL

finance_capital <- get_plot(y_var_code = "CAPITAL", 
                            title_string = "Capital Account Liberalization ", 
                            y_label = "CAPITAL", 
                            x_label = "Year", 
                            kind_group = selected_group)
finance_capital




# Outward FDI stocks

finance_fdi <- get_plot(y_var_code = "UNC_out_GDP", 
                        title_string = "Outward FDI stocks", 
                        y_label = "Outward FDI stocks in % GDP", 
                        x_label = "Year", 
                        kind_group = selected_group)
finance_fdi

# Inward FDI stocks



inward_fdi <- get_plot(y_var_code = "UNC_FDI_in", 
                           title_string = "Inward FDI stocks", 
                           y_label = "Inward FDI stocks in % GDP", 
                           x_label = "Year", 
                           kind_group = selected_group)
inward_fdi

outward_fdi <- get_plot(y_var_code = "UNC_FDI_out", 
                            title_string = "Outward FDI stocks", 
                            y_label = "Outward FDI stocks", 
                            x_label = "Year", 
                            kind_group = selected_group)
outward_fdi

# Final plot ----

full_plot_financial_openness <- ggarrange(ggarrange(inward_fdi_gdp, outward_fdi_gdp, ncol = 2, labels = c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                          ggarrange(finance_lmf_open, finance_lmf_eq, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "none"), 
                                          ggarrange(finance_kaopen, finance_capital, ncol = 2, labels = c("E)", "F)"), common.legend = TRUE, legend = "bottom"), 
                                          nrow = 3) 

full_plot_financial_openness <- ggarrange(ggarrange(inward_fdi_gdp, outward_fdi_gdp, ncol = 2, labels = c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                          ggarrange(finance_lmf_open, finance_kaopen, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "bottom"), 
                                          nrow = 2) 

filename <- "output/trend_figures/fig_3_2_finance.pdf"
ggsave(filename, plot = full_plot_financial_openness, width = 10, height = 8)


# 3.3: KOF globalization index ----

# KOF_total

kof_total <- get_plot(y_var_code = "KOF_econ", 
                      title_string = "KOF Econ Globalization Index - complete version", 
                      y_label = "KOF Index", 
                      x_label = "Year", 
                      kind_group = selected_group)
kof_total

# KOF de facto

kof_defacto <- get_plot(y_var_code = "KOF_defacto", 
                        title_string = "KOF Econ Globalization Index - de facto", 
                        y_label = "KOF Index", 
                        x_label = "Year", 
                        kind_group = selected_group)
kof_defacto

# KOF de jure

kof_dejure <- get_plot(y_var_code = "KOF_dejure", 
                       title_string = "KOF Econ Globalization Index - de jure", 
                       y_label = "KOF Index", 
                       x_label = "Year", 
                       kind_group = selected_group)
kof_dejure

# KOF trade de facto 

kof_trade_defacto <- get_plot(y_var_code = "KOF_trade_df", 
                       title_string = "KOF Trade Globalization Index - de facto", 
                       y_label = "KOF Index", 
                       x_label = "Year", 
                       kind_group = selected_group)
kof_trade_defacto

# KOF trade de jure

kof_trade_dejure <- get_plot(y_var_code = "KOF_trade_dj", 
                              title_string = "KOF Trade Globalization Index - de jure", 
                              y_label = "KOF Index", 
                              x_label = "Year", 
                              kind_group = selected_group)
kof_trade_dejure

# KOF finance de facto 

kof_finance_defacto <- get_plot(y_var_code = "KOF_finance_df", 
                              title_string = "KOF Financial Globalization Index - de facto", 
                              y_label = "KOF Index", 
                              x_label = "Year", 
                              kind_group = selected_group)
kof_finance_defacto

# KOF finance de jure

kof_finance_dejure <- get_plot(y_var_code = "KOF_finance_dj", 
                             title_string = "KOF Financial Globalization Index - de jure", 
                             y_label = "KOF Index", 
                             x_label = "Year", 
                             kind_group = selected_group)
kof_finance_dejure


# Final plot ----

full_plot_kof <- ggarrange(kof_total, 
                           ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "none"),
                           ggarrange(kof_trade_defacto, kof_trade_dejure, ncol = 2, labels = c("D)", "E)"), common.legend = TRUE, legend = "none"),
                           ggarrange(kof_finance_defacto, kof_finance_dejure, ncol = 2, labels = c("F)", "G)"), common.legend = TRUE, legend = "bottom"),
                           nrow = 4, 
                           labels = "A)", common.legend = TRUE, legend = "none"
) 

filename <- "output/trend_figures/fig_3_3_hybrid.pdf"
ggsave(filename, plot = full_plot_kof, width = 10, height = 12)

full_plot_kof_red <- ggarrange(kof_total, 
                           ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "none"),
                           ggarrange(kof_trade_defacto, kof_trade_dejure, ncol = 2, labels = c("D)", "E)"), common.legend = TRUE, legend = "none"),
                           nrow = 3, 
                           labels = "A)", common.legend = TRUE, legend = "bottom"
) 

filename <- "output/trend_figures/fig_3_3_hybrid_red.pdf"
ggsave(filename, plot = full_plot_kof_red, width = 10, height = 8)

full_plot_kof_red <- ggarrange(kof_total, 
                               ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "none"),
                               nrow = 2, 
                               labels = "A)", common.legend = TRUE, legend = "bottom"
) 

filename <- "output/trend_figures/fig_3_3_hybrid_red_red.pdf"
ggsave(filename, plot = full_plot_kof_red, width = 10, height = 6)

##### APPENDIX ####
selected_group <- "IncomeGroup"
# Fig 3.1: Trade openness ----

trade_gdp <- get_plot(y_var_code = "Trade_to_GDP", 
                      title_string = "Trade as % of GDP", 
                      y_label = "Per cent of GDP", 
                      x_label = "Year", 
                      kind_group = selected_group)

trade_gdp

#  WITS
trade_wits <- get_plot(y_var_code = "Tariff_WITS", 
                       title_string = "WITS-based index", 
                       y_label = "100-tariff rate", 
                       x_label = "Year", 
                       kind_group = selected_group)
trade_wits

# Mitte rechts: Alcala

trade_alcala <- get_plot(y_var_code = "Alcala", 
                         title_string = "Real trade share", 
                         y_label = "Trade to GDP (in billion PPP)", 
                         x_label = "Year", 
                         kind_group = selected_group)
trade_alcala

trade_lietal <- get_plot(y_var_code = "Lietal", 
                         title_string = "Adjusted trade share", 
                         y_label = "Adjusted trade share", 
                         x_label = "Year", 
                         kind_group = selected_group)
trade_lietal

# FTI

# trade_fti <- get_plot(y_var_code = "FTI_Index", 
#                       title_string = "Freedom to Trade Internationally Index (Fraser Institute)", 
#                       y_label = "Freedom to Trade Internationally Index", 
#                       x_label = "Year", 
#                       kind_group = selected_group)
# trade_fti
# 
# 
# # HF_trade
# 
# trade_hf <- get_plot(y_var_code = "HF_trade", 
#                      title_string = "Trade Freedom Index (Heritage Foundation)", 
#                      y_label = "Trade Freedom Index", 
#                      x_label = "Year", 
#                      kind_group = selected_group)
# trade_hf


# Final plot ----

full_plot_trade_openness <- ggarrange(ggarrange(trade_gdp + theme(legend.position = "none"), trade_lietal + theme(legend.position = "none"), ncol = 2, labels=c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                      ggarrange(trade_alcala, trade_wits, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "bottom"), 
                                      nrow = 2) 

filename <- "output/trend_figures/fig_A_1_trade.pdf"
ggsave(filename, plot = full_plot_trade_openness, width = 10, height = 8)


# Fig 3.2: Financial openness ----

# Reihe 1: OUtward/inward FDI in % of GDP

inward_fdi_gdp <- get_plot(y_var_code = "UNC_in_GDP", 
                           title_string = "Inward FDI stocks (% GDP)", 
                           y_label = "Inward FDI stocks in % GDP", 
                           x_label = "Year", 
                           kind_group = selected_group)
inward_fdi_gdp

outward_fdi_gdp <- get_plot(y_var_code = "UNC_out_GDP", 
                            title_string = "Outward FDI stocks (% GDP)", 
                            y_label = "Outward FDI stocks in % GDP", 
                            x_label = "Year", 
                            kind_group = selected_group)
outward_fdi_gdp


# Reihe 2: LMF_open und die Variante mit (OPEN_pv)

# LMF Open

finance_lmf_open <- get_plot(y_var_code = "LMF_open", 
                             title_string = "Foreign assets and liabilities (LMF_open)", 
                             y_label = "Stocks in % of GDP", 
                             x_label = "Year", 
                             kind_group = selected_group)
finance_lmf_open

# LMF_EQ

finance_lmf_eq <- get_plot(y_var_code = "LMF_EQ", 
                           title_string = "Portfolio Equity Assets and Liabilities (LMF_EQ)", 
                           y_label = "Stocks in % of GDP", 
                           x_label = "Year", 
                           kind_group = selected_group)
finance_lmf_eq

# Reihe 3: Chin-Ito und CAPITAL

# KAOPEN

finance_kaopen <- get_plot(y_var_code = "KAOPEN", 
                           title_string = "Chinn-Ito Index of capital account openness", 
                           y_label = "Chinn-Ito Index", 
                           x_label = "Year", 
                           kind_group = selected_group)
finance_kaopen

# CAPITAL

finance_capital <- get_plot(y_var_code = "CAPITAL", 
                            title_string = "Capital Account Liberalization ", 
                            y_label = "CAPITAL", 
                            x_label = "Year", 
                            kind_group = selected_group)
finance_capital




# Outward FDI stocks

finance_fdi <- get_plot(y_var_code = "UNC_out_GDP", 
                        title_string = "Outward FDI stocks", 
                        y_label = "Outward FDI stocks in % GDP", 
                        x_label = "Year", 
                        kind_group = selected_group)
finance_fdi

# Inward FDI stocks



inward_fdi <- get_plot(y_var_code = "UNC_FDI_in", 
                       title_string = "Inward FDI stocks", 
                       y_label = "Inward FDI stocks in % GDP", 
                       x_label = "Year", 
                       kind_group = selected_group)
inward_fdi

outward_fdi <- get_plot(y_var_code = "UNC_FDI_out", 
                        title_string = "Outward FDI stocks", 
                        y_label = "Outward FDI stocks", 
                        x_label = "Year", 
                        kind_group = selected_group)
outward_fdi

full_fdi_plot <- ggarrange(ggarrange(inward_fdi_gdp, outward_fdi_gdp, ncol = 2, labels = c("A)", "B)"), common.legend = TRUE, legend = "none"),
                           ggarrange(inward_fdi, outward_fdi, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "bottom"), 
                           nrow = 2) 

# filename <- "output/new_figs/fdi_measures.pdf"
# ggsave(filename, plot = full_fdi_plot, width = 10, height = 8)
# Final plot ----

full_plot_financial_openness <- ggarrange(ggarrange(inward_fdi_gdp, outward_fdi_gdp, ncol = 2, labels = c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                          ggarrange(finance_lmf_open, finance_lmf_eq, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "none"), 
                                          ggarrange(finance_kaopen, finance_capital, ncol = 2, labels = c("E)", "F)"), common.legend = TRUE, legend = "bottom"), 
                                          nrow = 3) 

full_plot_financial_openness <- ggarrange(ggarrange(inward_fdi_gdp, outward_fdi_gdp, ncol = 2, labels = c("A)", "B)"), common.legend = TRUE, legend = "none"),
                                          ggarrange(finance_lmf_open, finance_kaopen, ncol = 2, labels = c("C)", "D)"), common.legend = TRUE, legend = "bottom"), 
                                          nrow = 2) 

filename <- "output/trend_figures/fig_A_2_finance.pdf"
ggsave(filename, plot = full_plot_financial_openness, width = 10, height = 8)


# 3.3: KOF globalization index ----

# KOF_total

kof_total <- get_plot(y_var_code = "KOF_econ", 
                      title_string = "KOF Econ Globalization Index - complete version", 
                      y_label = "KOF Index", 
                      x_label = "Year", 
                      kind_group = selected_group)
kof_total

# KOF de facto

kof_defacto <- get_plot(y_var_code = "KOF_defacto", 
                        title_string = "KOF Econ Globalization Index - de facto", 
                        y_label = "KOF Index", 
                        x_label = "Year", 
                        kind_group = selected_group)
kof_defacto

# KOF de jure

kof_dejure <- get_plot(y_var_code = "KOF_dejure", 
                       title_string = "KOF Econ Globalization Index - de jure", 
                       y_label = "KOF Index", 
                       x_label = "Year", 
                       kind_group = selected_group)
kof_dejure

# KOF trade de facto 

kof_trade_defacto <- get_plot(y_var_code = "KOF_trade_df", 
                              title_string = "KOF Trade Globalization Index - de facto", 
                              y_label = "KOF Index", 
                              x_label = "Year", 
                              kind_group = selected_group)
kof_trade_defacto

# KOF trade de jure

kof_trade_dejure <- get_plot(y_var_code = "KOF_trade_dj", 
                             title_string = "KOF Trade Globalization Index - de jure", 
                             y_label = "KOF Index", 
                             x_label = "Year", 
                             kind_group = selected_group)
kof_trade_dejure

# KOF finance de facto 

kof_finance_defacto <- get_plot(y_var_code = "KOF_finance_df", 
                                title_string = "KOF Financial Globalization Index - de facto", 
                                y_label = "KOF Index", 
                                x_label = "Year", 
                                kind_group = selected_group)
kof_finance_defacto

# KOF finance de jure

kof_finance_dejure <- get_plot(y_var_code = "KOF_finance_dj", 
                               title_string = "KOF Financial Globalization Index - de jure", 
                               y_label = "KOF Index", 
                               x_label = "Year", 
                               kind_group = selected_group)
kof_finance_dejure


# Final plot ----

full_plot_kof <- ggarrange(kof_total, 
                           ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "none"),
                           ggarrange(kof_trade_defacto, kof_trade_dejure, ncol = 2, labels = c("D)", "E)"), common.legend = TRUE, legend = "none"),
                           ggarrange(kof_finance_defacto, kof_finance_dejure, ncol = 2, labels = c("F)", "G)"), common.legend = TRUE, legend = "bottom"),
                           nrow = 4, 
                           labels = "A)", common.legend = TRUE, legend = "none"
) 

filename <- "output/trend_figures/fig_A_3_hybrid.pdf"
ggsave(filename, plot = full_plot_kof, width = 10, height = 12)

full_plot_kof_red <- ggarrange(kof_total, 
                               ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "none"),
                               ggarrange(kof_trade_defacto, kof_trade_dejure, ncol = 2, labels = c("D)", "E)"), common.legend = TRUE, legend = "bottom"),
                               nrow = 3, 
                               labels = "A)", common.legend = TRUE, legend = "none"
) 

filename <- "output/trend_figures/fig_A_3_hybrid_red.pdf"
ggsave(filename, plot = full_plot_kof_red, width = 10, height = 8)

full_plot_kof_red <- ggarrange(kof_total, 
                               ggarrange(kof_defacto, kof_dejure, ncol = 2, labels = c("B)", "C)"), common.legend = TRUE, legend = "bottom"),
                               nrow = 2, 
                               labels = "A)", common.legend = TRUE, legend = "none"
) 

filename <- "output/trend_figures/fig_A_3_hybrid_red_red.pdf"
ggsave(filename, plot = full_plot_kof_red, width = 10, height = 6)

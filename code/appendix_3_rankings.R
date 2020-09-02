rm(list = ls())
library(tidyverse)
library(data.table)
library(xtable)
library(here)
library(plm)
library(countrycode)

classification_variables <- c("ComplexityGroup", "IncomeGroup", "period")

openess_complete <- fread(here("data/openness_1y.csv")) %>%
  dplyr::select(-one_of(classification_variables)) 


# Full data for rankings in the appendix ----
trade_data <- openess_complete %>%
  dplyr::select(one_of("ccode", "Year", "Trade_to_GDP", 
                       "Tariff_WITS", "KOF_defacto", "KOF_dejure")
                ) %>%
  dplyr::mutate(
    Country=countrycode::countrycode(ccode, "iso3c", "country.name")
    ) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::select(-ccode) %>%
  dplyr::filter(Year==2015) %>%
  dplyr::mutate(
    T2GDP_rank=rank(-Trade_to_GDP, ties.method = "average"),
    T2GDP_rank_max=max(T2GDP_rank),
    WITS_rank=rank(-Tariff_WITS, ties.method = "average"),
    WITS_rank_max=max(WITS_rank),
    KOF_df_rank=rank(-KOF_defacto, ties.method = "average"),
    KOF_df_rank_max=max(KOF_df_rank),
    KOF_dj_rank=rank(-KOF_dejure, ties.method = "average"),
    KOF_dj_rank_max=max(KOF_dj_rank)) %>%
  dplyr::mutate(diff_t2gdp_wits=T2GDP_rank-WITS_rank,
                diff_kof=KOF_df_rank-KOF_dj_rank)

# Single ranking tables for the appendix ----

# Trade2GDP
t_gdp_tab <- trade_data %>%
  dplyr::select(one_of("Country", "T2GDP_rank")) %>%
  dplyr::rename(Rank=T2GDP_rank) %>%
  dplyr::filter(Rank<11 | Rank>max(Rank)-11) %>%
  dplyr::arrange(Rank)
t_gdp_tab

latex_file <- here("appendix/trade_defacto_ranking.tex")
t_gdp_tab_tex <- xtable(t_gdp_tab, align = "ccc", digits = 0)
print.xtable(
  x = t_gdp_tab_tex, file = latex_file, floating = FALSE,
  tabular.environment="tabular", include.rownames = FALSE, 
  booktabs = TRUE, caption.placement = "top")

# WITS
wits_tab <- trade_data %>%
  dplyr::select(one_of("Country", "WITS_rank")) %>% 
  dplyr::rename(Rank=WITS_rank) %>%
  dplyr::filter(Rank<11 | Rank>max(Rank)-11) %>%
  dplyr::arrange(Rank)
wits_tab

latex_file <- here("appendix/wits_ranking.tex")
wits_tab_tex <- xtable(wits_tab, align = "ccc", digits = 0)
print.xtable(x = wits_tab_tex, file = latex_file, floating = FALSE,
             tabular.environment="tabular", include.rownames = FALSE, booktabs = TRUE,
             caption.placement = "bottom", caption = "FUCK")

# KOF De facto
kof_df_tab <- trade_data %>%
  dplyr::select(one_of("Country", "KOF_df_rank")) %>% 
  dplyr::rename(Rank=KOF_df_rank) %>%
  dplyr::filter(Rank<11 | Rank>max(Rank)-11) %>%
  dplyr::arrange(Rank)
kof_df_tab

latex_file <- here("appendix/kof_defacto_ranking.tex")
kof_df_tab_tex <- xtable(kof_df_tab, align = "ccc", digits = 0)
print.xtable(x = kof_df_tab_tex, file = latex_file, floating = FALSE,
             tabular.environment="tabular", include.rownames = FALSE, 
             booktabs = TRUE, caption.placement = "top")

# KOF De jure
kof_dj_tab <- trade_data %>%
  dplyr::select(one_of("Country", "KOF_dj_rank")) %>% 
  dplyr::rename(Rank=KOF_dj_rank) %>%
  dplyr::filter(Rank<11 | Rank>max(Rank)-11) %>%
  dplyr::arrange(Rank)
kof_dj_tab

latex_file <- here("appendix/kof_dejure_ranking.tex")
kof_dj_tab_tex <- xtable(kof_dj_tab, align = "ccc", digits = 0)
print.xtable(x = kof_dj_tab_tex, file = latex_file, floating = FALSE,
             tabular.environment="tabular", include.rownames = FALSE, 
             booktabs = TRUE, caption.placement = "top")

# Plots von Differenzen
# These plots were created by Mathematica
kof_diff <- trade_data %>%
  dplyr::select(one_of("Country", "diff_kof")) %>%
  dplyr::arrange(diff_kof) 
fwrite(kof_diff, path = here("appendix/appendix/kof_diff_data.csv"))

trade_diff <- trade_data %>%
  dplyr::select(one_of("Country", "diff_t2gdp_wits")) %>%
  dplyr::arrange(diff_t2gdp_wits) 
fwrite(trade_diff, path = here("appendix/appendix/trade_diff_data.csv"))


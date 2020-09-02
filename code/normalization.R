trade_gdp_test_dat <- openness_data_v4 %>%
  select(one_of("ccode", "Year","rgdpo", "population", "Trade_to_GDP")) %>%
  dplyr::mutate(trade_gdp=(Trade_to_GDP*rgdpo)/rgdpo,
                trade_pc=(Trade_to_GDP*rgdpo)/population,
                country=countrycode(ccode, "iso3c", "country.name"),
                gdp_per_cap=rgdpo/population) %>%
  dplyr::filter(Year>2010,
                !is.na(Trade_to_GDP),
                !is.na(rgdpo))
head(trade_gdp_test_dat)

plot_1 <- ggplot(trade_gdp_test_dat, aes(y=trade_gdp, x=trade_pc)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat, Year==2011), 
            aes(x=trade_pc, y=trade_gdp, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  ylab("Trade to GDP") + xlab("Trade per capita") +
  ggtitle("Trade/GDP vs. Trade per capita") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_1


plot_2 <- ggplot(trade_gdp_test_dat, aes(x=population, y=trade_gdp)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat, Year==2011), 
            aes(x=population, y=trade_gdp, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Population") + ylab("Trade/GDP") +
  ggtitle("Trade/GDP vs. population") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_2

plot_3 <- ggplot(trade_gdp_test_dat, aes(x=population, y=trade_pc)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat, Year==2011), 
            aes(x=population, y=trade_pc, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Population") + ylab("Trade per capita") +
  ggtitle("Trade per capita vs. population") +
theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_3

plot_4 <- ggplot(trade_gdp_test_dat, aes(x=trade_gdp, y=gdp_per_cap)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat, Year==2011), 
            aes(x=trade_gdp, y=gdp_per_cap, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Trade/GDP") + ylab("GDP per capita") +
  ggtitle("Trade/GDP vs. GDP per capita") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_4


##
trade_gdp_test_dat_red <- dplyr::filter(trade_gdp_test_dat, !country %in% c("India", "China"))
plot_1_red <- ggplot(trade_gdp_test_dat_red, aes(y=trade_gdp, x=trade_pc)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat_red, Year==2011), 
            aes(x=trade_pc, y=trade_gdp, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  ylab("Trade to GDP") + xlab("Trade per capita") +
  ggtitle("Trade/GDP vs. Trade per capita") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_1_red


plot_2_red <- ggplot(trade_gdp_test_dat_red, aes(x=population, y=trade_gdp)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat_red, Year==2011), 
            aes(x=population, y=trade_gdp, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Population") + ylab("Trade/GDP") +
  ggtitle("Trade/GDP vs. population") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_2_red

plot_3_red <- ggplot(trade_gdp_test_dat_red, aes(x=population, y=trade_pc)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat_red, Year==2011), 
            aes(x=population, y=trade_pc, label=country, color=country)) + 
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Population") + ylab("Trade per capita") +
  ggtitle("Trade per capita vs. population") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line(),
                     legend.position = "none")
plot_3_red

plot_4_red <- ggplot(trade_gdp_test_dat_red, aes(x=trade_gdp, y=gdp_per_cap)) +
  geom_point(aes(color=country)) + 
  geom_text(data=dplyr::filter(trade_gdp_test_dat_red, Year==2011), 
            aes(x=trade_gdp, y=gdp_per_cap, label=country, color=country)) +   
  geom_smooth(method="loess") + geom_smooth(method = "lm", se = FALSE) +
  xlab("Trade/GDP") + ylab("GDP per capita") +
  ggtitle("Trade/GDP vs. GDP per capita") +
  theme_bw() + theme(panel.border = element_blank(), axis.line = element_line())
plot_4_red

full_plot <- ggpubr::ggarrange(plot_1, plot_2, plot_3, plot_4,
                               plot_1_red, plot_2_red, plot_3_red, plot_4_red,
                               ncol=4, nrow = 2, legend = "none")
ggsave("output/normalization.pdf", plot = full_plot, width = 40, height = 24)


ranking_dat <- trade_gdp_test_dat_red %>%
  group_by(country) %>%
  summarise_if(is.numeric, mean, na.rm=T) %>%
  ungroup() %>%
  dplyr::select(-Year) %>%
  dplyr::mutate(rank_gdp = dense_rank(desc(rgdpo)),
                rank_gdp_pc = dense_rank(desc(gdp_per_cap)),
                rank_pop = dense_rank(desc(population)),
                rank_trade2GDP = dense_rank(desc(trade_gdp)),
                rank_trade_pc = dense_rank(desc(trade_pc)),
                trade2GDP.vs.trade_pc = rank_trade2GDP-rank_trade_pc)
head(ranking_dat)



reg_plot <- ggplot(ranking_dat, aes(x=rank_trade2GDP, y=rank_trade_pc)) +
  geom_point(aes(color=country)) +
  geom_text(aes(color=country, label=country)) +
  xlab("Rank of trade/GDP") + ylab("Rank of trade per capita") + 
  ggtitle("Ranking comparison") +
  geom_smooth(method="lm") + geom_smooth(method="loess") +
  theme_bw() + theme(panel.border = element_blank(), 
                     axis.line = element_line(),
                     legend.position = "none")
reg_plot
ggsave("output/ranking-correl.pdf", plot = reg_plot, width = 12, height = 12)


bar_plot <- ggplot(ranking_dat) + 
  geom_bar(aes(x=reorder(country, -trade2GDP.vs.trade_pc), y=trade2GDP.vs.trade_pc, fill=trade2GDP.vs.trade_pc>0), stat = "identity") +
  geom_text(aes(x=reorder(country, -trade2GDP.vs.trade_pc), 
                y=0, label=country, angle=90), size=5) +
  theme_bw() + ylab("Rank Trade/GDP - Rank trade per capita") +
  theme(axis.text.x = element_blank()) +
  theme(panel.border = element_blank(), 
        axis.line = element_line(),
        legend.position = "none",
        axis.title.x = element_blank())
bar_plot
ggsave("output/diff-ranks-plot.pdf", plot = bar_plot, width = 36, height = 12)

diff_pop_plot <- ggplot(ranking_dat) + 
  geom_point(aes(x=population, y=trade2GDP.vs.trade_pc, color=country)) +
  geom_text(aes(x=population, y=trade2GDP.vs.trade_pc, color=country, label=country)) + 
  geom_smooth(aes(x=population, y=trade2GDP.vs.trade_pc), method = "loess") + 
  geom_smooth(aes(x=population, y=trade2GDP.vs.trade_pc), method = "lm") + 
  theme(panel.border = element_blank(), 
        axis.line = element_line(),
        legend.position = "none")
diff_pop_plot


# -----
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

reg_data <- readRDS("data/sec5_regdata.rds")

original_data_ext <- reg_data$original_data_ext %>%
  #dplyr::mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(#!is.na(Country),
    hc>0,
    inv_share>0) %>%
  dplyr::mutate(trade_gdp=(Trade_to_GDP*rgdpo)/rgdpo,
                trade_pc=(Trade_to_GDP*rgdpo)/population,
                country=countrycode(ccode, "iso3c", "country.name"),
                gdp_per_cap=rgdpo/population)

original_data_ext$Country <- countrycode(original_data_ext$ccode, 'iso3c', 'country.name') #convert OECD name codes to three letter country codes (iso3n)

original_data_ext_5_year <- reg_data$original_data_ext_5_year %>%
  #dplyr::mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(#!is.na(Country),
    hc>0,
    initial_GDP_pc>0,
    inv_share>0) %>%
  dplyr::mutate(trade_gdp=(Trade_to_GDP*rgdpo)/rgdpo,
                trade_pc=(Trade_to_GDP*rgdpo)/population,
                country=countrycode(ccode, "iso3c", "country.name"),
                gdp_per_cap=rgdpo/population)

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

new_results_log <- list()
open_var <- "Trade_to_GDP"
new_results_log[[paste0(open_var, "_1y")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                           data=filter_data(open_var), 
                                           index = c("ccode","Year"), 
                                           model="within", effect="twoways")
new_results_log[[paste0(open_var, "_1y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_1y")]], 
                                                             vcov.=function(x) vcovHC(x, type="sss"))

open_var <- "trade_pc"
new_results_log[[paste0(open_var, "_1y")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                           data=filter_data(open_var), 
                                           index = c("ccode","Year"), 
                                           model="within", effect="twoways")
new_results_log[[paste0(open_var, "_1y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_1y")]], 
                                                             vcov.=function(x) vcovHC(x, type="sss"))

# Full spec
full_spec_l1 <- as.formula(paste0("GDP_pc_growth~log(KOF_econ)", 
                                  control_vars_1y))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_1y))

open_var <- "KOF_econ"
new_results_log[[paste0(open_var, "_1y")]]<- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                                 data=filter_data(open_var), 
                                                 index = c("ccode","Year"), 
                                                 model="within", effect="twoways")
new_results_log[[paste0(open_var, "_1y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_1y")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_1y))
new_results_log[["KOF_trade_df_1y"]] <- plm(full_spec_l3_kofs, data=original_data_ext, index = c("ccode","Year"), 
                           model="within", effect="twoways")
new_results_log[[paste0("KOF_trade_df_1y", "_1y_coeftest")]] <- coeftest(new_results_log[["KOF_trade_df_1y"]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_gdp <- as.formula(paste0("GDP_pc_growth~log(Trade_to_GDP)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                 control_vars_1y))
new_results_log[["Trade_to_GDP_1y_full"]] <- plm(full_spec_l3_kofs_trade_gdp, data=original_data_ext, index = c("ccode","Year"), 
                                         model="within", effect="twoways")
new_results_log[[paste0("Trade_to_GDP_full", "_1y_coeftest")]] <- coeftest(new_results_log[["Trade_to_GDP_1y_full"]], 
                                                                      vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_pc <- as.formula(paste0("GDP_pc_growth~log(trade_pc)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                control_vars_1y))
new_results_log[["trade_pc_1y_full"]] <- plm(full_spec_l3_kofs_trade_pc, data=original_data_ext, index = c("ccode","Year"), 
                                              model="within", effect="twoways")
new_results_log[[paste0("trade_pc_full", "_1y_coeftest")]] <- coeftest(new_results_log[["trade_pc_1y_full"]], 
                                                                           vcov.=function(x) vcovHC(x, type="sss"))

## 5 years ----
open_var <- "Trade_to_GDP"
new_results_log[[paste0(open_var, "_5y")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_5y)),
                                                  data=original_data_ext_5_year, 
                                                  index = c("ccode","Year"), 
                                                  model="within", effect="twoways")
new_results_log[[paste0(open_var, "_5y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_5y")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

open_var <- "trade_pc"
new_results_log[[paste0(open_var, "_5y")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_5y)),
                                                  data=original_data_ext_5_year, 
                                                  index = c("ccode","Year"), 
                                                  model="within", effect="twoways")
new_results_log[[paste0(open_var, "_5y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_5y")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

# Full spec
full_spec_l1 <- as.formula(paste0("GDP_pc_growth~log(KOF_econ)", 
                                  control_vars_5y))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_5y))

open_var <- "KOF_econ"
new_results_log[[paste0(open_var, "_5y")]]<- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_5y)),
                                                 data=original_data_ext_5_year, 
                                                 index = c("ccode","Year"), 
                                                 model="within", effect="twoways")
new_results_log[[paste0(open_var, "_5y_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_5y")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_5y))
new_results_log[["KOF_trade_df_5y"]] <- plm(full_spec_l3_kofs, data=original_data_ext_5_year, index = c("ccode","Year"), 
                                            model="within", effect="twoways")
new_results_log[[paste0("KOF_trade_df_5y", "_5y_coeftest")]] <- coeftest(new_results_log[["KOF_trade_df_5y"]], 
                                                                         vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_gdp <- as.formula(paste0("GDP_pc_growth~log(Trade_to_GDP)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                 control_vars_5y))
new_results_log[["Trade_to_GDP_5y_full"]] <- plm(full_spec_l3_kofs_trade_gdp, data=original_data_ext_5_year, index = c("ccode","Year"), 
                                                 model="within", effect="twoways")
new_results_log[[paste0("Trade_to_GDP_full", "_5y_coeftest")]] <- coeftest(new_results_log[["Trade_to_GDP_5y_full"]], 
                                                                           vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_pc <- as.formula(paste0("GDP_pc_growth~log(trade_pc)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                control_vars_5y))
new_results_log[["trade_pc_5y_full"]] <- plm(full_spec_l3_kofs_trade_pc, data=original_data_ext_5_year, index = c("ccode","Year"), 
                                             model="within", effect="twoways")
new_results_log[[paste0("trade_pc_full", "_5y_coeftest")]] <- coeftest(new_results_log[["trade_pc_5y_full"]], 
                                                                       vcov.=function(x) vcovHC(x, type="sss"))


# End 5 years

## FD ----
open_var <- "Trade_to_GDP"
new_results_log[[paste0(open_var, "_fd")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                                  data=filter_data(open_var), 
                                                  index = c("ccode","Year"), 
                                                  model="fd")
new_results_log[[paste0(open_var, "_fd_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_fd")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

open_var <- "trade_pc"
new_results_log[[paste0(open_var, "_fd")]] <- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                                  data=filter_data(open_var), 
                                                  index = c("ccode","Year"), 
                                                  model="fd")
new_results_log[[paste0(open_var, "_fd_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_fd")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

# Full spec
full_spec_l1 <- as.formula(paste0("GDP_pc_growth~log(KOF_econ)", 
                                  control_vars_1y))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_1y))

open_var <- "KOF_econ"
new_results_log[[paste0(open_var, "_fd")]]<- plm(as.formula(paste0("GDP_pc_growth~log(", open_var, ")", control_vars_1y)),
                                                 data=filter_data(open_var), 
                                                 index = c("ccode","Year"), 
                                                 model="fd")
new_results_log[[paste0(open_var, "_fd_coeftest")]] <- coeftest(new_results_log[[paste0(open_var, "_fd")]], 
                                                                vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs <- as.formula(paste0("GDP_pc_growth~log(KOF_trade_df)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                       control_vars_1y))
new_results_log[["KOF_trade_df_fd"]] <- plm(full_spec_l3_kofs, data=original_data_ext, index = c("ccode","Year"), 
                                            model="fd")
new_results_log[[paste0("KOF_trade_df_fd", "_fd_coeftest")]] <- coeftest(new_results_log[["KOF_trade_df_fd"]], 
                                                                         vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_gdp <- as.formula(paste0("GDP_pc_growth~log(Trade_to_GDP)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                 control_vars_1y))
new_results_log[["Trade_to_GDP_fd_full"]] <- plm(full_spec_l3_kofs_trade_gdp, data=original_data_ext, index = c("ccode","Year"), 
                                                 model="fd")
new_results_log[[paste0("Trade_to_GDP_full", "_fd_coeftest")]] <- coeftest(new_results_log[["Trade_to_GDP_fd_full"]], 
                                                                           vcov.=function(x) vcovHC(x, type="sss"))

full_spec_l3_kofs_trade_pc <- as.formula(paste0("GDP_pc_growth~log(trade_pc)+log(KOF_trade_dj)+log(KOF_finance_df)+log(KOF_finance_dj)", 
                                                control_vars_1y))
new_results_log[["trade_pc_fd_full"]] <- plm(full_spec_l3_kofs_trade_pc, data=original_data_ext, index = c("ccode","Year"), 
                                             model="fd")
new_results_log[[paste0("trade_pc_full", "_fd_coeftest")]] <- coeftest(new_results_log[["trade_pc_fd_full"]], 
                                                                       vcov.=function(x) vcovHC(x, type="sss"))


# End FD
# Stargazer ----

trade_per_capity_1y <- stargazer(new_results_log[["Trade_to_GDP_1y"]], new_results_log[["trade_pc_1y"]], 
                                 new_results_log[["KOF_econ_1y"]], new_results_log[["KOF_trade_df_1y"]], 
                                 new_results_log[["Trade_to_GDP_1y_full"]], new_results_log[["trade_pc_1y_full"]],
                                 t=list(unlist(new_results_log[["Trade_to_GDP_1y_coeftest"]][,3]), unlist(new_results_log[["trade_pc_1y_coeftest"]][,3]), 
                                        unlist(new_results_log[["KOF_econ_1y_coeftest"]][,3]), unlist(new_results_log[["KOF_trade_df_1y_1y_coeftest"]][,3]), 
                                        unlist(new_results_log[["Trade_to_GDP_full_1y_coeftest"]][,3]), unlist(new_results_log[["trade_pc_full_1y_coeftest"]][,3])), 
                                 se=list(unlist(new_results_log[["Trade_to_GDP_1y_coeftest"]][,2]), unlist(new_results_log[["trade_pc_1y_coeftest"]][,2]), 
                                         unlist(new_results_log[["KOF_econ_1y_coeftest"]][,2]), unlist(new_results_log[["KOF_trade_df_1y_1y_coeftest"]][,2]), 
                                         unlist(new_results_log[["Trade_to_GDP_full_1y_coeftest"]][,2]), unlist(new_results_log[["trade_pc_full_1y_coeftest"]][,2])), 
                                 p=list(unlist(new_results_log[["Trade_to_GDP_1y_coeftest"]][,4]), unlist(new_results_log[["trade_pc_1y_coeftest"]][,4]), 
                                         unlist(new_results_log[["KOF_econ_1y_coeftest"]][,4]), unlist(new_results_log[["KOF_trade_df_1y_1y_coeftest"]][,4]), 
                                         unlist(new_results_log[["Trade_to_GDP_full_1y_coeftest"]][,4]), unlist(new_results_log[["trade_pc_full_1y_coeftest"]][,4])),
                                 title = "Trade/capita comparison - 1y", #omit = c(16:20),
                                 dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                                 dep.var.caption = "Dependent variable: GDP per capita growth", 
                                 type = "html", out = paste0("output/regressions_html/", "trade_pc_1y", ".html"))



trade_per_capity_5y <- stargazer(new_results_log[["Trade_to_GDP_5y"]], new_results_log[["trade_pc_5y"]], 
                                 new_results_log[["KOF_econ_5y"]], new_results_log[["KOF_trade_df_5y"]], 
                                 new_results_log[["Trade_to_GDP_5y_full"]], new_results_log[["trade_pc_5y_full"]],
                                 t=list(unlist(new_results_log[["Trade_to_GDP_5y_coeftest"]][,3]), unlist(new_results_log[["trade_pc_5y_coeftest"]][,3]), 
                                        unlist(new_results_log[["KOF_econ_5y_coeftest"]][,3]), unlist(new_results_log[["KOF_trade_df_5y_5y_coeftest"]][,3]), 
                                        unlist(new_results_log[["Trade_to_GDP_full_5y_coeftest"]][,3]), unlist(new_results_log[["trade_pc_full_5y_coeftest"]][,3])), 
                                 se=list(unlist(new_results_log[["Trade_to_GDP_5y_coeftest"]][,2]), unlist(new_results_log[["trade_pc_5y_coeftest"]][,2]), 
                                         unlist(new_results_log[["KOF_econ_5y_coeftest"]][,2]), unlist(new_results_log[["KOF_trade_df_5y_5y_coeftest"]][,2]), 
                                         unlist(new_results_log[["Trade_to_GDP_full_5y_coeftest"]][,2]), unlist(new_results_log[["trade_pc_full_5y_coeftest"]][,2])), 
                                 p=list(unlist(new_results_log[["Trade_to_GDP_5y_coeftest"]][,4]), unlist(new_results_log[["trade_pc_5y_coeftest"]][,4]), 
                                        unlist(new_results_log[["KOF_econ_5y_coeftest"]][,4]), unlist(new_results_log[["KOF_trade_df_5y_5y_coeftest"]][,4]), 
                                        unlist(new_results_log[["Trade_to_GDP_full_5y_coeftest"]][,4]), unlist(new_results_log[["trade_pc_full_5y_coeftest"]][,4])),
                                 title = "Trade/capita comparison - 5y", #omit = c(16:20),
                                 dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                                 dep.var.caption = "Dependent variable: GDP per capita growth", 
                                 type = "html", out = paste0("output/regressions_html/", "trade_pc_5y", ".html"))


trade_per_capity_fd <- stargazer(new_results_log[["Trade_to_GDP_fd"]], new_results_log[["trade_pc_fd"]], 
                                 new_results_log[["KOF_econ_fd"]], new_results_log[["KOF_trade_df_fd"]], 
                                 new_results_log[["Trade_to_GDP_fd_full"]], new_results_log[["trade_pc_fd_full"]],
                                 t=list(unlist(new_results_log[["Trade_to_GDP_fd_coeftest"]][,3]), unlist(new_results_log[["trade_pc_fd_coeftest"]][,3]), 
                                        unlist(new_results_log[["KOF_econ_fd_coeftest"]][,3]), unlist(new_results_log[["KOF_trade_df_fd_fd_coeftest"]][,3]), 
                                        unlist(new_results_log[["Trade_to_GDP_full_fd_coeftest"]][,3]), unlist(new_results_log[["trade_pc_full_fd_coeftest"]][,3])), 
                                 se=list(unlist(new_results_log[["Trade_to_GDP_fd_coeftest"]][,2]), unlist(new_results_log[["trade_pc_fd_coeftest"]][,2]), 
                                         unlist(new_results_log[["KOF_econ_fd_coeftest"]][,2]), unlist(new_results_log[["KOF_trade_df_fd_fd_coeftest"]][,2]), 
                                         unlist(new_results_log[["Trade_to_GDP_full_fd_coeftest"]][,2]), unlist(new_results_log[["trade_pc_full_fd_coeftest"]][,2])), 
                                 p=list(unlist(new_results_log[["Trade_to_GDP_fd_coeftest"]][,4]), unlist(new_results_log[["trade_pc_fd_coeftest"]][,4]), 
                                        unlist(new_results_log[["KOF_econ_fd_coeftest"]][,4]), unlist(new_results_log[["KOF_trade_df_fd_fd_coeftest"]][,4]), 
                                        unlist(new_results_log[["Trade_to_GDP_full_fd_coeftest"]][,4]), unlist(new_results_log[["trade_pc_full_fd_coeftest"]][,4])),
                                 title = "Trade/capita comparison - fd", #omit = c(16:20),
                                 dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                                 dep.var.caption = "Dependent variable: GDP per capita growth", 
                                 type = "html", out = paste0("output/regressions_html/", "trade_pc_fd", ".html"))




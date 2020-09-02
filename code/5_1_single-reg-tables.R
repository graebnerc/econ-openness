
# 1.1.1. Trade de facto - 5 year averages ----
de_fact_regs <- regressions_list[["trade_de_facto_results_log_5y"]]

tr_df5_reg_1 <- de_fact_regs$trade_to_GDP
tr_df5_ses.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,2])
tr_df5_tvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,3])
tr_df5_pvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,4])

tr_df5_reg_2 <- de_fact_regs$alcala
tr_df5_ses.reg_2 <- list(de_fact_regs$alcala_coeftest[,2])
tr_df5_tvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,3])
tr_df5_pvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,4])

tr_df5_reg_3 <- de_fact_regs$lietal
tr_df5_ses.reg_3 <- list(de_fact_regs$lietal_coeftest[,2])
tr_df5_tvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,3])
tr_df5_pvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,4])

tr_df5_reg_4 <- de_fact_regs$toi
tr_df5_ses.reg_4 <- list(de_fact_regs$toi_coeftest[,2])
tr_df5_tvals.reg_4 <- list(de_fact_regs$toi_coeftest[,3])
tr_df5_pvals.reg_4 <- list(de_fact_regs$toi_coeftest[,4])

tr_df5_reg_5 <- de_fact_regs$kof_defacto
tr_df5_ses.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,2])
tr_df5_tvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,3])
tr_df5_pvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,4])

tr_df5_reg_6 <- de_fact_regs$cts
tr_df5_ses.reg_6 <- list(de_fact_regs$cts_coeftest[,2])
tr_df5_tvals.reg_6 <- list(de_fact_regs$cts_coeftest[,3])
tr_df5_pvals.reg_6 <- list(de_fact_regs$cts_coeftest[,4])

table_title <- "Trade - De facto (5 year averages)"
output_name <- "trade_de_facto_5y"

stargazer(tr_df5_reg_1, tr_df5_reg_2, tr_df5_reg_3, tr_df5_reg_4, tr_df5_reg_5, tr_df5_reg_6,
          t=list(unlist(tr_df5_tvals.reg_1), unlist(tr_df5_tvals.reg_2), unlist(tr_df5_tvals.reg_3), unlist(tr_df5_tvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
          se=list(unlist(tr_df5_ses.reg_1), unlist(tr_df5_ses.reg_2), unlist(tr_df5_ses.reg_3), unlist(tr_df5_ses.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
          p=list(unlist(tr_df5_pvals.reg_1), unlist(tr_df5_pvals.reg_2), unlist(tr_df5_pvals.reg_3), unlist(tr_df5_pvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)),
          title = table_title, 
          omit = c(7:10),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_df5 <- stargazer(tr_df5_reg_1, tr_df5_reg_2, tr_df5_reg_3, tr_df5_reg_4, tr_df5_reg_5, tr_df5_reg_6,
                    t=list(unlist(tr_df5_tvals.reg_1), unlist(tr_df5_tvals.reg_2), unlist(tr_df5_tvals.reg_3), unlist(tr_df5_tvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
                    se=list(unlist(tr_df5_ses.reg_1), unlist(tr_df5_ses.reg_2), unlist(tr_df5_ses.reg_3), unlist(tr_df5_ses.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
                    p=list(unlist(tr_df5_pvals.reg_1), unlist(tr_df5_pvals.reg_2), unlist(tr_df5_pvals.reg_3), unlist(tr_df5_pvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)),
                    title = table_title, 
                    omit = c(7:11),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

tr_df5_app <- stargazer(tr_df5_reg_1, tr_df5_reg_2, tr_df5_reg_3, tr_df5_reg_4, tr_df5_reg_5, tr_df5_reg_6,
                        t=list(unlist(tr_df5_tvals.reg_1), unlist(tr_df5_tvals.reg_2), unlist(tr_df5_tvals.reg_3), unlist(tr_df5_tvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
                        se=list(unlist(tr_df5_ses.reg_1), unlist(tr_df5_ses.reg_2), unlist(tr_df5_ses.reg_3), unlist(tr_df5_ses.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)), 
                        p=list(unlist(tr_df5_pvals.reg_1), unlist(tr_df5_pvals.reg_2), unlist(tr_df5_pvals.reg_3), unlist(tr_df5_pvals.reg_4), unlist(tr_df5_tvals.reg_5), unlist(tr_df5_tvals.reg_6)),
                        title = table_title, df = FALSE, 
                        no.space = TRUE,
                        dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"), float = FALSE,
                        dep.var.caption = "Dependent variable: GDP per capita growth", 
                        type = "latex", out = here(paste0("appendix/", output_name, ".tex")))

# 1.1.2. Trade de facto - annual data ----
de_fact_regs <- regressions_list[["trade_de_facto_results_log"]]

tr_df1_reg_1 <- de_fact_regs$trade_to_GDP
tr_df1_ses.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,2])
tr_df1_tvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,3])
tr_df1_pvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,4])

tr_df1_reg_2 <- de_fact_regs$alcala
tr_df1_ses.reg_2 <- list(de_fact_regs$alcala_coeftest[,2])
tr_df1_tvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,3])
tr_df1_pvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,4])

tr_df1_reg_3 <- de_fact_regs$lietal
tr_df1_ses.reg_3 <- list(de_fact_regs$lietal_coeftest[,2])
tr_df1_tvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,3])
tr_df1_pvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,4])

tr_df1_reg_4 <- de_fact_regs$toi
tr_df1_ses.reg_4 <- list(de_fact_regs$toi_coeftest[,2])
tr_df1_tvals.reg_4 <- list(de_fact_regs$toi_coeftest[,3])
tr_df1_pvals.reg_4 <- list(de_fact_regs$toi_coeftest[,4])

tr_df1_reg_5 <- de_fact_regs$kof_defacto
tr_df1_ses.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,2])
tr_df1_tvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,3])
tr_df1_pvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,4])

tr_df1_reg_6 <- de_fact_regs$cts
tr_df1_ses.reg_6 <- list(de_fact_regs$cts_coeftest[,2])
tr_df1_tvals.reg_6 <- list(de_fact_regs$cts_coeftest[,3])
tr_df1_pvals.reg_6 <- list(de_fact_regs$cts_coeftest[,4])


table_title <- "Trade - De facto (annual data)"
output_name <- "trade_de_facto_1y"

stargazer(tr_df1_reg_1, tr_df1_reg_2, tr_df1_reg_3, tr_df1_reg_4, tr_df1_reg_5, tr_df1_reg_6,
          t=list(unlist(tr_df1_tvals.reg_1), unlist(tr_df1_tvals.reg_2), unlist(tr_df1_tvals.reg_3), unlist(tr_df1_tvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
          se=list(unlist(tr_df1_ses.reg_1), unlist(tr_df1_ses.reg_2), unlist(tr_df1_ses.reg_3), unlist(tr_df1_ses.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
          p=list(unlist(tr_df1_pvals.reg_1), unlist(tr_df1_pvals.reg_2), unlist(tr_df1_pvals.reg_3), unlist(tr_df1_pvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)),
          title = table_title, omit = c(),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_df1 <- stargazer(tr_df1_reg_1, tr_df1_reg_2, tr_df1_reg_3, tr_df1_reg_4, tr_df1_reg_5, tr_df1_reg_6,
                    t=list(unlist(tr_df1_tvals.reg_1), unlist(tr_df1_tvals.reg_2), unlist(tr_df1_tvals.reg_3), unlist(tr_df1_tvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
                    se=list(unlist(tr_df1_ses.reg_1), unlist(tr_df1_ses.reg_2), unlist(tr_df1_ses.reg_3), unlist(tr_df1_ses.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
                    p=list(unlist(tr_df1_pvals.reg_1), unlist(tr_df1_pvals.reg_2), unlist(tr_df1_pvals.reg_3), unlist(tr_df1_pvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)),
                    title = table_title, omit = c(1, 6:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

# 1.1.3. Trade de facto - annual data with FD ----
de_fact_regs <- regressions_list[["trade_de_facto_results_log_fd"]]

tr_df1_reg_1 <- de_fact_regs$trade_to_GDP
tr_df1_ses.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,2])
tr_df1_tvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,3])
tr_df1_pvals.reg_1 <- list(de_fact_regs$trade_to_GDP_coeftest[,4])

tr_df1_reg_2 <- de_fact_regs$alcala
tr_df1_ses.reg_2 <- list(de_fact_regs$alcala_coeftest[,2])
tr_df1_tvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,3])
tr_df1_pvals.reg_2 <- list(de_fact_regs$alcala_coeftest[,4])

tr_df1_reg_3 <- de_fact_regs$lietal
tr_df1_ses.reg_3 <- list(de_fact_regs$lietal_coeftest[,2])
tr_df1_tvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,3])
tr_df1_pvals.reg_3 <- list(de_fact_regs$lietal_coeftest[,4])

tr_df1_reg_4 <- de_fact_regs$toi
tr_df1_ses.reg_4 <- list(de_fact_regs$toi_coeftest[,2])
tr_df1_tvals.reg_4 <- list(de_fact_regs$toi_coeftest[,3])
tr_df1_pvals.reg_4 <- list(de_fact_regs$toi_coeftest[,4])

tr_df1_reg_5 <- de_fact_regs$kof_defacto
tr_df1_ses.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,2])
tr_df1_tvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,3])
tr_df1_pvals.reg_5 <- list(de_fact_regs$kof_defacto_coeftest[,4])

tr_df1_reg_6 <- de_fact_regs$cts
tr_df1_ses.reg_6 <- list(de_fact_regs$cts_coeftest[,2])
tr_df1_tvals.reg_6 <- list(de_fact_regs$cts_coeftest[,3])
tr_df1_pvals.reg_6 <- list(de_fact_regs$cts_coeftest[,4])


table_title <- "Trade - De facto (annual data) with FD"
output_name <- "trade_de_facto_1y_fd"

stargazer(tr_df1_reg_1, tr_df1_reg_2, tr_df1_reg_3, tr_df1_reg_4, tr_df1_reg_5, tr_df1_reg_6,
          t=list(unlist(tr_df1_tvals.reg_1), unlist(tr_df1_tvals.reg_2), unlist(tr_df1_tvals.reg_3), unlist(tr_df1_tvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
          se=list(unlist(tr_df1_ses.reg_1), unlist(tr_df1_ses.reg_2), unlist(tr_df1_ses.reg_3), unlist(tr_df1_ses.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
          p=list(unlist(tr_df1_pvals.reg_1), unlist(tr_df1_pvals.reg_2), unlist(tr_df1_pvals.reg_3), unlist(tr_df1_pvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)),
          title = table_title, omit = c(),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_df1 <- stargazer(tr_df1_reg_1, tr_df1_reg_2, tr_df1_reg_3, tr_df1_reg_4, tr_df1_reg_5, tr_df1_reg_6,
                    t=list(unlist(tr_df1_tvals.reg_1), unlist(tr_df1_tvals.reg_2), unlist(tr_df1_tvals.reg_3), unlist(tr_df1_tvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
                    se=list(unlist(tr_df1_ses.reg_1), unlist(tr_df1_ses.reg_2), unlist(tr_df1_ses.reg_3), unlist(tr_df1_ses.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)), 
                    p=list(unlist(tr_df1_pvals.reg_1), unlist(tr_df1_pvals.reg_2), unlist(tr_df1_pvals.reg_3), unlist(tr_df1_pvals.reg_4), unlist(tr_df1_tvals.reg_5), unlist(tr_df1_tvals.reg_6)),
                    title = table_title, omit = c(8:10),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "text", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))


# 1.2.1. Trade de jure - 5 year averages ----
de_jure_regs <- regressions_list[["trade_de_jure_results_log_5y"]]

tr_dj5_reg_1 <- de_jure_regs$kof_dejure
tr_dj5_ses.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,2])
tr_dj5_tvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,3])
tr_dj5_pvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,4])

tr_dj5_reg_2 <- de_jure_regs$Tariff_WITS_ipo
tr_dj5_ses.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,2])
tr_dj5_tvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,3])
tr_dj5_pvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,4])

tr_dj5_reg_3 <- de_jure_regs$FTI
tr_dj5_ses.reg_3 <- list(de_jure_regs$FTI_coeftest[,2])
tr_dj5_tvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,3])
tr_dj5_pvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,4])

tr_dj5_reg_4 <- de_jure_regs$HF_trade
tr_dj5_ses.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,2])
tr_dj5_tvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,3])
tr_dj5_pvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,4])

table_title <- "Trade - De jure (5 year averages)"
output_name <- "trade_de_jure_5y"

stargazer(tr_dj5_reg_1, tr_dj5_reg_2, tr_dj5_reg_3, tr_dj5_reg_4, 
          t=list(unlist(tr_dj5_tvals.reg_1), unlist(tr_dj5_tvals.reg_2), unlist(tr_dj5_tvals.reg_3), unlist(tr_dj5_tvals.reg_4)), 
          se=list(unlist(tr_dj5_ses.reg_1), unlist(tr_dj5_ses.reg_2), unlist(tr_dj5_ses.reg_3), unlist(tr_dj5_ses.reg_4)), 
          p=list(unlist(tr_dj5_pvals.reg_1), unlist(tr_dj5_pvals.reg_2), unlist(tr_dj5_pvals.reg_3), unlist(tr_dj5_pvals.reg_4)),
          title = table_title, omit = c(5:9),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_dj5 <- stargazer(tr_dj5_reg_1, tr_dj5_reg_2, tr_dj5_reg_3, tr_dj5_reg_4, 
                    t=list(unlist(tr_dj5_tvals.reg_1), unlist(tr_dj5_tvals.reg_2), unlist(tr_dj5_tvals.reg_3), unlist(tr_dj5_tvals.reg_4)), 
                    se=list(unlist(tr_dj5_ses.reg_1), unlist(tr_dj5_ses.reg_2), unlist(tr_dj5_ses.reg_3), unlist(tr_dj5_ses.reg_4)), 
                    p=list(unlist(tr_dj5_pvals.reg_1), unlist(tr_dj5_pvals.reg_2), unlist(tr_dj5_pvals.reg_3), unlist(tr_dj5_pvals.reg_4)),
                    title = table_title, omit = c(5:9),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

tr_dj5_app <- stargazer(tr_dj5_reg_1, tr_dj5_reg_2, tr_dj5_reg_3, tr_dj5_reg_4, 
                        t=list(unlist(tr_dj5_tvals.reg_1), unlist(tr_dj5_tvals.reg_2), unlist(tr_dj5_tvals.reg_3), unlist(tr_dj5_tvals.reg_4)), 
                        se=list(unlist(tr_dj5_ses.reg_1), unlist(tr_dj5_ses.reg_2), unlist(tr_dj5_ses.reg_3), unlist(tr_dj5_ses.reg_4)), 
                        p=list(unlist(tr_dj5_pvals.reg_1), unlist(tr_dj5_pvals.reg_2), unlist(tr_dj5_pvals.reg_3), unlist(tr_dj5_pvals.reg_4)),
                        title = table_title, df = FALSE, 
                        no.space = TRUE,
                        dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"), float = FALSE,
                        dep.var.caption = "Dependent variable: GDP per capita growth", 
                        type = "latex", out = here(paste0("appendix/", output_name, ".tex")))

# 1.2.2. Trade de jure - annual data ----
de_jure_regs <- regressions_list[["trade_de_jure_results_log"]]

tr_dj1_reg_1 <- de_jure_regs$kof_dejure
tr_dj1_ses.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,2])
tr_dj1_tvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,3])
tr_dj1_pvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,4])

tr_dj1_reg_2 <- de_jure_regs$Tariff_WITS_ipo
tr_dj1_ses.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,2])
tr_dj1_tvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,3])
tr_dj1_pvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,4])

tr_dj1_reg_3 <- de_jure_regs$FTI
tr_dj1_ses.reg_3 <- list(de_jure_regs$FTI_coeftest[,2])
tr_dj1_tvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,3])
tr_dj1_pvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,4])

tr_dj1_reg_4 <- de_jure_regs$HF_trade
tr_dj1_ses.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,2])
tr_dj1_tvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,3])
tr_dj1_pvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,4])

table_title <- "Trade - De jure (annual data)"
output_name <- "trade_de_jure_1y"

stargazer(tr_dj1_reg_1, tr_dj1_reg_2, tr_dj1_reg_3, tr_dj1_reg_4, 
          t=list(unlist(tr_dj1_tvals.reg_1), unlist(tr_dj1_tvals.reg_2), unlist(tr_dj1_tvals.reg_3), unlist(tr_dj1_tvals.reg_4)), 
          se=list(unlist(tr_dj1_ses.reg_1), unlist(tr_dj1_ses.reg_2), unlist(tr_dj1_ses.reg_3), unlist(tr_dj1_ses.reg_4)), 
          p=list(unlist(tr_dj1_pvals.reg_1), unlist(tr_dj1_pvals.reg_2), unlist(tr_dj1_pvals.reg_3), unlist(tr_dj1_pvals.reg_4)),
          title = table_title, omit = c(6:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_dj1 <- stargazer(tr_dj1_reg_1, tr_dj1_reg_2, tr_dj1_reg_3, tr_dj1_reg_4, 
                    t=list(unlist(tr_dj1_tvals.reg_1), unlist(tr_dj1_tvals.reg_2), unlist(tr_dj1_tvals.reg_3), unlist(tr_dj1_tvals.reg_4)), 
                    se=list(unlist(tr_dj1_ses.reg_1), unlist(tr_dj1_ses.reg_2), unlist(tr_dj1_ses.reg_3), unlist(tr_dj1_ses.reg_4)), 
                    p=list(unlist(tr_dj1_pvals.reg_1), unlist(tr_dj1_pvals.reg_2), unlist(tr_dj1_pvals.reg_3), unlist(tr_dj1_pvals.reg_4)),
                    title = table_title, omit = c(6:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

# 1.2.3. Trade de jure - annual data with FD ----
de_jure_regs <- regressions_list[["trade_de_jure_results_log_fd"]]

tr_dj1_reg_1 <- de_jure_regs$kof_dejure
tr_dj1_ses.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,2])
tr_dj1_tvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,3])
tr_dj1_pvals.reg_1 <- list(de_jure_regs$kof_dejure_coeftest[,4])

tr_dj1_reg_2 <- de_jure_regs$Tariff_WITS_ipo
tr_dj1_ses.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,2])
tr_dj1_tvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,3])
tr_dj1_pvals.reg_2 <- list(de_jure_regs$Tariff_WITS_ipo_coeftest[,4])

tr_dj1_reg_3 <- de_jure_regs$FTI
tr_dj1_ses.reg_3 <- list(de_jure_regs$FTI_coeftest[,2])
tr_dj1_tvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,3])
tr_dj1_pvals.reg_3 <- list(de_jure_regs$FTI_coeftest[,4])

tr_dj1_reg_4 <- de_jure_regs$HF_trade
tr_dj1_ses.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,2])
tr_dj1_tvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,3])
tr_dj1_pvals.reg_4 <- list(de_jure_regs$HF_trade_coeftest[,4])

table_title <- "Trade - De jure (annual data) with FD"
output_name <- "trade_de_jure_1y_fd"

stargazer(tr_dj1_reg_1, tr_dj1_reg_2, tr_dj1_reg_3, tr_dj1_reg_4, 
          t=list(unlist(tr_dj1_tvals.reg_1), unlist(tr_dj1_tvals.reg_2), unlist(tr_dj1_tvals.reg_3), unlist(tr_dj1_tvals.reg_4)), 
          se=list(unlist(tr_dj1_ses.reg_1), unlist(tr_dj1_ses.reg_2), unlist(tr_dj1_ses.reg_3), unlist(tr_dj1_ses.reg_4)), 
          p=list(unlist(tr_dj1_pvals.reg_1), unlist(tr_dj1_pvals.reg_2), unlist(tr_dj1_pvals.reg_3), unlist(tr_dj1_pvals.reg_4)),
          title = table_title, omit = c(6:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

tr_dj1 <- stargazer(tr_dj1_reg_1, tr_dj1_reg_2, tr_dj1_reg_3, tr_dj1_reg_4, 
                    t=list(unlist(tr_dj1_tvals.reg_1), unlist(tr_dj1_tvals.reg_2), unlist(tr_dj1_tvals.reg_3), unlist(tr_dj1_tvals.reg_4)), 
                    se=list(unlist(tr_dj1_ses.reg_1), unlist(tr_dj1_ses.reg_2), unlist(tr_dj1_ses.reg_3), unlist(tr_dj1_ses.reg_4)), 
                    p=list(unlist(tr_dj1_pvals.reg_1), unlist(tr_dj1_pvals.reg_2), unlist(tr_dj1_pvals.reg_3), unlist(tr_dj1_pvals.reg_4)),
                    title = table_title, omit = c(6:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))


# 1.3.1. Finance de facto - 5 year averages ----
de_facto_finance_regs <- regressions_list[["finance_de_facto_results_log_5y"]]

fi_df5_reg_1 <- de_facto_finance_regs$LMF_open_gdp
fi_df5_ses.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,2])
fi_df5_tvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,3])
fi_df5_pvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,4])

fi_df5_reg_2 <- de_facto_finance_regs$LMF_EQ_gdp
fi_df5_ses.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,2])
fi_df5_tvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,3])
fi_df5_pvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,4])

fi_df5_reg_3 <- de_facto_finance_regs$UNC_FDI_in_stock_GDP
fi_df5_ses.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,2])
fi_df5_tvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,3])
fi_df5_pvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,4])

fi_df5_reg_4 <- de_facto_finance_regs$UNC_FDI_out_stock_GDP
fi_df5_ses.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,2])
fi_df5_tvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,3])
fi_df5_pvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,4])

table_title <- "Finance - De facto (5 year averages)"
output_name <- "finance_de_facto_5y"

stargazer(fi_df5_reg_1, fi_df5_reg_2, fi_df5_reg_3, fi_df5_reg_4, 
          t=list(unlist(fi_df5_tvals.reg_1), unlist(fi_df5_tvals.reg_2), unlist(fi_df5_tvals.reg_3), unlist(fi_df5_tvals.reg_4)), 
          se=list(unlist(fi_df5_ses.reg_1), unlist(fi_df5_ses.reg_2), unlist(fi_df5_ses.reg_3), unlist(fi_df5_ses.reg_4)), 
          p=list(unlist(fi_df5_pvals.reg_1), unlist(fi_df5_pvals.reg_2), unlist(fi_df5_pvals.reg_3), unlist(fi_df5_pvals.reg_4)),
          title = table_title, omit = c(5:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_df5 <- stargazer(fi_df5_reg_1, fi_df5_reg_2, fi_df5_reg_3, fi_df5_reg_4, 
                    t=list(unlist(fi_df5_tvals.reg_1), unlist(fi_df5_tvals.reg_2), unlist(fi_df5_tvals.reg_3), unlist(fi_df5_tvals.reg_4)), 
                    se=list(unlist(fi_df5_ses.reg_1), unlist(fi_df5_ses.reg_2), unlist(fi_df5_ses.reg_3), unlist(fi_df5_ses.reg_4)), 
                    p=list(unlist(fi_df5_pvals.reg_1), unlist(fi_df5_pvals.reg_2), unlist(fi_df5_pvals.reg_3), unlist(fi_df5_pvals.reg_4)),
                    title = table_title, omit = c(5:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

fi_df5_app <- stargazer(fi_df5_reg_1, fi_df5_reg_2, fi_df5_reg_3, fi_df5_reg_4, 
                        t=list(unlist(fi_df5_tvals.reg_1), unlist(fi_df5_tvals.reg_2), unlist(fi_df5_tvals.reg_3), unlist(fi_df5_tvals.reg_4)), 
                        se=list(unlist(fi_df5_ses.reg_1), unlist(fi_df5_ses.reg_2), unlist(fi_df5_ses.reg_3), unlist(fi_df5_ses.reg_4)), 
                        p=list(unlist(fi_df5_pvals.reg_1), unlist(fi_df5_pvals.reg_2), unlist(fi_df5_pvals.reg_3), unlist(fi_df5_pvals.reg_4)),
                        title = table_title, df = FALSE, 
                        no.space = TRUE,
                        dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"), float = FALSE,
                        dep.var.caption = "Dependent variable: GDP per capita growth", 
                        type = "html", out = here(paste0("appendix/", output_name, ".tex")))

# 1.3.2. Finance de facto - annual data ----
de_facto_finance_regs <- regressions_list[["finance_de_facto_results_log"]]

fi_df1_reg_1 <- de_facto_finance_regs$LMF_open_gdp
fi_df1_ses.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,2])
fi_df1_tvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,3])
fi_df1_pvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,4])

fi_df1_reg_2 <- de_facto_finance_regs$LMF_EQ_gdp
fi_df1_ses.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,2])
fi_df1_tvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,3])
fi_df1_pvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,4])

fi_df1_reg_3 <- de_facto_finance_regs$UNC_FDI_in_stock_GDP
fi_df1_ses.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,2])
fi_df1_tvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,3])
fi_df1_pvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,4])

fi_df1_reg_4 <- de_facto_finance_regs$UNC_FDI_out_stock_GDP
fi_df1_ses.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,2])
fi_df1_tvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,3])
fi_df1_pvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,4])

table_title <- "Finance - De facto (annual data)"
output_name <- "finance_de_facto_1y"

stargazer(fi_df1_reg_1, fi_df1_reg_2, fi_df1_reg_3, fi_df1_reg_4, 
          t=list(unlist(fi_df1_tvals.reg_1), unlist(fi_df1_tvals.reg_2), unlist(fi_df1_tvals.reg_3), unlist(fi_df1_tvals.reg_4)), 
          se=list(unlist(fi_df1_ses.reg_1), unlist(fi_df1_ses.reg_2), unlist(fi_df1_ses.reg_3), unlist(fi_df1_ses.reg_4)), 
          p=list(unlist(fi_df1_pvals.reg_1), unlist(fi_df1_pvals.reg_2), unlist(fi_df1_pvals.reg_3), unlist(fi_df1_pvals.reg_4)),
          title = table_title, omit = c(5:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_df1 <- stargazer(fi_df1_reg_1, fi_df1_reg_2, fi_df1_reg_3, fi_df1_reg_4, 
                    t=list(unlist(fi_df1_tvals.reg_1), unlist(fi_df1_tvals.reg_2), unlist(fi_df1_tvals.reg_3), unlist(fi_df1_tvals.reg_4)), 
                    se=list(unlist(fi_df1_ses.reg_1), unlist(fi_df1_ses.reg_2), unlist(fi_df1_ses.reg_3), unlist(fi_df1_ses.reg_4)), 
                    p=list(unlist(fi_df1_pvals.reg_1), unlist(fi_df1_pvals.reg_2), unlist(fi_df1_pvals.reg_3), unlist(fi_df1_pvals.reg_4)),
                    title = table_title, omit = c(5:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

# 1.3.3. Finance de facto - annual data with FD ----
de_facto_finance_regs <- regressions_list[["finance_de_facto_results_log_fd"]]

fi_df1_reg_1 <- de_facto_finance_regs$LMF_open_gdp
fi_df1_ses.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,2])
fi_df1_tvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,3])
fi_df1_pvals.reg_1 <- list(de_facto_finance_regs$LMF_open_gdp_coeftest[,4])

fi_df1_reg_2 <- de_facto_finance_regs$LMF_EQ_gdp
fi_df1_ses.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,2])
fi_df1_tvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,3])
fi_df1_pvals.reg_2 <- list(de_facto_finance_regs$LMF_EQ_gdp_coeftest[,4])

fi_df1_reg_3 <- de_facto_finance_regs$UNC_FDI_in_stock_GDP
fi_df1_ses.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,2])
fi_df1_tvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,3])
fi_df1_pvals.reg_3 <- list(de_facto_finance_regs$UNC_FDI_in_stock_GDPcoeftest[,4])

fi_df1_reg_4 <- de_facto_finance_regs$UNC_FDI_out_stock_GDP
fi_df1_ses.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,2])
fi_df1_tvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,3])
fi_df1_pvals.reg_4 <- list(de_facto_finance_regs$UNC_FDI_out_stock_GDP_coeftest[,4])

table_title <- "Finance - De facto (annual data) with FD"
output_name <- "finance_de_facto_1y_fd"

stargazer(fi_df1_reg_1, fi_df1_reg_2, fi_df1_reg_3, fi_df1_reg_4, 
          t=list(unlist(fi_df1_tvals.reg_1), unlist(fi_df1_tvals.reg_2), unlist(fi_df1_tvals.reg_3), unlist(fi_df1_tvals.reg_4)), 
          se=list(unlist(fi_df1_ses.reg_1), unlist(fi_df1_ses.reg_2), unlist(fi_df1_ses.reg_3), unlist(fi_df1_ses.reg_4)), 
          p=list(unlist(fi_df1_pvals.reg_1), unlist(fi_df1_pvals.reg_2), unlist(fi_df1_pvals.reg_3), unlist(fi_df1_pvals.reg_4)),
          title = table_title, omit = c(6:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_df1 <- stargazer(fi_df1_reg_1, fi_df1_reg_2, fi_df1_reg_3, fi_df1_reg_4, 
                    t=list(unlist(fi_df1_tvals.reg_1), unlist(fi_df1_tvals.reg_2), unlist(fi_df1_tvals.reg_3), unlist(fi_df1_tvals.reg_4)), 
                    se=list(unlist(fi_df1_ses.reg_1), unlist(fi_df1_ses.reg_2), unlist(fi_df1_ses.reg_3), unlist(fi_df1_ses.reg_4)), 
                    p=list(unlist(fi_df1_pvals.reg_1), unlist(fi_df1_pvals.reg_2), unlist(fi_df1_pvals.reg_3), unlist(fi_df1_pvals.reg_4)),
                    title = table_title, omit = c(6:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

# 1.4.1. Finance de jure - 5 year averages ----
de_jure_finance_regs <- regressions_list[["finance_de_jure_results_log_5y"]]

fi_dj5_reg_1 <- de_jure_finance_regs$KAOPEN
fi_dj5_ses.reg_1 <- list(de_jure_finance_regs$KAOPEN_coeftest[,2])
fi_dj5_tvals.reg_1 <- list(de_jure_finance_regs$KAOPEN_coeftest[,3])
fi_dj5_pvals.reg_1 <- list(de_jure_finance_regs$KAOPEN_coeftest[,4])

fi_dj5_reg_2 <- de_jure_finance_regs$HF_fin
fi_dj5_ses.reg_2 <- list(de_jure_finance_regs$HF_fin_coeftest[,2])
fi_dj5_tvals.reg_2 <- list(de_jure_finance_regs$HF_fin_coeftest[,3])
fi_dj5_pvals.reg_2 <- list(de_jure_finance_regs$HF_fin_coeftest[,4])

fi_dj5_reg_3 <- de_jure_finance_regs$CAPITAL
fi_dj5_ses.reg_3 <- list(de_jure_finance_regs$CAPITAL_coeftest[,2])
fi_dj5_tvals.reg_3 <- list(de_jure_finance_regs$CAPITAL_coeftest[,3])
fi_dj5_pvals.reg_3 <- list(de_jure_finance_regs$CAPITAL_coeftest[,4])

table_title <- "Finance - De jure (5 year averages)"
output_name <- "finance_de_jure_5y"

stargazer(fi_dj5_reg_1, fi_dj5_reg_2, fi_dj5_reg_3,  
          t=list(unlist(fi_dj5_tvals.reg_1), unlist(fi_dj5_tvals.reg_2), unlist(fi_dj5_tvals.reg_3)),
          se=list(unlist(fi_dj5_ses.reg_1), unlist(fi_dj5_ses.reg_2), unlist(fi_dj5_ses.reg_3)), 
          p=list(unlist(fi_dj5_pvals.reg_1), unlist(fi_dj5_pvals.reg_2), unlist(fi_dj5_pvals.reg_3)),
          title = table_title, omit = c(4:8),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_dj5 <- stargazer(fi_dj5_reg_1, fi_dj5_reg_2, fi_dj5_reg_3,  
                    t=list(unlist(fi_dj5_tvals.reg_1), unlist(fi_dj5_tvals.reg_2), unlist(fi_dj5_tvals.reg_3)),
                    se=list(unlist(fi_dj5_ses.reg_1), unlist(fi_dj5_ses.reg_2), unlist(fi_dj5_ses.reg_3)), 
                    p=list(unlist(fi_dj5_pvals.reg_1), unlist(fi_dj5_pvals.reg_2), unlist(fi_dj5_pvals.reg_3)),
                    title = table_title, omit = c(4:8),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

fi_dj5_app <- stargazer(fi_dj5_reg_1, fi_dj5_reg_2, fi_dj5_reg_3,  
                        t=list(unlist(fi_dj5_tvals.reg_1), unlist(fi_dj5_tvals.reg_2), unlist(fi_dj5_tvals.reg_3)),
                        se=list(unlist(fi_dj5_ses.reg_1), unlist(fi_dj5_ses.reg_2), unlist(fi_dj5_ses.reg_3)), 
                        p=list(unlist(fi_dj5_pvals.reg_1), unlist(fi_dj5_pvals.reg_2), unlist(fi_dj5_pvals.reg_3)),
                        title = table_title, df = FALSE, 
                        no.space = TRUE,
                        dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"), float = FALSE,
                        dep.var.caption = "Dependent variable: GDP per capita growth", 
                        type = "html", out = here(paste0("appendix/", output_name, ".tex")))


# 1.4.2. Finance de jure - annual data ----
de_facto_finance_regs <- regressions_list[["finance_de_jure_results_log"]]

fi_dj1_reg_1 <- de_facto_finance_regs$KAOPEN
fi_dj1_ses.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,2])
fi_dj1_tvals.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,3])
fi_dj1_pvals.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,4])

fi_dj1_reg_2 <- de_facto_finance_regs$HF_fin
fi_dj1_ses.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,2])
fi_dj1_tvals.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,3])
fi_dj1_pvals.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,4])

fi_dj1_reg_3 <- de_facto_finance_regs$CAPITAL
fi_dj1_ses.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,2])
fi_dj1_tvals.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,3])
fi_dj1_pvals.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,4])

table_title <- "Finance - De jure (annual data)"
output_name <- "finance_de_jure_1y"

stargazer(fi_dj1_reg_1, fi_dj1_reg_2, fi_dj1_reg_3, 
          t=list(unlist(fi_dj1_tvals.reg_1), unlist(fi_dj1_tvals.reg_2), unlist(fi_dj1_tvals.reg_3)), 
          se=list(unlist(fi_dj1_ses.reg_1), unlist(fi_dj1_ses.reg_2), unlist(fi_dj1_ses.reg_3)), 
          p=list(unlist(fi_dj1_pvals.reg_1), unlist(fi_dj1_pvals.reg_2), unlist(fi_dj1_pvals.reg_3)),
          title = table_title, omit = c(4:7),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_dj1 <- stargazer(fi_dj1_reg_1, fi_dj1_reg_2, fi_dj1_reg_3, 
                    t=list(unlist(fi_dj1_tvals.reg_1), unlist(fi_dj1_tvals.reg_2), unlist(fi_dj1_tvals.reg_3)), 
                    se=list(unlist(fi_dj1_ses.reg_1), unlist(fi_dj1_ses.reg_2), unlist(fi_dj1_ses.reg_3)), 
                    p=list(unlist(fi_dj1_pvals.reg_1), unlist(fi_dj1_pvals.reg_2), unlist(fi_dj1_pvals.reg_3)),
                    title = table_title, omit = c(4:7),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))


# 1.4.3. Finance de jure - annual data with FD----
de_facto_finance_regs <- regressions_list[["finance_de_jure_results_log_fd"]]

fi_dj1_reg_1 <- de_facto_finance_regs$KAOPEN
fi_dj1_ses.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,2])
fi_dj1_tvals.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,3])
fi_dj1_pvals.reg_1 <- list(de_facto_finance_regs$KAOPEN_coeftest[,4])

fi_dj1_reg_2 <- de_facto_finance_regs$HF_fin
fi_dj1_ses.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,2])
fi_dj1_tvals.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,3])
fi_dj1_pvals.reg_2 <- list(de_facto_finance_regs$HF_fin_coeftest[,4])

fi_dj1_reg_3 <- de_facto_finance_regs$CAPITAL
fi_dj1_ses.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,2])
fi_dj1_tvals.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,3])
fi_dj1_pvals.reg_3 <- list(de_facto_finance_regs$CAPITAL_coeftest[,4])

table_title <- "Finance - De jure (annual data) with FD"
output_name <- "finance_de_jure_1y_fd"

stargazer(fi_dj1_reg_1, fi_dj1_reg_2, fi_dj1_reg_3, 
          t=list(unlist(fi_dj1_tvals.reg_1), unlist(fi_dj1_tvals.reg_2), unlist(fi_dj1_tvals.reg_3)), 
          se=list(unlist(fi_dj1_ses.reg_1), unlist(fi_dj1_ses.reg_2), unlist(fi_dj1_ses.reg_3)), 
          p=list(unlist(fi_dj1_pvals.reg_1), unlist(fi_dj1_pvals.reg_2), unlist(fi_dj1_pvals.reg_3)),
          title = table_title, omit = c(5:7),
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          type = "text")

fi_dj1 <- stargazer(fi_dj1_reg_1, fi_dj1_reg_2, fi_dj1_reg_3, 
                    t=list(unlist(fi_dj1_tvals.reg_1), unlist(fi_dj1_tvals.reg_2), unlist(fi_dj1_tvals.reg_3)), 
                    se=list(unlist(fi_dj1_ses.reg_1), unlist(fi_dj1_ses.reg_2), unlist(fi_dj1_ses.reg_3)), 
                    p=list(unlist(fi_dj1_pvals.reg_1), unlist(fi_dj1_pvals.reg_2), unlist(fi_dj1_pvals.reg_3)),
                    title = table_title, omit = c(5:7),
                    dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                    dep.var.caption = "Dependent variable: GDP per capita growth", 
                    type = "html", out = paste0("output/regressions_html/tab8-regs/", output_name, ".html"))

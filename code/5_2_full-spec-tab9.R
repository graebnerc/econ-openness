
full_spec_regs_new <- regressions_list[["full_spec"]]

full_1_kof_econ <- full_spec_regs_new$full_spec_kof_1y
full_1_kof_econ.ses <- list(full_spec_regs_new$full_spec_kof_1y_coeftest[,2])
full_1_kof_econ.tvals <- list(full_spec_regs_new$full_spec_kof_1y_coeftest[,3])
full_1_kof_econ.pvals <- list(full_spec_regs_new$full_spec_kof_1y_coeftest[,4])

full_fd_kof_econ <- full_spec_regs_new$full_spec_kof_1y_fd
full_fd_kof_econ.ses <- list(full_spec_regs_new$full_spec_kof_1y_fd_coeftest[,2])
full_fd_kof_econ.tvals <- list(full_spec_regs_new$full_spec_kof_1y_fd_coeftest[,3])
full_fd_kof_econ.pvals <- list(full_spec_regs_new$full_spec_kof_1y_fd_coeftest[,4])

full_5_kof_econ <- full_spec_regs_new$full_spec_kof_5y
full_5_kof_econ.ses <- list(full_spec_regs_new$full_spec_kof_5y_coeftest[,2])
full_5_kof_econ.tvals <- list(full_spec_regs_new$full_spec_kof_5y_coeftest[,3])
full_5_kof_econ.pvals <- list(full_spec_regs_new$full_spec_kof_5y_coeftest[,4])


full_1_kof_djdf <- full_spec_regs_new$full_spec_kof_dfdj_1y
full_1_kof_djdf.ses <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_coeftest[,2])
full_1_kof_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_coeftest[,3])
full_1_kof_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_coeftest[,4])

full_fd_kof_djdf <- full_spec_regs_new$full_spec_kof_dfdj_1y_fd
full_fd_kof_djdf.ses <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_fd_coeftest[,2])
full_fd_kof_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_fd_coeftest[,3])
full_fd_kof_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_dfdj_1y_fd_coeftest[,4])

full_5_kof_djdf <- full_spec_regs_new$full_spec_kof_dfdj_5y
full_5_kof_djdf.ses <- list(full_spec_regs_new$full_spec_kof_dfdj_5y_coeftest[,2])
full_5_kof_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_dfdj_5y_coeftest[,3])
full_5_kof_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_dfdj_5y_coeftest[,4])


full_1_kof_trfin <- full_spec_regs_new$full_spec_kof_trfin_1y
full_1_kof_trfin.ses <- list(full_spec_regs_new$full_spec_kof_trfin_1y_coeftest[,2])
full_1_kof_trfin.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_1y_coeftest[,3])
full_1_kof_trfin.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_1y_coeftest[,4])

full_fd_kof_trfin <- full_spec_regs_new$full_spec_kof_trfin_1y_fd
full_fd_kof_trfin.ses <- list(full_spec_regs_new$full_spec_kof_trfin_1y_fd_coeftest[,2])
full_fd_kof_trfin.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_1y_fd_coeftest[,3])
full_fd_kof_trfin.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_1y_fd_coeftest[,4])

full_5_kof_trfin <- full_spec_regs_new$full_spec_kof_trfin_5y
full_5_kof_trfin.ses <- list(full_spec_regs_new$full_spec_kof_trfin_5y_coeftest[,2])
full_5_kof_trfin.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_5y_coeftest[,3])
full_5_kof_trfin.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_5y_coeftest[,4])


full_1_kof_trfin_djdf <- full_spec_regs_new$full_spec_kof_trfin_dfdj_1y
full_1_kof_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_coeftest[,2])
full_1_kof_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_coeftest[,3])
full_1_kof_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_coeftest[,4])

full_fd_kof_trfin_djdf <- full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_fd
full_fd_kof_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_fd_coeftest[,2])
full_fd_kof_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_fd_coeftest[,3])
full_fd_kof_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_1y_fd_coeftest[,4])

full_5_kof_trfin_djdf <- full_spec_regs_new$full_spec_kof_trfin_dfdj_5y
full_5_kof_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_5y_coeftest[,2])
full_5_kof_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_5y_coeftest[,3])
full_5_kof_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_kof_trfin_dfdj_5y_coeftest[,4])


full_1_other_trfin_djdf <- full_spec_regs_new$full_spec_other_trfin_dfdj_1y
full_1_other_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_coeftest[,2])
full_1_other_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_coeftest[,3])
full_1_other_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_coeftest[,4])

full_fd_other_trfin_djdf <- full_spec_regs_new$full_spec_other_trfin_dfdj_1y_fd
full_fd_other_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_fd_coeftest[,2])
full_fd_other_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_fd_coeftest[,3])
full_fd_other_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_1y_fd_coeftest[,4])

full_5_other_trfin_djdf <- full_spec_regs_new$full_spec_other_trfin_dfdj_5y
full_5_other_trfin_djdf.ses <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_5y_coeftest[,2])
full_5_other_trfin_djdf.tvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_5y_coeftest[,3])
full_5_other_trfin_djdf.pvals <- list(full_spec_regs_new$full_spec_other_trfin_dfdj_5y_coeftest[,4])

table_title <- "Full specification"
output_name <- "full_specification"

stargazer(full_1_kof_econ, full_fd_kof_econ, full_5_kof_econ, 
          full_1_kof_trfin, full_fd_kof_trfin, full_5_kof_trfin,
          full_1_kof_djdf, full_fd_kof_djdf, full_5_kof_djdf,
          full_1_kof_trfin_djdf, full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
          full_1_other_trfin_djdf, full_fd_other_trfin_djdf, full_5_other_trfin_djdf,
          se=list(unlist(full_1_kof_econ.ses), unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                  unlist(full_1_kof_trfin.ses), unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                  unlist(full_1_kof_djdf.ses), unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                  unlist(full_1_kof_trfin_djdf.ses), unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses),
                  unlist(full_1_other_trfin_djdf.ses), unlist(full_fd_other_trfin_djdf.ses), unlist(full_5_other_trfin_djdf.ses)), 
          t=list(unlist(full_1_kof_econ.tvals), unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                 unlist(full_1_kof_trfin.tvals), unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                 unlist(full_1_kof_djdf.tvals), unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                 unlist(full_1_kof_trfin_djdf.tvals), unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals),
                 unlist(full_1_other_trfin_djdf.tvals), unlist(full_fd_other_trfin_djdf.tvals), unlist(full_5_other_trfin_djdf.tvals)), 
          p=list(unlist(full_1_kof_econ.pvals), unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                 unlist(full_1_kof_trfin.pvals), unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                 unlist(full_1_kof_djdf.pvals), unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                 unlist(full_1_kof_trfin_djdf.pvals), unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals),
                 unlist(full_1_other_trfin_djdf.pvals), unlist(full_fd_other_trfin_djdf.pvals), unlist(full_5_other_trfin_djdf.pvals)), 
          title = table_title, #omit = c(4:7),
          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
          dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          column.labels = rep(c("Yearly data", "FD", "5-year av"), 5),
          type = "text")

full_reg <- stargazer(full_1_kof_econ, full_fd_kof_econ, full_5_kof_econ, 
                          full_1_kof_trfin, full_fd_kof_trfin, full_5_kof_trfin,
                          full_1_kof_djdf, full_fd_kof_djdf, full_5_kof_djdf,
                          full_1_kof_trfin_djdf, full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
                          full_1_other_trfin_djdf, full_fd_other_trfin_djdf, full_5_other_trfin_djdf,
                          se=list(unlist(full_1_kof_econ.ses), unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                                  unlist(full_1_kof_trfin.ses), unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                                  unlist(full_1_kof_djdf.ses), unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                                  unlist(full_1_kof_trfin_djdf.ses), unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses),
                                  unlist(full_1_other_trfin_djdf.ses), unlist(full_fd_other_trfin_djdf.ses), unlist(full_5_other_trfin_djdf.ses)), 
                          t=list(unlist(full_1_kof_econ.tvals), unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                                 unlist(full_1_kof_trfin.tvals), unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                                 unlist(full_1_kof_djdf.tvals), unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                                 unlist(full_1_kof_trfin_djdf.tvals), unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals),
                                 unlist(full_1_other_trfin_djdf.tvals), unlist(full_fd_other_trfin_djdf.tvals), unlist(full_5_other_trfin_djdf.tvals)), 
                          p=list(unlist(full_1_kof_econ.pvals), unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                                 unlist(full_1_kof_trfin.pvals), unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                                 unlist(full_1_kof_djdf.pvals), unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                                 unlist(full_1_kof_trfin_djdf.pvals), unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals),
                                 unlist(full_1_other_trfin_djdf.pvals), unlist(full_fd_other_trfin_djdf.pvals), unlist(full_5_other_trfin_djdf.pvals)), 
                          title = table_title, #omit = c(4:7),
                          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                          dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
                          dep.var.caption = "Dependent variable: GDP per capita growth", 
                          column.labels = rep(c("Yearly data", "FD", "5-year av"), 5),
                          type = "html", out = paste0("output/regressions_html/tab9-regs/", output_name, ".html"))

# Full with only FD and 5 year averages ------
table_title <- "Full specification"
output_name <- "full_specification-fd-5y"
stargazer(full_fd_kof_econ, full_5_kof_econ, 
          full_fd_kof_trfin, full_5_kof_trfin,
          full_fd_kof_djdf, full_5_kof_djdf,
          full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
          full_fd_other_trfin_djdf, full_5_other_trfin_djdf,
          se=list(unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                  unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                  unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                  unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses),
                  unlist(full_fd_other_trfin_djdf.ses), unlist(full_5_other_trfin_djdf.ses)), 
          t=list(unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                 unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                 unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                 unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals),
                 unlist(full_fd_other_trfin_djdf.tvals), unlist(full_5_other_trfin_djdf.tvals)), 
          p=list(unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                 unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                 unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                 unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals),
                 unlist(full_fd_other_trfin_djdf.pvals), unlist(full_5_other_trfin_djdf.pvals)), 
          title = table_title, #omit = c(4:7),
          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
          dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          column.labels = rep(c("FD", "5-year av"), 5),
          type = "text")

full_reg <- stargazer(full_fd_kof_econ, full_5_kof_econ, 
                      full_fd_kof_trfin, full_5_kof_trfin,
                      full_fd_kof_djdf, full_5_kof_djdf,
                      full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
                      full_fd_other_trfin_djdf, full_5_other_trfin_djdf,
                      se=list(unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                              unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                              unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                              unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses),
                              unlist(full_fd_other_trfin_djdf.ses), unlist(full_5_other_trfin_djdf.ses)), 
                      t=list(unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                             unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                             unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                             unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals),
                             unlist(full_fd_other_trfin_djdf.tvals), unlist(full_5_other_trfin_djdf.tvals)), 
                      p=list(unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                             unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                             unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                             unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals),
                             unlist(full_fd_other_trfin_djdf.pvals), unlist(full_5_other_trfin_djdf.pvals)), 
                      title = table_title, #omit = c(4:7),
                      # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                      dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
                      dep.var.caption = "Dependent variable: GDP per capita growth", 
                      column.labels = rep(c("FD", "5-year av"), 5),
                      type = "html", out = paste0("output/regressions_html/tab9-regs/", output_name, ".html"))

# Full with only FD and 5 year averages and only with KOF ------
table_title <- "Full specification"
output_name <- "full_specification-fd-5y-kof-only"
stargazer(full_fd_kof_econ, full_5_kof_econ, 
          full_fd_kof_trfin, full_5_kof_trfin,
          full_fd_kof_djdf, full_5_kof_djdf,
          full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
          se=list(unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                  unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                  unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                  unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses)), 
          t=list(unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                 unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                 unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                 unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals)), 
          p=list(unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                 unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                 unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                 unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals)), 
          title = table_title, #omit = c(4:7),
          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
          dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          column.labels = rep(c("FD", "5-year av"), 5),
          type = "text")

full_reg <- stargazer(full_fd_kof_econ, full_5_kof_econ, 
                      full_fd_kof_trfin, full_5_kof_trfin,
                      full_fd_kof_djdf, full_5_kof_djdf,
                      full_fd_kof_trfin_djdf, full_5_kof_trfin_djdf,
                      se=list(unlist(full_fd_kof_econ.ses), unlist(full_5_kof_econ.ses), 
                              unlist(full_fd_kof_trfin.ses), unlist(full_5_kof_trfin.ses), 
                              unlist(full_fd_kof_djdf.ses), unlist(full_5_kof_djdf.ses), 
                              unlist(full_fd_kof_trfin_djdf.ses), unlist(full_5_kof_trfin_djdf.ses)), 
                      t=list(unlist(full_fd_kof_econ.tvals), unlist(full_5_kof_econ.tvals), 
                             unlist(full_fd_kof_trfin.tvals), unlist(full_5_kof_trfin.tvals),
                             unlist(full_fd_kof_djdf.tvals), unlist(full_5_kof_djdf.tvals), 
                             unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals)), 
                      p=list(unlist(full_fd_kof_econ.pvals), unlist(full_5_kof_econ.pvals), 
                             unlist(full_fd_kof_trfin.pvals), unlist(full_5_kof_trfin.pvals), 
                             unlist(full_fd_kof_djdf.pvals), unlist(full_5_kof_djdf.pvals), 
                             unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals)), 
                      title = table_title, #omit = c(4:7),
                      # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                      dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
                      dep.var.caption = "Dependent variable: GDP per capita growth", 
                      column.labels = rep(c("FD", "5-year av"), 5),
                      type = "html", out = paste0("output/regressions_html/tab9-regs/", output_name, ".html"))

# Full new only FD ----

table_title <- "Full specification (only FD)"
output_name <- "full_specification_fd"

full_reg_new <- stargazer(full_fd_kof_econ, 
                          full_fd_kof_trfin, 
                          full_fd_kof_djdf, 
                          full_fd_kof_trfin_djdf, 
                          # full_fd_other_trfin_djdf, 
                          se=list(unlist(full_fd_kof_econ.ses), 
                                  unlist(full_fd_kof_trfin.ses), 
                                  unlist(full_fd_kof_djdf.ses), 
                                  unlist(full_fd_kof_trfin_djdf.ses),
                                  unlist(full_fd_other_trfin_djdf.ses)), #unlist(full_5_other_trfin_djdf.ses)), 
                          t=list(unlist(full_fd_kof_econ.tvals), 
                                 unlist(full_fd_kof_trfin.tvals), 
                                 unlist(full_fd_kof_djdf.tvals)), #unlist(full_fd_kof_trfin_djdf.tvals)),
                          p=list(unlist(full_fd_kof_econ.pvals), 
                                 unlist(full_fd_kof_trfin.pvals), 
                                 unlist(full_fd_kof_djdf.pvals)),# unlist(full_fd_kof_trfin_djdf.pvals)),
                          title = table_title, #omit = c(4:7),
                          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                          dep.var.labels.include =  FALSE,  omit.stat = c("f"),
                          dep.var.caption = "Dependent variable: GDP per capita growth", 
                          # column.labels = rep(c("Yearly data", "FD", "5-year av"), 5),
                          type = "html", out = paste0("output/regressions_html/tab9-regs/", output_name, ".html"))

# Full new FD separated ----
table_title <- "Full specification (only FD)"
output_name <- "full_specification_fd-sep"

full_new_reg_sep <- stargazer(full_1_kof_econ, full_1_kof_trfin, full_1_kof_djdf,full_1_kof_trfin_djdf,full_1_other_trfin_djdf,
                              full_fd_kof_econ, full_fd_kof_trfin, full_fd_kof_djdf, full_fd_kof_trfin_djdf,full_fd_other_trfin_djdf,
                              full_5_kof_econ, full_5_kof_trfin, full_5_kof_djdf, full_5_kof_trfin_djdf,full_5_other_trfin_djdf,
                              se=list(unlist(full_1_kof_econ.ses), unlist(full_1_kof_trfin.ses), unlist(full_1_kof_djdf.ses), unlist(full_1_kof_trfin_djdf.ses), unlist(full_1_other_trfin_djdf.ses), 
                                      unlist(full_fd_kof_econ.ses), unlist(full_fd_kof_trfin.ses), unlist(full_fd_kof_djdf.ses), unlist(full_fd_kof_trfin_djdf.ses), unlist(full_fd_other_trfin_djdf.ses), 
                                      unlist(full_5_kof_econ.ses), unlist(full_5_kof_trfin.ses), unlist(full_5_kof_djdf.ses), unlist(full_5_kof_trfin_djdf.ses),unlist(full_5_other_trfin_djdf.ses)), 
                              t=list(unlist(full_1_kof_econ.tvals), unlist(full_1_kof_trfin.tvals), unlist(full_1_kof_djdf.tvals), unlist(full_1_kof_trfin_djdf.tvals), unlist(full_1_other_trfin_djdf.tvals), 
                                     unlist(full_fd_kof_econ.tvals), unlist(full_fd_kof_trfin.tvals), unlist(full_fd_kof_djdf.tvals), unlist(full_fd_kof_trfin_djdf.tvals), unlist(full_fd_other_trfin_djdf.tvals), 
                                     unlist(full_5_kof_econ.tvals), unlist(full_5_kof_trfin.tvals), unlist(full_5_kof_djdf.tvals), unlist(full_5_kof_trfin_djdf.tvals),unlist(full_5_other_trfin_djdf.tvals)), 
                              p=list(unlist(full_1_kof_econ.pvals), unlist(full_1_kof_trfin.pvals), unlist(full_1_kof_djdf.pvals), unlist(full_1_kof_trfin_djdf.pvals), unlist(full_1_other_trfin_djdf.pvals), 
                                     unlist(full_fd_kof_econ.pvals), unlist(full_fd_kof_trfin.pvals), unlist(full_fd_kof_djdf.pvals), unlist(full_fd_kof_trfin_djdf.pvals), unlist(full_fd_other_trfin_djdf.pvals), 
                                     unlist(full_5_kof_econ.pvals), unlist(full_5_kof_trfin.pvals), unlist(full_5_kof_djdf.pvals), unlist(full_5_kof_trfin_djdf.pvals),unlist(full_5_other_trfin_djdf.pvals)), 
                              title = table_title, #omit = c(4:7),
                              # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                              dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
                              dep.var.caption = "Dependent variable: GDP per capita growth", 
                              column.labels = c(rep("Yearly data", 5), rep("FD", 5), rep("5-year av", 5)),
                              type = "html", out = paste0("output/regressions_html/tab9-regs/", output_name, ".html"))

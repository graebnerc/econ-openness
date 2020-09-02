

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


# from 5_analyze_regs:


## HERE
# Full new classic design ----

table_title <- "Full specification (only FD)"
output_name <- "full_specification_fd-only"


# Concatenate all HTMLs ----
out_file_5 <- paste0("output/regressions_html/", "all_frames_5y", ".html")

write("<p>", file = out_file_5, append = FALSE)
write(tr_df5, file = out_file_5, append = TRUE)
write("</p>", file = out_file_5, append = TRUE)

write("<p>", file = out_file_5, append = TRUE)
write(tr_dj5, file = out_file_5, append = TRUE)
write("</p>", file = out_file_5, append = TRUE)

write("<p>", file = out_file_5, append = TRUE)
write(fi_df5, file = out_file_5, append = TRUE)
write("</p>", file = out_file_5, append = TRUE)

write("<p>", file = out_file_5, append = TRUE)
write(fi_dj5, file = out_file_5, append = TRUE)
write("</p>", file = out_file_5, append = TRUE)

write("<p>", file = out_file_5, append = TRUE)
write(full_reg, file = out_file_5, append = TRUE)
write("</p>", file = out_file_5, append = TRUE)


out_file_1 <- paste0("output/regressions_html/", "all_frames_1y", ".html")

write("<p>", file = out_file_1, append = FALSE)
write(tr_df1, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

write("<p>", file = out_file_1, append = TRUE)
write(tr_dj1, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

write("<p>", file = out_file_1, append = TRUE)
write(fi_df1, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

write("<p>", file = out_file_1, append = TRUE)
write(fi_dj1, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

write("<p>", file = out_file_1, append = TRUE)
write(full_reg, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

write("<p>", file = out_file_1, append = TRUE)
write(full_reg_paper, file = out_file_1, append = TRUE)
write("</p>", file = out_file_1, append = TRUE)

# 2. Make an integrated regression table to compare the four dimensions ----
# BIS HIER FD HINZUGEFUEGT

integrated_table_5y <- stargazer(tr_df5_reg_1, tr_df5_reg_2, tr_df5_reg_3, tr_df5_reg_4, 
                                 tr_dj5_reg_1, tr_dj5_reg_2, tr_dj5_reg_3, tr_dj5_reg_4, 
                                 fi_df5_reg_1, fi_df5_reg_2, fi_df5_reg_3, fi_df5_reg_4, 
                                 fi_dj5_reg_1, fi_dj5_reg_2, fi_dj5_reg_3, 
                                 t=list(unlist(tr_df5_tvals.reg_1), unlist(tr_df5_tvals.reg_2), unlist(tr_df5_tvals.reg_3), unlist(tr_df5_tvals.reg_4),
                                        unlist(tr_dj5_tvals.reg_1), unlist(tr_dj5_tvals.reg_2), unlist(tr_dj5_tvals.reg_3), unlist(tr_dj5_tvals.reg_4),
                                        unlist(fi_df5_tvals.reg_1), unlist(fi_df5_tvals.reg_2), unlist(fi_df5_tvals.reg_3), unlist(fi_df5_tvals.reg_4),
                                        unlist(fi_dj5_tvals.reg_1), unlist(fi_dj5_tvals.reg_2), unlist(fi_dj5_tvals.reg_3)), 
                                 se=list(unlist(tr_df5_ses.reg_1), unlist(tr_df5_ses.reg_2), unlist(tr_df5_ses.reg_3), unlist(tr_df5_ses.reg_4),
                                         unlist(tr_dj5_ses.reg_1), unlist(tr_dj5_ses.reg_2), unlist(tr_dj5_ses.reg_3), unlist(tr_dj5_ses.reg_4),
                                         unlist(fi_df5_ses.reg_1), unlist(fi_df5_ses.reg_2), unlist(fi_df5_ses.reg_3), unlist(fi_df5_ses.reg_4),
                                         unlist(fi_dj5_ses.reg_1), unlist(fi_dj5_ses.reg_2), unlist(fi_dj5_ses.reg_3)), 
                                 p=list(unlist(tr_df5_pvals.reg_1), unlist(tr_df5_pvals.reg_2), unlist(tr_df5_pvals.reg_3), unlist(tr_df5_pvals.reg_4),
                                        unlist(tr_dj5_pvals.reg_1), unlist(tr_dj5_pvals.reg_2), unlist(tr_dj5_pvals.reg_3), unlist(tr_dj5_pvals.reg_4),
                                        unlist(fi_df5_pvals.reg_1), unlist(fi_df5_pvals.reg_2), unlist(fi_df5_pvals.reg_3), unlist(fi_df5_pvals.reg_4),
                                        unlist(fi_dj5_pvals.reg_1), unlist(fi_dj5_pvals.reg_2), unlist(fi_dj5_pvals.reg_3)),
                                 title = "Overview - Five years averages", #omit = c(1, 17:21),
                                 dep.var.labels.include =  FALSE, omit.stat = c("adj.rsq"),
                                 dep.var.caption = "Dependent variable: GDP per capita growth", 
                                 type = "html", out = paste0("output/regressions_html/tab9-regs/", "integrated_table_5y_full", ".html"))

integrated_table_1y <- stargazer(tr_df1_reg_1, tr_df1_reg_2, tr_df1_reg_3, tr_df1_reg_4, 
                                 tr_dj1_reg_1, tr_dj1_reg_2, tr_dj1_reg_3, tr_dj1_reg_4, 
                                 fi_df1_reg_1, fi_df1_reg_2, fi_df1_reg_3, fi_df1_reg_4, 
                                 fi_dj1_reg_1, fi_dj1_reg_2, fi_dj1_reg_3, 
                                 t=list(unlist(tr_df1_tvals.reg_1), unlist(tr_df1_tvals.reg_2), unlist(tr_df1_tvals.reg_3), unlist(tr_df1_tvals.reg_4),
                                        unlist(tr_dj1_tvals.reg_1), unlist(tr_dj1_tvals.reg_2), unlist(tr_dj1_tvals.reg_3), unlist(tr_dj1_tvals.reg_4),
                                        unlist(fi_df1_tvals.reg_1), unlist(fi_df1_tvals.reg_2), unlist(fi_df1_tvals.reg_3), unlist(fi_df1_tvals.reg_4),
                                        unlist(fi_dj1_tvals.reg_1), unlist(fi_dj1_tvals.reg_2), unlist(fi_dj1_tvals.reg_3)), 
                                 se=list(unlist(tr_df1_ses.reg_1), unlist(tr_df1_ses.reg_2), unlist(tr_df1_ses.reg_3), unlist(tr_df1_ses.reg_4),
                                         unlist(tr_dj1_ses.reg_1), unlist(tr_dj1_ses.reg_2), unlist(tr_dj1_ses.reg_3), unlist(tr_dj1_ses.reg_4),
                                         unlist(fi_df1_ses.reg_1), unlist(fi_df1_ses.reg_2), unlist(fi_df1_ses.reg_3), unlist(fi_df1_ses.reg_4),
                                         unlist(fi_dj1_ses.reg_1), unlist(fi_dj1_ses.reg_2), unlist(fi_dj1_ses.reg_3)), 
                                 p=list(unlist(tr_df1_pvals.reg_1), unlist(tr_df1_pvals.reg_2), unlist(tr_df1_pvals.reg_3), unlist(tr_df1_pvals.reg_4),
                                        unlist(tr_dj1_pvals.reg_1), unlist(tr_dj1_pvals.reg_2), unlist(tr_dj1_pvals.reg_3), unlist(tr_dj1_pvals.reg_4),
                                        unlist(fi_df1_pvals.reg_1), unlist(fi_df1_pvals.reg_2), unlist(fi_df1_pvals.reg_3), unlist(fi_df1_pvals.reg_4),
                                        unlist(fi_dj1_pvals.reg_1), unlist(fi_dj1_pvals.reg_2), unlist(fi_dj1_pvals.reg_3)),
                                 title = "Overview - Annual data", #omit = c(16:20),
                                 dep.var.labels.include =  FALSE,  omit.stat = c("adj.rsq"),
                                 dep.var.caption = "Dependent variable: GDP per capita growth", 
                                 type = "html", out = paste0("output/regressions_html/", "integrated_table_1y_full", ".html"))


# 1.5. Full specification ----
full_spec_regs <- regressions_list[["full_spec"]]

full_reg_1y <- full_spec_regs$full_reg_1_1y
full_ses.reg_1y <- list(full_spec_regs$full_reg_1_1y_coeftest[,2])
full_tvals.reg_1y <- list(full_spec_regs$full_reg_1_1y_coeftest[,3])
full_pvals.reg_1y <- list(full_spec_regs$full_reg_1_1y_coeftest[,4])

full_reg_1y_fd <- full_spec_regs$full_reg_1_1y_fd
full_ses.reg_1y_fd <- list(full_spec_regs$full_reg_1_1y_fd_coeftest[,2])
full_tvals.reg_1y_fd <- list(full_spec_regs$full_reg_1_1y_fd_coeftest[,3])
full_pvals.reg_1y_fd <- list(full_spec_regs$full_reg_1_1y_fd_coeftest[,4])

full_reg_5y <- full_spec_regs$full_reg_1_5y
full_ses.reg_5y <- list(full_spec_regs$full_reg_1_5y_coeftest[,2])
full_tvals.reg_5y <- list(full_spec_regs$full_reg_1_5y_coeftest[,3])
full_pvals.reg_5y <- list(full_spec_regs$full_reg_1_5y_coeftest[,4])

full_reg_1y_unc_in_fd <- full_spec_regs$full_reg_1_1y_unc_in_fd
full_ses.reg_1y_unc_in_fd <- list(full_spec_regs$full_reg_1_1y_unc_in_fd_coeftest[,2])
full_tvals.reg_1y_unc_in_fd <- list(full_spec_regs$full_reg_1_1y_unc_in_fd_coeftest[,3])
full_pvals.reg_1y_unc_in_fd <- list(full_spec_regs$full_reg_1_1y_unc_in_fd_coeftest[,4])

full_reg_1y_unc_in <- full_spec_regs$full_reg_1_1y_unc_in
full_ses.reg_1y_unc_in <- list(full_spec_regs$full_reg_1_1y_unc_in_coeftest[,2])
full_tvals.reg_1y_unc_in <- list(full_spec_regs$full_reg_1_1y_unc_in_coeftest[,3])
full_pvals.reg_1y_unc_in <- list(full_spec_regs$full_reg_1_1y_unc_in_coeftest[,4])

full_reg_5y_unc_in <- full_spec_regs$full_reg_1_5y_unc_in
full_ses.reg_5y_unc_in <- list(full_spec_regs$full_reg_1_5y_unc_in_coeftest[,2])
full_tvals.reg_5y_unc_in <- list(full_spec_regs$full_reg_1_5y_unc_in_coeftest[,3])
full_pvals.reg_5y_unc_in <- list(full_spec_regs$full_reg_1_5y_unc_in_coeftest[,4])

full_reg_1y_lmf_open <- full_spec_regs$full_reg_1_1y_lmf_open
full_ses.reg_1y_lmf_open <- list(full_spec_regs$full_reg_1_1y_lmf_open_coeftest[,2])
full_tvals.reg_1y_lmf_open <- list(full_spec_regs$full_reg_1_1y_lmf_open_coeftest[,3])
full_pvals.reg_1y_lmf_open <- list(full_spec_regs$full_reg_1_1y_lmf_open_coeftest[,4])

full_reg_1y_lmf_open_fd <- full_spec_regs$full_reg_1_1y_lmf_open_fd
full_ses.reg_1y_lmf_open_fd <- list(full_spec_regs$full_reg_1_1y_lmf_open_fd_coeftest[,2])
full_tvals.reg_1y_lmf_open_fd <- list(full_spec_regs$full_reg_1_1y_lmf_open_fd_coeftest[,3])
full_pvals.reg_1y_lmf_open_fd <- list(full_spec_regs$full_reg_1_1y_lmf_open_fd_coeftest[,4])

full_reg_5y_lmf_open <- full_spec_regs$full_reg_1_5y_lmf_open
full_ses.reg_5y_lmf_open <- list(full_spec_regs$full_reg_1_5y_lmf_open_coeftest[,2])
full_tvals.reg_5y_lmf_open <- list(full_spec_regs$full_reg_1_5y_lmf_open_coeftest[,3])
full_pvals.reg_5y_lmf_open <- list(full_spec_regs$full_reg_1_5y_lmf_open_coeftest[,4])


table_title <- "Full specification"
output_name <- "full_specification"

stargazer(full_reg_1y, full_reg_1y_fd, full_reg_5y, full_reg_1y_unc_in, full_reg_1y_unc_in_fd, full_reg_5y_unc_in, full_reg_1y_lmf_open, full_reg_1y_lmf_open_fd, full_reg_5y_lmf_open,
          t=list(unlist(full_tvals.reg_1y), unlist(full_tvals.reg_1y_fd), unlist(full_tvals.reg_5y), unlist(full_tvals.reg_1y_unc_in), unlist(full_tvals.reg_1y_unc_in_fd), unlist(full_tvals.reg_5y_unc_in), unlist(full_tvals.reg_1y_lmf_open), unlist(full_tvals.reg_1y_lmf_open_fd), unlist(full_tvals.reg_5y_lmf_open)), 
          se=list(unlist(full_ses.reg_1y), unlist(full_ses.reg_1y_fd), unlist(full_ses.reg_5y), unlist(full_ses.reg_1y_unc_in), unlist(full_ses.reg_1y_unc_in_fd), unlist(full_ses.reg_5y_unc_in), unlist(full_ses.reg_1y_lmf_open), unlist(full_ses.reg_1y_lmf_open_fd), unlist(full_ses.reg_5y_lmf_open)), 
          p=list(unlist(full_pvals.reg_1y), unlist(full_pvals.reg_1y_fd), unlist(full_pvals.reg_5y), unlist(full_pvals.reg_1y_unc_in), unlist(full_pvals.reg_1y_unc_in_fd), unlist(full_pvals.reg_5y_unc_in), unlist(full_pvals.reg_1y_lmf_open), unlist(full_pvals.reg_1y_lmf_open_fd), unlist(full_pvals.reg_5y_lmf_open)),
          title = table_title, #omit = c(4:7),
          # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
          dep.var.caption = "Dependent variable: GDP per capita growth", 
          column.labels = rep(c("Yearly data", "5-year av"), 3),
          type = "text")

full_reg <- stargazer(full_reg_1y, full_reg_1y_fd, full_reg_5y, full_reg_1y_unc_in, full_reg_1y_unc_in_fd, full_reg_5y_unc_in, full_reg_1y_lmf_open, full_reg_1y_lmf_open_fd, full_reg_5y_lmf_open,
                      t=list(unlist(full_tvals.reg_1y), unlist(full_tvals.reg_1y_fd), unlist(full_tvals.reg_5y), unlist(full_tvals.reg_1y_unc_in), unlist(full_tvals.reg_1y_unc_in_fd), unlist(full_tvals.reg_5y_unc_in), unlist(full_tvals.reg_1y_lmf_open), unlist(full_tvals.reg_1y_lmf_open_fd), unlist(full_tvals.reg_5y_lmf_open)), 
                      se=list(unlist(full_ses.reg_1y), unlist(full_ses.reg_1y_fd), unlist(full_ses.reg_5y), unlist(full_ses.reg_1y_unc_in), unlist(full_ses.reg_1y_unc_in_fd), unlist(full_ses.reg_5y_unc_in), unlist(full_ses.reg_1y_lmf_open), unlist(full_ses.reg_1y_lmf_open_fd), unlist(full_ses.reg_5y_lmf_open)), 
                      p=list(unlist(full_pvals.reg_1y), unlist(full_pvals.reg_1y_fd), unlist(full_pvals.reg_5y), unlist(full_pvals.reg_1y_unc_in), unlist(full_pvals.reg_1y_unc_in_fd), unlist(full_pvals.reg_5y_unc_in), unlist(full_pvals.reg_1y_lmf_open), unlist(full_pvals.reg_1y_lmf_open_fd), unlist(full_pvals.reg_5y_lmf_open)),
                      title = table_title, #omit = c(4:7),
                      # notes = "Models (1), (3) and (5) build upon yearly data, models (2), (4) and (6) on 5-year averages.", 
                      dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                      dep.var.caption = "Dependent variable: GDP per capita growth", 
                      column.labels = rep(c("Yearly data", "5-year av"), 3),
                      type = "html", out = paste0("output/regressions_html/", output_name, ".html"))



# 10. Full specification - paper ----

table_title <- "Full specification"
output_name <- "full_specification_paper"

stargazer(              full_reg_5y,               full_reg_1y_fd,               full_reg_5y_unc_in,               full_reg_1y_unc_in_fd,               full_reg_5y_lmf_open,               full_reg_1y_lmf_open_fd,
                        t=list(unlist(full_tvals.reg_5y), unlist(full_tvals.reg_1y_fd), unlist(full_tvals.reg_5y_unc_in), unlist(full_tvals.reg_1y_unc_in_fd), unlist(full_tvals.reg_5y_lmf_open), unlist(full_tvals.reg_1y_lmf_open_fd)), 
                        se=list(unlist(full_ses.reg_5y), unlist(full_ses.reg_1y_fd), unlist(full_ses.reg_5y_unc_in), unlist(full_ses.reg_1y_unc_in_fd), unlist(full_ses.reg_5y_lmf_open), unlist(full_ses.reg_1y_lmf_open_fd)), 
                        p=list(unlist(full_pvals.reg_5y), unlist(full_pvals.reg_1y_fd), unlist(full_pvals.reg_5y_unc_in), unlist(full_pvals.reg_1y_unc_in_fd), unlist(full_pvals.reg_5y_lmf_open), unlist(full_pvals.reg_1y_lmf_open_fd)),
                        title = table_title, #omit = c(4:7),
                        notes = "Models (1), (3) and (5) build upon 5-year averages, models (2), (4) and (6) on yearly data and FD estimation", 
                        dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                        dep.var.caption = "Dependent variable: GDP per capita growth", 
                        #column.labels = rep(c("Yearly data", "5-year av"), 3),
                        type = "text")

full_reg_paper <- stargazer(              full_reg_5y,               full_reg_1y_fd,               full_reg_5y_unc_in,               full_reg_1y_unc_in_fd,               full_reg_5y_lmf_open,               full_reg_1y_lmf_open_fd,
                                          t=list(unlist(full_tvals.reg_5y), unlist(full_tvals.reg_1y_fd), unlist(full_tvals.reg_5y_unc_in), unlist(full_tvals.reg_1y_unc_in_fd), unlist(full_tvals.reg_5y_lmf_open), unlist(full_tvals.reg_1y_lmf_open_fd)), 
                                          se=list(unlist(full_ses.reg_5y), unlist(full_ses.reg_1y_fd), unlist(full_ses.reg_5y_unc_in), unlist(full_ses.reg_1y_unc_in_fd), unlist(full_ses.reg_5y_lmf_open), unlist(full_ses.reg_1y_lmf_open_fd)), 
                                          p=list(unlist(full_pvals.reg_5y), unlist(full_pvals.reg_1y_fd), unlist(full_pvals.reg_5y_unc_in), unlist(full_pvals.reg_1y_unc_in_fd), unlist(full_pvals.reg_5y_lmf_open), unlist(full_pvals.reg_1y_lmf_open_fd)),
                                          title = table_title, #omit = c(4:7),
                                          notes = "Models (1), (3) and (5) build upon 5-year averages, models (2), (4) and (6) on yearly data and FD estimation", 
                                          dep.var.labels.include =  FALSE, # omit.stat = c("adj.rsq"),
                                          dep.var.caption = "Dependent variable: GDP per capita growth", 
                                          #column.labels = rep(c("Yearly data", "5-year av"), 3),
                                          type = "html", out = paste0("output/regressions_html/", output_name, ".html"))

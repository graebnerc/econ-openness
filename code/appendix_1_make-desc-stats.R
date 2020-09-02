rm(list = ls()) 
# library(tidyverse)
# library(xtable)
# library(haven)
original_data_ext <- fread(here("data/openness_1y.csv"))
original_data_ext_5_year <- fread(here("data/openness_5y.csv"))

original_data_ext_sum <- original_data_ext %>%
  dplyr::select(-one_of("ComplexityGroup")) %>%
  tidyr::gather(variable, value, -ccode, -Year) %>%
  dplyr::mutate(value=as.double(value)) %>%
  dplyr::filter(!is.na(value))

original_data_ext_sum_2 <- original_data_ext_sum %>%
  dplyr::group_by(ccode, variable) %>%
  dplyr::summarize(total=round(sum(!is.na(value)), digits = 0),
                   mean=round(mean(value, na.rm=T), digits = 2) ,
                   sd=round(sd(value, na.rm=T), digits = 2)) %>%
  dplyr::ungroup()

head(original_data_ext_sum)

original_data_ext_sum_3 <- original_data_ext_sum_2 %>%
  tidyr::gather(measure, val, -ccode, -variable) %>%
  tidyr::spread(variable, val)

original_data_ext_sum_3 <- original_data_ext_sum_3[,order(colnames(original_data_ext_sum_3))] %>%
  dplyr::select(ccode, measure, everything())

original_data_ext_sum_3_1 <- original_data_ext_sum_3 %>%
  dplyr::select(1:2, 3:14)
original_data_ext_sum_3_2 <- original_data_ext_sum_3 %>%
  dplyr::select(1:2, 15:26)
original_data_ext_sum_3_3 <- original_data_ext_sum_3 %>%
  dplyr::select(1:2, 27:38)

latex_file <- "output/appendix/desc_sta_1.tex"
original_data_ext_sum_3_1_la <- xtable(original_data_ext_sum_3_1, align = rep("c", ncol(original_data_ext_sum_3_1)+1), digits = 2)
print.xtable(x = original_data_ext_sum_3_1_la, file = latex_file, floating = FALSE,
             tabular.environment="longtable", include.rownames = FALSE, booktabs = TRUE, size = 6,
             caption.placement = "bottom", caption = "FUCK")
latex_file <- "output/appendix/desc_sta_2.tex"
original_data_ext_sum_3_2_la <- xtable(original_data_ext_sum_3_2, align = rep("c", ncol(original_data_ext_sum_3_2)+1), digits = 2)
print.xtable(x = original_data_ext_sum_3_2_la, file = latex_file, floating = FALSE,
             tabular.environment="longtable", include.rownames = FALSE, booktabs = TRUE, size = 6,
             caption.placement = "bottom", caption = "FUCK")
latex_file <- "output/appendix/desc_sta_3.tex"
original_data_ext_sum_3_3_la <- xtable(original_data_ext_sum_3_3, align = rep("c", ncol(original_data_ext_sum_3_3)+1), digits = 2)
print.xtable(x = original_data_ext_sum_3_3_la, file = latex_file, floating = FALSE,
             tabular.environment="longtable", include.rownames = FALSE, booktabs = TRUE, size = 6,
             caption.placement = "bottom", caption = "FUCK")

# Alternative: more coarse grained descriptives -----

variable_level <- original_data_ext_sum %>%
  dplyr::group_by(variable) %>%
  dplyr::summarize(`Observations`=round(sum(!is.na(value)), digits = 0),
                   Mean=round(mean(value, na.rm=T), digits = 2) ,
                   `Standard deviation`=round(sd(value, na.rm=T), digits = 2)) %>%
  dplyr::rename(Variable=variable) %>%
  dplyr::ungroup()

latex_file <- "output/appendix/desc_sta_var_level.tex"
variable_level_latex <- xtable(variable_level, align = rep("c", ncol(variable_level)+1), digits = c(0,0,0,2,4))
print.xtable(x = variable_level_latex, file = latex_file, floating = FALSE,
             tabular.environment="tabular", include.rownames = FALSE, booktabs = TRUE, size = 6)

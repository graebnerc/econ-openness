
rm(list = ls()) 

library(CADFtest)
library(countrycode)
library(plm)
library(punitroots)
library(tidyverse)
library(xtable)

source("code/appendix_2_adf_cadf-func.R")

report_progress <- F # set to true to get info about current tests

reg_data <- readRDS("data/sec5_regdata.rds")

original_data_ext <- reg_data$original_data_ext %>%
  dplyr::mutate(Country=countrycode(ccode, "iso3c", "country.name")) %>%
  dplyr::filter(!is.na(Country))


cou_of_interest <- unique(original_data_ext$ccode)
ind_of_interest <- c("Lietal", "Trade_to_GDP", "Alcala", "CTS", "KOF_defacto", "KOF_dejure", "KOF_econ", "Frankel",
                     "LMF_open", 
                     "LMF_EQ", 
                     "UNC_in_GDP", 
                     "UNC_out_GDP", 
                     "FTI_Index_ipo",
                     "FTI_trade_ipo",
                     "ln_FTI_Index_ipo", 
                     "Tariff_WITS_ipo", 
                     "HF_trade", 
                     "KAOPEN", 
                     "HF_fin", 
                     "CAPITAL",
                     "TOI",
                     "UNC_FDI_out",
                     "UNC_FDI_in")

get_stars <- function(x){
  ret_val <- ifelse(x<0.01, "***", 
                    ifelse(x<0.05, "**",
                           ifelse(x<0.1, "*", ".")))
  return(ret_val)
}

country_ts_list <- list()
country_info_list <- list()
info_frame <- data.frame(country=character(), var_name=character(),
                         data_start=double(), data_end=double(), share_na=double())

for (v in ind_of_interest){
  var_used <- v
  if (report_progress){print(var_used)}
  subset_data <- original_data_ext %>%
    dplyr::select(one_of(c(c("Year", "ccode"), var_used))) %>%
    tidyr::spread(ccode, 3) %>%
    dplyr::select(-Year)
  for (cou in cou_of_interest){
    if (report_progress){print(cou)}
    if (sum(!is.na(subset_data[cou]))<=10){
      if (report_progress){print("only NA")}
      inter_frame <- data.frame(country=cou, var_name=var_used,
                                data_start=NA, data_end=NA, share_na=NA,
                                adf_pval=NA, sig=NA)
    } else if (var(subset_data[cou], na.rm=T)[1]<0.0001){
      if (report_progress){print("no variance")}
      inter_frame <- data.frame(country=cou, var_name=var_used,
                                data_start=NA, data_end=NA, share_na=NA,
                                adf_pval=NA, sig=NA)
    } else{
      if (report_progress){print("data exists")}
      curr_ts <- na.trim(zoo(ts(subset_data[cou], start = 1960, frequency = 1)))
      # adf <- CADFtest(curr_ts)
      adf <- make_cadf(curr_ts, cou, var_used)
      adf_p_value <- round(adf$p.value, digits = 3)
      adf_stars <- get_stars(adf$p.value)
      country_ts_list[[cou]][[var_used]] <- na.approx(curr_ts, maxgap=3)
      inter_frame <- data.frame(country=cou, var_name=var_used,
                                data_start=start(curr_ts), data_end=end(curr_ts), share_na=round((sum(is.na(curr_ts)) / length(curr_ts))*100, digits = 3),
                                adf_pval=adf_p_value, sig=adf_stars)
    }
    info_frame <- rbind(info_frame, inter_frame)
  }
}
latex_file <- "output/appendix/adf_table.tex"
info_frame_tex <- xtable(info_frame, align = "cccccccc", digits = c(0,0,0,0,0, 3, 3, 0))
print.xtable(x = info_frame_tex, file = latex_file, floating = FALSE,
             tabular.environment="longtable", include.rownames = FALSE, booktabs = TRUE,
             caption.placement = "bottom", caption = "AF tests")

rm(list=ls())
source("code/3_correl-matrices-functions.R")

openess_indices_of_interest <- c(
  "EXP_to_GDP", "IMP_to_GDP", "Trade_to_GDP", "FTI_original_ipo", "Tariff_RES",
  "HF_trade", "chinn_ito_normed", "CAPITAL", "FIN_CUR", "HF_fin", 
  "KOF_econ", "KOF_defacto", 
  "KOF_dejure", "LMF_EQ", "LMF_EQ_gdp", "LMF_open", "LMF_open_gdp", 
  "LMF_FDI_total_stocks", "LMF_FDI_total_stocks_GDP", "LMF_open_pv",
  "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP",
  "Alcala", "Lietal", "CTS", "TOI",
  "Tariff_WITS_ipo",  
  "UNC_FDI_out_stock_GDP", "UNC_FDI_in_stock_GDP", "UNC_FDI_total_stocks_GDP", 
  "KOF_econ", "KOF_defacto", "KOF_finance_df", "KOF_trade_df", "KOF_finance", "KOF_trade",
  "KOF_dejure", "KOF_finance_dj", "KOF_trade_dj") 

indices_groups <- list()

index_vars <- c("Country", "Year")

indices_groups[["de_jure"]] <- c(
  "KOF_dejure", "KOF_trade_dj", "FTI_original_ipo", "Tariff_WITS_ipo", "Tariff_RES", 
  "HF_trade", "chinn_ito_normed", "CAPITAL", "FIN_CUR", "HF_fin", "KA_Index", "KOF_finance_dj")

indices_groups[["de_facto"]] <- c(
  "KOF_econ", "CTS", "TOI", "Alcala", "Trade_to_GDP", "EXP_to_GDP", "IMP_to_GDP",
  "Lietal",  
  "KOF_trade_df", "KOF_defacto", "KOF_finance_df",
  "LMF_EQ", "LMF_EQ_gdp", "LMF_FDI_total_stocks", "LMF_FDI_total_stocks_GDP", 
  "LMF_open", "LMF_open_gdp", "LMF_open_pv",
  "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP", "UNC_FDI_total_stocks_GDP"
  )

indices_groups[["trade"]] <- c(
  "EXP_to_GDP", "IMP_to_GDP", "Trade_to_GDP", "FTI_original_ipo",
  "Tariff_WITS_ipo", "Tariff_RES", "HF_trade", "Alcala", "Lietal", "CTS", "TOI", 
  "KOF_econ", "KOF_trade_df", "KOF_trade_dj")

indices_groups[["financial"]] <- c(
  "chinn_ito_normed", "CAPITAL", "FIN_CUR", "HF_fin", "KA_Index",
  "KOF_econ", "KOF_finance_df", "KOF_finance_dj", 
  "LMF_EQ", "LMF_open", "LMF_EQ_gdp", "LMF_open_gdp",
  "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP", "UNC_FDI_total_stocks_GDP"
  )

indices_groups[["all"]] <- c(
  indices_groups[["de_facto"]], indices_groups[["de_jure"]]) 


# Import the data ----
reg_data <- fread(here("data/openness_1y.csv"))
raw_data <- reg_data %>% 
  dplyr::mutate(
    Country=countrycode::countrycode(ccode, "iso3c", "country.name")
    ) %>%
  dplyr::filter(!is.na(Country)) %>%
  dplyr::select(one_of(c("Country", "Year", openess_indices_of_interest))) 


# Filter data as in the regressions:
list_of_dependent_variables <- c(
  "Trade_to_GDP", "Alcala", "Lietal", "CTS", "KOF_defacto", "TOI", "KOF_dejure", 
  "FTI_original_ipo", "Tariff_WITS_ipo", "HF_trade", "chinn_ito_normed", "HF_fin", 
  "LMF_open_gdp", "LMF_EQ_gdp", "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP"
)
raw_data_red <- raw_data %>%
  filter_at(vars(list_of_dependent_variables), all_vars(!is.na(.)))

# Add the differences -----

year_old <- raw_data$Year
raw_data_diff <- raw_data %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(across(.cols = everything(), .fns = ~ . - dplyr::lag(.) )) %>%
  dplyr::ungroup()
raw_data_diff$Year <- year_old

year_old_red <- raw_data_red$Year
raw_data_red_diff <- raw_data_red %>%
  dplyr::group_by(Country) %>%
  dplyr::mutate(across(.cols = everything(), .fns = ~ . - dplyr::lag(.) )) %>%
  dplyr::ungroup()
raw_data_red_diff$Year <- year_old_red

cols_order <- c(
  "Country", "Year",
  "KOF_econ", "KOF_defacto", "KOF_dejure", "KOF_finance", "KOF_trade", 
  "KOF_trade_df", "Trade_to_GDP", "EXP_to_GDP", "IMP_to_GDP", "Alcala", "Lietal", "TOI", "CTS",  
  "KOF_trade_dj", "Tariff_WITS_ipo", "FTI_original_ipo", "Tariff_RES", 
  "KOF_finance_df", "LMF_EQ", "LMF_EQ_gdp", "LMF_FDI_total_stocks", 
  "LMF_FDI_total_stocks_GDP", "LMF_open", "LMF_open_gdp", "LMF_open_pv",
  "UNC_FDI_in_stock_GDP", "UNC_FDI_out_stock_GDP", "UNC_FDI_total_stocks_GDP",
  "KOF_finance_dj", "chinn_ito_normed", "CAPITAL", "HF_trade", "FIN_CUR", "HF_fin"
)
raw_data <- raw_data[, ..cols_order]
raw_data_diff <- raw_data_diff[, cols_order]

rounding_value <- 1
text_size_coeficients <- 4 
size_of_axis_text <- 10 

# Spearman correlations of levels ----
correlation_method <- "spearman"

spearman_levels_lower <- get_heatmap(
  data_used = raw_data, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
spearman_levels_lower
ggsave("output/correlation_matrices/figure_4_spearman_levels_red.pdf",
       height = 12, width = 12)

spearman_levels_lower_red_sample <- get_heatmap(
  data_used = raw_data_red, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
spearman_levels_lower_red_sample
ggsave("output/correlation_matrices/figure_4_spearman_levels_red_red_sample.pdf",
       height = 12, width = 12)

# Spearman correlations of differences ----
correlation_method <- "spearman"

spearman_diffs_lower <- get_heatmap(
  data_used = raw_data_diff, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
spearman_diffs_lower
ggsave("output/correlation_matrices/figure_5_spearman_diffs_red.pdf", 
       height = 12, width = 12)

correlation_method <- "spearman"

spearman_diffs_lower_red_sample <- get_heatmap(
  data_used = raw_data_red_diff, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
spearman_diffs_lower_red_sample
ggsave("output/correlation_matrices/figure_5_spearman_diffs_red_red_sample.pdf", 
       height = 12, width = 12)


# Pearson correlations of levels ----
correlation_method <- "pearson"

pearson_levels_lower <- get_heatmap(
  data_used = raw_data, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
pearson_levels_lower
ggsave(here("appendix/figure_4A_pearson_levels_red.pdf"), 
       height = 12, width = 12)

# Pearson correlations of differences ----
correlation_method <- "pearson"

pearson_diffs_lower <- get_heatmap(
  data_used = raw_data_diff, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
pearson_diffs_lower
ggsave(here("appendix/figure_5A_pearson_diffs_red.pdf"), 
       height = 12, width = 12)



# Pearson correlations of levels ----
correlation_method <- "pearson"

pearson_levels_lower_red_sample <- get_heatmap(
  data_used = raw_data_red, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
pearson_levels_lower_red_sample
ggsave(here("appendix/figure_4A_pearson_levels_red_red_sample.pdf"), 
       height = 12, width = 12)

# Pearson correlations of differences ----
correlation_method <- "pearson"

pearson_diffs_lower <- get_heatmap(
  data_used = raw_data_red_diff, 
  cor_method_used = correlation_method, 
  text_size_coefs = text_size_coeficients, 
  axis_text_size = size_of_axis_text, 
  rounding_val = rounding_value, 
  upper_only = T)
pearson_diffs_lower
ggsave(here("appendix/figure_5A_pearson_diffs_red_red_sample.pdf"), 
       height = 12, width = 12)

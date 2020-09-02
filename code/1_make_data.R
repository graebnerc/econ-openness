# Creates the tidy data set

rm(list = ls())
down_wb <- FALSE # Set true to download world bank data

# Import Florians data -----
# This data has been gathered by Florian Springholz

Florian_data <- fread("data/openness-data-raw.csv")

vars_of_interest <- c(
  "ComplexityGroup",
  "countrycode",
  "Year",
  "Frankel",
  "LMF_open",
  "LMF_open_pv",
  "LMF_EQ",
  "LMF_FDI_in",
  "LMF_in_GDP",
  "LMF_FDI_out",
  "LMF_out_GDP",
  "LMF_FDI_total_stocks_GDP",
  "UNC_in_GDP",
  "UNC_out_GDP",
  "UNC_FDI_out",
  "UNC_FDI_in",
  "UNC_FDI_total_stocks_GDP",
  "FTI_Index_original",
  "FTI_Index_ipo",
  "FTI_trade",
  "FTI_trade_ipo",
  "ln_FTI_Index_ipo",
  # "FTI_Index_original",
  "Tariff_WITS",
  "Tariff_WITS_ipo",
  "HF_trade",
  "KAOPEN",
  "HF_fin",
  "CAPITAL",
  "TOI",
  "X_GDP",
  "M_GDP",
  "Tariff_RES",
  "FIN_CUR",
  "KA_Index",
  "IncomeGroup"
)

Florian_data_red <- dplyr::select(
  Florian_data,
  one_of(vars_of_interest)
) %>%
  dplyr::filter(countrycode != "") %>%
  dplyr::mutate(Frankel = Frankel * -1)

# Add additional data ----
penn_world <- read_stata("data/pwt90.dta") %>%
  dplyr::select(one_of("countrycode", "year", "hc", "rgdpo", "csh_i")) %>%
  dplyr::filter(countrycode %in% unique(Florian_data_red$countrycode))

kof_data <- read_stata("data/KOF_Data_2018.dta") %>%
  dplyr::select(one_of(
    "code", "year",
    "KOFEcGI", # KOF_econ
    "KOFEcGIdf", # KOF_defacto
    "KOFEcGIdj", # KOF_dejure
    "KOFTrGI", # KOF_trade
    "KOFTrGIdf", # KOF_trade_df
    "KOFTrGIdj", # KOF_trade_dj
    "KOFFiGI", # KOF_finance
    "KOFFiGIdf", # KOF_finance_df
    "KOFFiGIdj" # KOF_finance_dj
  )) %>% 
  dplyr::rename(
    KOF_econ = KOFEcGI,
    KOF_defacto = KOFEcGIdf,
    KOF_dejure = KOFEcGIdj,
    KOF_trade = KOFTrGI,
    KOF_trade_df = KOFTrGIdf,
    KOF_trade_dj = KOFTrGIdj,
    KOF_finance= KOFFiGI,
    KOF_finance_df = KOFFiGIdf,
    KOF_finance_dj = KOFFiGIdj
  )

if (down_wb == TRUE) {
  library(WDI)
  world_bank_raw <- WDI::WDI(
    country = countrycode(unique(Florian_data_red$countrycode), 
                          "iso3c", "iso2c"),
    indicator = c("FP.CPI.TOTL.ZG", "NE.TRD.GNFS.ZS", "SP.POP.TOTL"),
    start = 1950, end = 2017
  )
  world_bank <- world_bank_raw %>%
    dplyr::rename(
      inflation = FP.CPI.TOTL.ZG,
      Trade_to_GDP = NE.TRD.GNFS.ZS,
      population = SP.POP.TOTL
    ) %>% #
    dplyr::mutate(iso3c = countrycode(iso2c, "iso2c", "iso3c")) %>%
    dplyr::select(-iso2c, -country) %>%
    dplyr::mutate(year = as.numeric(year))
  fwrite(world_bank, "data/wdi-data.csv")
} else {
  world_bank <- fread("data/wdi-data.csv") %>%
    dplyr::mutate(year = as.numeric(year))
}
indicator_data <- fread("data/Tang_CES_data.csv") %>%
  dplyr::select(one_of("year", "ccode", "CTS", "Lietal", "Alcala")) %>%
  dplyr::mutate(year = as.numeric(year))

additional_data <- full_join(
  penn_world, kof_data, by = c("countrycode" = "code", "year" = "year")
  ) %>%
  full_join(
    ., world_bank, by = c("countrycode" = "iso3c", "year" = "year")
    ) %>%
  dplyr::rename(
    ccode = countrycode,
    Year = year
  ) %>%
  dplyr::mutate(Year = as.numeric(Year)) %>%
  full_join(., indicator_data, by = c("ccode" = "ccode", "Year" = "year"))


# check duplicates (if latter data frame is empty, no duplicates)
dupe <- additional_data[, c("ccode", "Year")] # select columns to check duplicates
additional_data[duplicated(dupe) | duplicated(dupe, fromLast = TRUE), ]

# nb ob observations
# shares na
add_data_info <- list()
for (i in names(additional_data)) {
  add_data_info[[i]][["n_obs"]] <- sum(!is.na(additional_data[i]))
  add_data_info[[i]][["share_nonna"]] <- sum(
    !is.na(additional_data[i])) / nrow(additional_data[i])
}
availability <- as.data.frame(add_data_info)
availability

# Merge data ----

openness_data_v1 <- additional_data %>%
  left_join(., Florian_data_red, 
            by = c("Year" = "Year", "ccode" = "countrycode")
            ) %>%
  dplyr::rename(
    IMP_to_GDP = M_GDP,
    EXP_to_GDP = X_GDP,
    FTI_Index = FTI_Index_original
  ) %>%
  dplyr::mutate(
    Penn_GDP_PPP = rgdpo / (population * 1000), # GDP per capita at PPP
    Penn_GDP_PPP_log = log(Penn_GDP_PPP), # log of GDP per capita at PPP
    pop_log = log(population)
  ) # log of population

openness_data_v2 <- plyr::ddply(
  openness_data_v1, "ccode", transform, # calculate GDP per capita growth rate (by taking difference of log)
  GDP_pc_growth = c(NA, diff(Penn_GDP_PPP_log) * 100),
  pop_growth = c(NA, diff(pop_log) * 100)
) %>%
  dplyr::mutate(inv_share = csh_i * 100) %>% # investment share
  dplyr::mutate(GDP_pc_growth = ifelse(
    abs(GDP_pc_growth) < 30, GDP_pc_growth, NA)
    ) %>% # eliminate outliers in GDP
  dplyr::select(-csh_i)

add_data_info_open_v2 <- list()
for (i in names(openness_data_v2)) {
  add_data_info_open_v2[[i]][["n_obs"]] <- sum(!is.na(openness_data_v2[i]))
  add_data_info_open_v2[[i]][["share_nonna"]] <- sum(
    !is.na(openness_data_v2[i])) / nrow(openness_data_v2[i])
}
availability_open_v2 <- as.data.frame(add_data_info_open_v2)
availability_open_v2

# 5yr averages ----
year_added <-
  openness_data_v3 <- openness_data_v2 %>%
  dplyr::filter(
    Year > 1959,
    Year < 2017
  )
openness_data_v3$period <- cut(
  openness_data_v3$Year, seq(1900, 2100, 5)) # set 5 year periods

openness_data_v3_5y <- openness_data_v3 %>%
  group_by(ccode, period) %>%
  summarise_if(is.numeric, mean, na.rm = T) %>%
  ungroup() %>%
  dplyr::mutate(first_year = ifelse(Year == 1960, 1960,
    ifelse(Year == 2016, 2016,
      ifelse(Year == 1970, 1966, Year - 2)
    )
  ))

openness_data_v3_5y_first_year_gdp <- openness_data_v3 %>%
  dplyr::filter(Year %in% unique(openness_data_v3_5y$first_year)) %>%
  dplyr::select(one_of("ccode", "Penn_GDP_PPP", "Year")) %>%
  dplyr::rename(initial_GDP_pc = Penn_GDP_PPP)

openness_data_v3_5y <- left_join(openness_data_v3_5y, 
                                 openness_data_v3_5y_first_year_gdp, 
                                 by = c("ccode", "first_year" = "Year"))

openness_data_v4_5y <- openness_data_v3_5y %>%
  dplyr::mutate(
    Lietal = Lietal + 1,
    LMF_EQ = LMF_EQ + 1,
    KAOPEN = KAOPEN + 1
  )

openness_data_v4 <- openness_data_v3 %>%
  dplyr::mutate(
    Lietal = Lietal + 1,
    LMF_EQ = LMF_EQ + 1,
    KAOPEN = KAOPEN + 1
  )

# Save final data ----

reg_data <- list(
  "original_data_ext" = openness_data_v4,
  "original_data_ext_5_year" = openness_data_v4_5y
)

saveRDS(reg_data, "data/regdata.rds")

fwrite(openness_data_v4, "data/final_openness_data_1y.csv")
fwrite(openness_data_v4_5y, "data/final_openness_data_5y.csv")

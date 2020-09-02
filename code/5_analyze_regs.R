# Analyze the regressions as created in file make_regs_s5
# Correspondance: claudius@claudius-graebner.com

rm(list = ls())
regressions_list <- readRDS("output/sec5_regressions.rds")

# 1. Regression tables underlying table 8 =====================================
source("code/5_1_single-reg-tables.R")

# 2. Re-creation of table 9 ===================================================
source("code/5_2_full-spec-tab9.R")

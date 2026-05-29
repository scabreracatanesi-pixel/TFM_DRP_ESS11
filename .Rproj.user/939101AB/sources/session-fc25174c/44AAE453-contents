# ============================================================
# 02_curate_IVs.R
# Curate IVs (age, gender, subjective economic insecurity)
# Input:  data/ess11_party_vote_drp.rds
# Output: data/ess11_18_all_curated.rds
#         data/ess11_18_eu_curated.rds
# ============================================================

rm(list = ls())

library(dplyr)

# ------------------------------------------------------------
# Paths
# ------------------------------------------------------------

input_path <- "data/ess11_party_vote_drp.rds"
out_all    <- "data/ess11_18_all_curated.rds"
out_eu     <- "data/ess11_18_eu_curated.rds"

# ------------------------------------------------------------
# EU countries (ISO2)
# ------------------------------------------------------------

eu_iso2 <- c(
  "AT","BE","BG","HR","CY","CZ","DK","EE","FI","FR","DE","GR","HU",
  "IE","IT","LV","LT","LU","MT","NL","PL","PT","RO","SK","SI","ES","SE"
)

# ============================================================
# 1) Load data
# ============================================================

ess3 <- readRDS(input_path)

# ============================================================
# 2) Filter: age >= 18
# ============================================================

ess_18 <- ess3 %>%
  filter(!is.na(agea), agea >= 18)

# ============================================================
# 3) Curate IVs
# ============================================================

ess_18 <- ess_18 %>%
  mutate(
    # gender binary (0 = male, 1 = female)
    gndr01 = case_when(
      gndr == 1 ~ 0,
      gndr == 2 ~ 1,
      TRUE      ~ NA_real_
    ),
    
    # subjective economic insecurity
    precariedad = as.numeric(hincfel),
    
    # education (years), conservative trimming
    eduyrs_clean = if_else(as.numeric(eduyrs) > 30, NA_real_, as.numeric(eduyrs)),
    
    # mean-centered age
    agea_c = as.numeric(scale(agea, center = TRUE, scale = FALSE))
  )

# ============================================================
# 4) EU subset
# ============================================================

ess_18_ue <- ess_18 %>%
  filter(cntry %in% eu_iso2)

# ============================================================
# 5) Save outputs
# ============================================================

saveRDS(ess_18,    out_all)
saveRDS(ess_18_ue, out_eu)

# ============================================================
# Output objects:
# - ess_18
# - ess_18_ue
# ============================================================

# ============================================================
# 03_models_vote_drp_robust.R
# Modelos LPM y Logit con errores robustos (HC1)
# ============================================================

rm(list = ls())

# --------------------
# Paquetes
# --------------------
library(dplyr)
library(lmtest)
library(sandwich)

# --------------------
# Función robusta (HC1)
# --------------------
robust <- function(model) {
  lmtest::coeftest(model, vcov. = sandwich::vcovHC(model, type = "HC1"))
}

# --------------------
# Cargar datos curados
# --------------------
df_all <- readRDS("data/ess11_18_all_curated.rds")
df_ue  <- readRDS("data/ess11_18_eu_curated.rds")

# ============================================================
# 1) MODELOS LINEALES (LPM)
# ============================================================

# Modelo 0
m0_lpm_all <- lm(vote_drp ~ precariedad, data = df_all)
m0_lpm_ue  <- lm(vote_drp ~ precariedad, data = df_ue)

robust(m0_lpm_all)
robust(m0_lpm_ue)

# Modelo 1
m1_lpm_all <- lm(vote_drp ~ precariedad + agea_c + gndr01, data = df_all)
m1_lpm_ue  <- lm(vote_drp ~ precariedad + agea_c + gndr01, data = df_ue)

robust(m1_lpm_all)
robust(m1_lpm_ue)

# Modelo 2: interacción edad × precariedad
m2_lpm_all <- lm(vote_drp ~ precariedad * agea_c + gndr01, data = df_all)
m2_lpm_ue  <- lm(vote_drp ~ precariedad * agea_c + gndr01, data = df_ue)

robust(m2_lpm_all)
robust(m2_lpm_ue)

# Modelo 3: control educación
m3_lpm_all <- lm(vote_drp ~ precariedad + agea_c + gndr01 + eduyrs_clean, data = df_all)
m3_lpm_ue  <- lm(vote_drp ~ precariedad + agea_c + gndr01 + eduyrs_clean, data = df_ue)

robust(m3_lpm_all)
robust(m3_lpm_ue)

# Modelo 4: interacción precariedad × género (UE)
m4_lpm_all <- lm(vote_drp ~ precariedad + agea_c + gndr01, data = df_all)
m4_lpm_ue  <- lm(vote_drp ~ precariedad * gndr01 + agea_c, data = df_ue)

robust(m4_lpm_all)
robust(m4_lpm_ue)

# Modelo 5: efectos fijos por país (ALL)
m5_lpm_all_fe <- lm(
  vote_drp ~ precariedad + agea_c + gndr01 + factor(cntry_iso2),
  data = df_all
)

robust(m5_lpm_all_fe)

# ============================================================
# 2) MODELOS LOGÍSTICOS
# ============================================================

m1_logit_all <- glm(
  vote_drp ~ precariedad + agea_c + gndr01,
  data = df_all,
  family = binomial(link = "logit")
)

m1_logit_ue <- glm(
  vote_drp ~ precariedad + agea_c + gndr01,
  data = df_ue,
  family = binomial(link = "logit")
)

m2_logit_all <- glm(
  vote_drp ~ precariedad * agea_c + gndr01,
  data = df_all,
  family = binomial(link = "logit")
)

robust(m1_logit_all)
robust(m1_logit_ue)
robust(m2_logit_all)

exists("m3_lpm_ue")
exists("pred_precar_ue")
exists("df_ue")
# ============================================================
# TABLAS · vote_drp (UE)
# Descriptivos + modelos LPM con SE robustos (HC1)
# ============================================================

# Requisitos:
# - df_ue
# - m1_lpm_ue, m2_lpm_ue, m3_lpm_ue, m4_lpm_ue
# - función robust()

library(dplyr)
library(tidyr)
library(knitr)
library(kableExtra)
library(lmtest)
library(sandwich)

# ============================================================
# TABLA 1 · Descriptivos básicos (UE)
# ============================================================

df_t1 <- df_ue %>%
  select(vote_drp, agea, gndr01, precariedad, eduyrs_clean) %>%
  filter(complete.cases(.))

tabla1_resumen <- tibble(
  N = nrow(df_t1),
  `Voto DRP (%)` = mean(df_t1$vote_drp == 1) * 100,
  `Edad media` = mean(df_t1$agea),
  `Edad (SD)` = sd(df_t1$agea),
  `Hombres (%)` = mean(df_t1$gndr01 == 0) * 100,
  `Mujeres (%)` = mean(df_t1$gndr01 == 1) * 100,
  `Educación (años) media` = mean(df_t1$eduyrs_clean),
  `Educación (años) SD` = sd(df_t1$eduyrs_clean)
)

tabla1_precariedad <- df_t1 %>%
  count(precariedad) %>%
  mutate(`Precariedad (%)` = n / sum(n) * 100) %>%
  arrange(precariedad)

tabla1_resumen %>%
  kable(
    caption = "Tabla 1. Estadísticos descriptivos de la muestra (UE)",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE)

tabla1_precariedad %>%
  kable(
    caption = "Tabla 2. Distribución de la precariedad económica subjetiva (UE)",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(bootstrap_options = c("striped"),
                full_width = FALSE)

# ============================================================
# TABLA 3 · Modelo 1 (UE)
# ============================================================

ct1 <- robust(m1_lpm_ue)

tab_m1 <- data.frame(
  term = rownames(ct1),
  estimate = ct1[, 1],
  se_robust = ct1[, 2],
  p = ct1[, 4],
  row.names = NULL
) %>%
  mutate(
    Variable = recode(
      term,
      "(Intercept)" = "Constante",
      "precariedad" = "Precariedad (hincfel, 1–4)",
      "agea_c" = "Edad (centrada)",
      "gndr01" = "Mujer (ref.=hombre)"
    ),
    `Coef.` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  ) %>%
  select(Variable, `Coef.`, `SE robusta`, `p-valor`)

tab_m1 %>%
  kable(
    caption = "Tabla 3. Modelo 1 (UE) — LPM con errores robustos (HC1)",
    align = "lccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE)

# ============================================================
# TABLA 4 · Modelo 3 (UE)
# ============================================================

ct3 <- robust(m3_lpm_ue)

tab_m3 <- data.frame(
  term = rownames(ct3),
  estimate = ct3[, 1],
  se_robust = ct3[, 2],
  p = ct3[, 4],
  row.names = NULL
) %>%
  mutate(
    Variable = recode(
      term,
      "(Intercept)" = "Constante",
      "precariedad" = "Precariedad (hincfel, 1–4)",
      "agea_c" = "Edad (centrada)",
      "gndr01" = "Mujer (ref.=hombre)",
      "eduyrs_clean" = "Educación (años)"
    ),
    `Coef.` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  ) %>%
  select(Variable, `Coef.`, `SE robusta`, `p-valor`)

tab_m3 %>%
  kable(
    caption = "Tabla 4. Modelo 3 (UE) — LPM con control educativo (HC1)",
    align = "lccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE)

# ============================================================
# TABLA 5 · Modelo 2 (UE) — interacción edad × precariedad
# ============================================================

ct2 <- robust(m2_lpm_ue)

tab_m2 <- data.frame(
  term = rownames(ct2),
  estimate = ct2[, 1],
  se_robust = ct2[, 2],
  p = ct2[, 4],
  row.names = NULL
) %>%
  mutate(
    Variable = recode(
      term,
      "(Intercept)" = "Constante",
      "precariedad" = "Precariedad (hincfel, 1–4)",
      "agea_c" = "Edad (centrada)",
      "gndr01" = "Mujer (ref.=hombre)",
      "precariedad:agea_c" = "Precariedad × edad"
    ),
    `Coef.` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  ) %>%
  select(Variable, `Coef.`, `SE robusta`, `p-valor`)

tab_m2 %>%
  kable(
    caption = "Tabla 5. Modelo 2 (UE) — Interacción edad × precariedad (HC1)",
    align = "lccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE)

# ============================================================
# TABLA 6 · Modelo 4 (UE) — interacción género × precariedad
# ============================================================

ct4 <- robust(m4_lpm_ue)

tab_m4 <- data.frame(
  term = rownames(ct4),
  estimate = ct4[, 1],
  se_robust = ct4[, 2],
  p = ct4[, 4],
  row.names = NULL
) %>%
  mutate(
    Variable = recode(
      term,
      "(Intercept)" = "Constante",
      "precariedad" = "Precariedad (hincfel, 1–4)",
      "gndr01" = "Mujer (ref.=hombre)",
      "agea_c" = "Edad (centrada)",
      "precariedad:gndr01" = "Precariedad × mujer"
    ),
    `Coef.` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
  ) %>%
  select(Variable, `Coef.`, `SE robusta`, `p-valor`)

tab_m4 %>%
  kable(
    caption = "Tabla 6. Modelo 4 (UE) — Interacción precariedad × género (HC1)",
    align = "lccc"
  ) %>%
  kable_styling(bootstrap_options = c("striped", "condensed"),
                full_width = FALSE)


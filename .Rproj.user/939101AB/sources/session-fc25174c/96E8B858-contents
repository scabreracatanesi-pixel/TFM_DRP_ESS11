# Tabla 1 (UE) · descriptivos básicos para la sección 4.1
library(dplyr)
library(tidyr)
 
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

print(tabla1_resumen)
print(tabla1_precariedad)

# Opcional: guardar para pegar en Word/Docs
write.csv(tabla1_resumen, "tabla1_resumen_ue.csv", row.names = FALSE)
write.csv(tabla1_precariedad, "tabla1_precariedad_ue.csv", row.names = FALSE)


#####################

library(knitr)
library(kableExtra)

tabla1_resumen %>%
  kable(
    caption = "Tabla 1. Estadísticos descriptivos de la muestra (UE)",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  )
####################

tabla1_precariedad %>%
  kable(
    caption = "Tabla 2. Distribución de la precariedad económica subjetiva (UE)",
    digits = 2,
    align = "c"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped"),
    full_width = FALSE
  )

###############################################

# ============================================================
# TABLA · 4.3.1 Modelo 1 (LPM, UE) — efectos principales
# Precariedad + edad (centrada) + género
# Requisitos: existen df_ue y m1_lpm_ue (o lo crea aquí)
# Salida: tabla bonita + CSV para pegar en el TFM
# ============================================================

library(dplyr)
library(lmtest)
library(sandwich)
library(knitr)
library(kableExtra)

# (1) Ajustar Modelo 1 si aún no existe
m1_lpm_ue <- lm(vote_drp ~ precariedad + agea_c + gndr01, data = df_ue)

# (2) Coeficientes con errores robustos (HC1)
ct <- lmtest::coeftest(m1_lpm_ue, vcov. = sandwich::vcovHC(m1_lpm_ue, type = "HC1"))

tab_m1 <- data.frame(
  term = rownames(ct),
  estimate = ct[, 1],
  se_robust = ct[, 2],
  t = ct[, 3],
  p = ct[, 4],
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
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, `Coef.`, `SE robusta`, `p-valor`, `Sig.`)

# (3) Imprimir tabla bonita (Viewer/Rmd)
tab_m1 %>%
  kable(
    caption = "Tabla 3. Modelo 1 (UE) — LPM (coeficientes en probabilidad) con errores robustos (HC1). VD: voto DRP (1/0).",
    align = "lcccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

################################################

# ============================================================
# TABLA 4 · Modelo 3 (LPM, UE) — control por educación
# Precariedad + edad (centrada) + género + educación
# ============================================================

library(dplyr)
library(knitr)
library(kableExtra)

# Extraer resultados robustos (HC1)
ct3 <- robust(m3_lpm_ue)

tab_m3 <- data.frame(
  term = rownames(ct3),
  estimate = ct3[, 1],
  se_robust = ct3[, 2],
  t = ct3[, 3],
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
    `Coef. (pp)` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, `Coef. (pp)`, `SE robusta`, `p-valor`, `Sig.`)

tab_m3 %>%
  kable(
    caption = "Tabla 4. Modelo 3 (UE) — LPM con educación como variable de control y errores robustos (HC1). VD: voto DRP (1/0).",
    align = "lcccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

# ============================================================
# TABLA · Modelo 2 (LPM, UE) — interacción precariedad × edad
# vote_drp ~ precariedad * agea_c + gndr01
# ============================================================

library(dplyr)
library(knitr)
library(kableExtra)

# (1) Ajustar Modelo 2 (UE)
m2_lpm_ue <- lm(vote_drp ~ precariedad * agea_c + gndr01, data = df_ue)

# (2) Extraer robust (HC1) usando tu helper robust()
ct2 <- robust(m2_lpm_ue)

tab_m2 <- data.frame(
  term = rownames(ct2),
  estimate = ct2[, 1],
  se_robust = ct2[, 2],
  t = ct2[, 3],
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
    `Coef. (pp)` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, `Coef. (pp)`, `SE robusta`, `p-valor`, `Sig.`)

# (3) Mostrar tabla bonita
tab_m2 %>%
  kable(
    caption = "Tabla 5. Modelo 2 (UE) — LPM con interacción precariedad × edad y errores robustos (HC1). VD: voto DRP (1/0).",
    align = "lcccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

#################################

# ============================================================
# TABLA · Interacción precariedad × género (UE)
# ============================================================

library(dplyr)
library(knitr)
library(kableExtra)

ct4 <- robust(m4_lpm_ue)

tab_m4 <- data.frame(
  term = rownames(ct4),
  estimate = ct4[, 1],
  se_robust = ct4[, 2],
  t = ct4[, 3],
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
    `Coef. (pp)` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, `Coef. (pp)`, `SE robusta`, `p-valor`, `Sig.`)

tab_m4 %>%
  kable(
    caption = "Tabla 6. Modelo 4 (UE) — LPM con interacción precariedad × género y errores robustos (HC1). VD: voto DRP (1/0).",
    align = "lcccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )


##############################################

# ============================================================
# TABLA DESCRIPTIVA AMPLIADA (UE) · TFM
# Variables: vote_drp, precariedad, agea(+agea_c), gndr01, eduyrs_clean, cntry_iso2
# ============================================================

library(dplyr)
library(tidyr)

# 0) Dataset descriptivo (solo casos con VD no-missing, como tu pipeline)
df_desc <- df_ue %>%
  filter(!is.na(vote_drp)) %>%
  select(vote_drp, precariedad, agea, agea_c, gndr01, eduyrs_clean, cntry_iso2)

# -----------------------
# 1) Resumen general (continuas + binarias)
# -----------------------
tabla_desc_resumen <- tibble::tibble(
  N = nrow(df_desc),
  
  `Voto DRP (%)` = mean(df_desc$vote_drp == 1, na.rm = TRUE) * 100,
  
  `Edad media` = mean(df_desc$agea, na.rm = TRUE),
  `Edad (SD)`  = sd(df_desc$agea, na.rm = TRUE),
  
  `Edad centrada media` = mean(df_desc$agea_c, na.rm = TRUE),
  `Edad centrada (SD)`  = sd(df_desc$agea_c, na.rm = TRUE),
  
  `Mujeres (%)` = mean(df_desc$gndr01 == 1, na.rm = TRUE) * 100,
  `Hombres (%)` = mean(df_desc$gndr01 == 0, na.rm = TRUE) * 100,
  
  `Educación (años) media` = mean(df_desc$eduyrs_clean, na.rm = TRUE),
  `Educación (años) SD`    = sd(df_desc$eduyrs_clean, na.rm = TRUE)
)

print(tabla_desc_resumen)

# -----------------------
# 2) Distribución precariedad (1–4)
# -----------------------
tabla_desc_precariedad <- df_desc %>%
  filter(!is.na(precariedad)) %>%
  count(precariedad, name = "n") %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(precariedad)

print(tabla_desc_precariedad)

# -----------------------
# 3) Distribución por país (peso muestral)
# -----------------------
tabla_desc_paises <- df_desc %>%
  filter(!is.na(cntry_iso2)) %>%
  count(cntry_iso2, name = "n") %>%
  mutate(pct = n / sum(n) * 100) %>%
  arrange(desc(pct))

print(tabla_desc_paises)

# -----------------------
# 4) Guardar CSVs (para pegar en el TFM / apéndice)
# -----------------------
write.csv(tabla_desc_resumen,      "tabla_desc_resumen_ue.csv", row.names = FALSE)
write.csv(tabla_desc_precariedad,  "tabla_desc_precariedad_ue.csv", row.names = FALSE)
write.csv(tabla_desc_paises,       "tabla_desc_paises_ue.csv", row.names = FALSE)

# ============================================================
# (Opcional) versión "bonita" para Rmd/Viewer
# ============================================================
library(knitr)
library(kableExtra)

tabla_desc_resumen %>%
  kable(caption = "Tabla. Estadísticos descriptivos (UE)", digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

tabla_desc_precariedad %>%
  kable(caption = "Tabla. Distribución de precariedad (UE)", digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)

tabla_desc_paises %>%
  kable(caption = "Tabla. Peso muestral por país (UE)", digits = 2, align = "c") %>%
  kable_styling(bootstrap_options = c("striped", "condensed"), full_width = FALSE)


##########################

# ============================================================
# TABLA A2 · LPM con efectos fijos por país (robustez)
# ISO2 + nombre país
# ============================================================

library(dplyr)
library(lmtest)
library(sandwich)
library(knitr)
library(kableExtra)

# Extraer coeficientes robustos HC1
ct_fe <- coeftest(m5_lpm_all_fe, vcov. = vcovHC(m5_lpm_all_fe, type = "HC1"))

# Construir tabla FE países
tabla_fe <- data.frame(
  term = rownames(ct_fe),
  estimate = ct_fe[,1],
  se_robust = ct_fe[,2],
  p = ct_fe[,4],
  row.names = NULL
) %>%
  filter(grepl("^factor\\(cntry_iso2\\)", term)) %>%
  mutate(
    cntry_iso2 = gsub("factor\\(cntry_iso2\\)", "", term),
    `Coef. (pp)` = sprintf("%.3f", estimate),
    `SE robusta` = sprintf("%.3f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(cntry_iso2, `Coef. (pp)`, `SE robusta`, `p-valor`, `Sig.`)

# Diccionario ISO2 → nombre país
cntry_labels <- tibble::tibble(
  cntry_iso2 = c("BE","BG","CH","CY","EE","ES","FI","FR","GB","GR","HR","HU","IE","IL",
                 "IS","IT","LV","ME","NL","NO","PL","PT","RS","SE","SI","SK","UA"),
  Pais = c("Bélgica","Bulgaria","Suiza","Chipre","Estonia","España",
           "Finlandia","Francia","Reino Unido","Grecia","Croacia",
           "Hungría","Irlanda","Israel","Islandia","Italia","Letonia",
           "Montenegro","Países Bajos","Noruega","Polonia","Portugal",
           "Serbia","Suecia","Eslovenia","Eslovaquia","Ucrania")
)

tabla_fe <- tabla_fe %>%
  left_join(cntry_labels, by = "cntry_iso2") %>%
  select(cntry_iso2, Pais, everything())

# Imprimir tabla bonita
tabla_fe %>%
  kable(
    caption = "Tabla A2. LPM con efectos fijos por país (robustez). País de referencia: Austria.",
    align = "lccccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

# Guardar CSV (apéndice)
write.csv(tabla_fe, "tabla_FE_paises_LPM.csv", row.names = FALSE)


#############################################

#PESO MUESTREAL PAÍSES

library(dplyr)
library(knitr)
library(kableExtra)

# Diccionario ISO2 → nombre país
cntry_labels <- tibble::tibble(
  cntry_iso2 = c("AT","BE","BG","CH","CY","EE","ES","FI","FR","GB","GR","HR","HU",
                 "IE","IL","IS","IT","LV","ME","NL","NO","PL","PT","RS","SE","SI",
                 "SK","UA"),
  Pais = c("Austria","Bélgica","Bulgaria","Suiza","Chipre","Estonia","España",
           "Finlandia","Francia","Reino Unido","Grecia","Croacia","Hungría",
           "Irlanda","Israel","Islandia","Italia","Letonia","Montenegro",
           "Países Bajos","Noruega","Polonia","Portugal","Serbia","Suecia",
           "Eslovenia","Eslovaquia","Ucrania")
)

tabla_peso_paises <- df_all %>%
  count(cntry_iso2, name = "N") %>%
  mutate(`% muestra` = round(N / sum(N) * 100, 2)) %>%
  left_join(cntry_labels, by = "cntry_iso2") %>%
  select(cntry_iso2, Pais, N, `% muestra`) %>%
  arrange(desc(N))

# Tabla bonita
tabla_peso_paises %>%
  kable(
    caption = "Tabla A1. Distribución muestral por país",
    align = "lccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  column_spec(1, width = "2.5cm") %>%   # cntry_iso2
  column_spec(2, width = "4cm") %>%     # País
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )



#################################################

# TABLA · Interacción edad × género (UE)
# ============================================================

library(dplyr)
library(knitr)
library(kableExtra)

ct6 <- robust(m6_lpm_ue_ageXgender)

tab_m6 <- data.frame(
  term = rownames(ct6),
  estimate = ct6[, 1],
  se_robust = ct6[, 2],
  t = ct6[, 3],
  p = ct6[, 4],
  row.names = NULL
) %>%
  mutate(
    Variable = recode(
      term,
      "(Intercept)" = "Constante",
      "precariedad" = "Precariedad (hincfel, 1–4)",
      "gndr01" = "Mujer (ref.=hombre)",
      "agea_c" = "Edad (centrada)",
      "eduyrs_clean" = "Años de educación",
      "agea_c:gndr01" = "Edad × mujer"
    ),
    `Coef. (pp)` = sprintf("%.4f", estimate),
    `SE robusta` = sprintf("%.4f", se_robust),
    `p-valor` = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)),
    `Sig.` = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      p < 0.10  ~ "·",
      TRUE ~ ""
    )
  ) %>%
  select(Variable, `Coef. (pp)`, `SE robusta`, `p-valor`, `Sig.`)

tab_m6 %>%
  kable(
    caption = "Tabla 7. Modelo 5 (UE) — LPM con interacción edad × género y errores robustos (HC1). VD: voto DRP (1/0).",
    align = "lcccc"
  ) %>%
  kable_styling(
    bootstrap_options = c("striped", "condensed"),
    full_width = FALSE
  ) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

##############################

# Tabla 1. Descriptivos (versión Sergi) — con SD también en variables binarias

library(dplyr)
library(knitr)
library(kableExtra)

desc_table <- tibble(
  Variable = c(
    "Vote DRP",
    "Edad",
    "Mujer",
    "Precariedad (hincfel)",
    "Educación (años)"
  ),
  `Mean / Prop.` = c(
    mean(df_all$vote_drp, na.rm = TRUE),
    mean(df_all$agea, na.rm = TRUE),
    mean(df_all$gndr01, na.rm = TRUE),
    mean(df_all$hincfel, na.rm = TRUE),
    mean(df_all$eduyrs_clean, na.rm = TRUE)
  ),
  `Std. Dev.` = c(
    sd(df_all$vote_drp, na.rm = TRUE),
    sd(df_all$agea, na.rm = TRUE),
    sd(df_all$gndr01, na.rm = TRUE),
    sd(df_all$hincfel, na.rm = TRUE),
    sd(df_all$eduyrs_clean, na.rm = TRUE)
  ),
  N = sum(!is.na(df_all$vote_drp))
)

kable(
  desc_table,
  digits = 3,
  caption = "Tabla 1. Estadísticos descriptivos de la muestra (UE)"
) %>%
  kable_styling(full_width = FALSE) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )


############################

#tabla2. distribución de la precariedad económica subjetiva.

library(dplyr)
library(knitr)
library(kableExtra)

tabla_precariedad <- df_all %>%
  filter(hincfel %in% 1:4) %>%
  mutate(
    `Nivel de precariedad` = case_when(
      hincfel == 1 ~ "1. Vive cómodamente con los ingresos actuales",
      hincfel == 2 ~ "2. Se apaña con los ingresos actuales",
      hincfel == 3 ~ "3. Tiene dificultades con los ingresos actuales",
      hincfel == 4 ~ "4. Tiene muchas dificultades con los ingresos actuales"
    )
  ) %>%
  count(`Nivel de precariedad`) %>%
  mutate(
    `Precariedad (%)` = round(100 * n / sum(n), 2)
  )

kable(
  tabla_precariedad,
  caption = "Tabla 2. Distribución de la precariedad económica subjetiva (UE)",
  col.names = c("Nivel de precariedad", "n", "Precariedad (%)"),
  align = "lrr"
) %>%
  kable_styling(full_width = FALSE) %>%
  footnote(
    general = "Fuente: Elaboración propia. Datos: European Social Survey (ESS11).",
    general_title = ""
  )

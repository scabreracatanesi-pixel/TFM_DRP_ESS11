library(ggplot2)
library(ggeffects)
library(dplyr)


# 04_plots_drp.R -------------------------------------------------------------

p1 <- ggplot(pred_precar_ue, aes(x = x, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Gráfica 1. UE · Probabilidad predicha de voto DRP según precariedad",
    x = "Precariedad subjetiva",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones del Modelo 3 (LPM): controles fijados en edad media (agea_c)",
      "y educación media (eduyrs_clean). Género promediado según su distribución",
      "observada en la muestra.\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(hjust = 0, size = 9),
    plot.title = element_text(size = 12)
  )

print(p1)

ggsave(
  "fig_01_precariedad_UE_m3.png",
  p1,
  width = 7,
  height = 5,
  dpi = 300
)




# --- GRÁFICA 2: efecto de la edad (UE) con controles promedio ------------------

library(ggplot2)
library(ggeffects)

# 1) Valores "típicos" para fijar controles
prec_fix <- mean(df_ue$precariedad, na.rm = TRUE)
edu_fix  <- mean(df_ue$eduyrs_clean, na.rm = TRUE)

# Para mostrar edad en años reales (aunque el modelo use agea_c)
mean_age <- mean(df_ue$agea, na.rm = TRUE)

# 2) Predicciones marginales para edad
pred_age_ue <- ggeffects::ggpredict(
  m3_lpm_ue,
  terms = "agea_c [all]",
  condition = c(
    precariedad  = prec_fix,
    eduyrs_clean = edu_fix
    # género NO fijado → promedio
  )
)

# 3) Plot
p2 <- ggplot(pred_age_ue, aes(x = x + mean_age, y = predicted)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(
    title = "Gráfica 2. UE · Probabilidad predicha de voto DRP según edad",
    x = "Edad (años)",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones del Modelo 3 (LPM): controles fijados en precariedad media (precariedad)",
      "y educación media (eduyrs_clean).\n",
      "Género promediado según su distribución observada en la muestra.\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin  = margin(t = 10, r = 10, b = 25, l = 10)
  )

print(p2)

# 4) Guardar
ggsave("fig_02_edad_UE_m3.png", p2, width = 8.5, height = 6, dpi = 300)


p3 <- ggplot(
  pred_compare,
  aes(x = x, y = predicted, linetype = modelo, group = modelo)
) +
  geom_line(linewidth = 1, color = "black") +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey70",
    alpha = 0.2
  ) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Gráfica 3. UE · Precariedad y voto DRP: comparación Modelo 1 vs Modelo 3",
    x = "Precariedad subjetiva",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones LPM. Ambos modelos fijan la edad en su valor medio (agea_c).",
      "El Modelo 3 incorpora educación como variable de control (eduyrs_clean).\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "bottom"
  )

print(p3)

ggsave(
  "fig_03_compare_m1_m3_precariedad_UE.png",
  p3,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

#########################
pred_precar_ue
as.data.frame(pred_precar_ue)

########################################

# ============================================================
# GRÁFICA 4 (UE) · Modelo 2 (LPM)
# Interacción precariedad × edad
# Estilo homogéneo (B/N), sin "mejor/peor", con fuente en caption
# ============================================================

library(dplyr)
library(ggplot2)

# Edades representativas (años reales)
ages_plot <- c(30, 50, 70)

# Media de edad para centrar (coherente con agea_c)
age_mean <- mean(df_ue$agea, na.rm = TRUE)

# Grid de predicción
pred_m2 <- expand.grid(
  precariedad = 1:4,
  agea_c = ages_plot - age_mean,
  gndr01 = 0
)

# Predicciones + IC 95%
pred <- predict(
  m2_lpm_ue,
  newdata = pred_m2,
  se.fit = TRUE
)

pred_m2 <- pred_m2 %>%
  mutate(
    fit = pred$fit,
    lwr = fit - 1.96 * pred$se.fit,
    upr = fit + 1.96 * pred$se.fit,
    Edad = factor(
      agea_c + age_mean,
      levels = ages_plot,
      labels = ages_plot
    )
  )

# Plot (B/N)
p4 <- ggplot(pred_m2, aes(x = precariedad, y = fit, group = Edad)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(aes(linetype = Edad), color = "black", linewidth = 1) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Gráfica 4. UE · Precariedad subjetiva y voto DRP según edad (Modelo 2)",
    x = "Precariedad subjetiva",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones del Modelo 2 (LPM) con interacción precariedad × edad.",
      "Género fijado en hombre y edad representada a los 30, 50 y 70 años.\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "bottom"
  )

print(p4)

ggsave(
  "fig_04_precariedad_edad_m2_UE.png",
  p4,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)


# ============================================================
# GRÁFICA 5 (UE) · Modelo 4 (LPM)
# Interacción precariedad × género
# Estilo homogéneo (B/N) + fuente en caption
# ============================================================

library(dplyr)
library(ggplot2)

# Grid de predicción
pred_m4 <- expand.grid(
  precariedad = 1:4,
  gndr01 = c(0, 1),   # 0 = hombre, 1 = mujer
  agea_c = 0          # edad centrada (media)
)

# Predicciones con IC 95%
pred <- predict(
  m4_lpm_ue,
  newdata = pred_m4,
  se.fit = TRUE
)

pred_m4 <- pred_m4 %>%
  mutate(
    fit = pred$fit,
    lwr = fit - 1.96 * pred$se.fit,
    upr = fit + 1.96 * pred$se.fit,
    Género = factor(
      gndr01,
      levels = c(0, 1),
      labels = c("Hombres", "Mujeres")
    )
  )

p5 <- ggplot(pred_m4, aes(x = precariedad, y = fit, group = Género)) +
  geom_ribbon(
    aes(ymin = lwr, ymax = upr),
    fill = "grey70",
    alpha = 0.25
  ) +
  geom_line(aes(linetype = Género), color = "black", linewidth = 1) +
  scale_x_continuous(breaks = 1:4) +
  labs(
    title = "Gráfica 5. UE · Precariedad económica subjetiva y voto DRP según género (Modelo 4)",
    x = "Precariedad subjetiva",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones del Modelo 4 (LPM) con interacción precariedad × género.",
      "Edad fijada en su valor medio (agea_c = 0).\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(size = 12),
    plot.caption = element_text(hjust = 0, size = 9),
    legend.position = "bottom"
  )

print(p5)

ggsave(
  "fig_05_precariedad_genero_m4_UE.png",
  p5,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)


#########################

# ============================================================
# GRÁFICA 6 (UE) · Modelo 5 (LPM)
# Interacción edad × género
# Estilo homogéneo (B/N), misma huella que el resto, con fuente
# ============================================================

library(ggplot2)
library(ggeffects)

# Controles fijos
prec_fix <- mean(df_ue$precariedad, na.rm = TRUE)
edu_fix  <- mean(df_ue$eduyrs_clean, na.rm = TRUE)
mean_age <- mean(df_ue$agea, na.rm = TRUE)

# Predicciones: edad × género
pred_age_gender <- ggpredict(
  m6_lpm_ue_ageXgender,
  terms = c("agea_c [all]", "gndr01"),
  condition = c(
    precariedad  = prec_fix,
    eduyrs_clean = edu_fix
  )
)

# Etiquetas de género
pred_age_gender$group <- factor(
  pred_age_gender$group,
  levels = c("0", "1"),
  labels = c("Hombres", "Mujeres")
)

# Plot (B/N: linetype + ribbon gris)
p6 <- ggplot(
  pred_age_gender,
  aes(x = x + mean_age, y = predicted, group = group)
) +
  geom_ribbon(
    aes(ymin = conf.low, ymax = conf.high),
    fill = "grey70",
    alpha = 0.20
  ) +
  geom_line(aes(linetype = group), color = "black", linewidth = 1) +
  labs(
    title = "Gráfica 6. UE · Probabilidad predicha de voto DRP según edad y género (Modelo 5)",
    x = "Edad (años)",
    y = "Probabilidad predicha de votar DRP",
    caption = paste(
      "Predicciones del Modelo 5 (LPM) con interacción edad × género.",
      "Controles fijados en precariedad media y educación media.\n",
      "Fuente: Elaboración propia. Datos: European Social Survey (ESS11)."
    )
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(size = 12),
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin  = margin(t = 10, r = 10, b = 25, l = 10)
  )

print(p6)

ggsave(
  "fig_06_edad_genero_m6_UE.png",
  p6,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

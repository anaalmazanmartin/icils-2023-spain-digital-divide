# ICILS 2023 España: brecha socioeconómica en competencia digital (CIL)
# y en autoeficacia TIC
#
# P1: ¿Qué tan grande es la brecha socioeconómica en CIL (media y % bajo el Nivel 2)?
# P2: ¿Las brechas en acceso, experiencia y autoeficacia son tan grandes como en CIL?
# P3: ¿Qué tanto se corresponde la autoeficacia con la CIL medida?
#
# Salidas: table1-3.csv, tableA1.csv, figure1-3.png (las tablas y figuras del paper)
#
# Paquetes (instalar una vez):
# install.packages(c("haven", "survey", "mitools", "dplyr", "ggplot2"))

library(haven)    # lee el archivo de SPSS
library(survey)   # pesos y errores estándar jackknife
library(mitools)  # combina los cinco valores plausibles
library(dplyr)    # preparación de datos
library(ggplot2)  # figuras


# 1. Cargar los datos ------------------------------------------------------

# Archivo de alumnos de España (BSG = alumnos, ESP = España, I3 = ICILS 2023).
# Debe estar en la carpeta de trabajo.
icils <- read_sav("BSGESPI3.sav", user_na = FALSE)  # los códigos de dato faltante pasan a NA
names(icils) <- tolower(names(icils))

# Los 10 ítems de autoeficacia general (pregunta 24); solo para el alfa de Cronbach
items <- paste0("is3g24", c("a", "b", "c", "e", "f", "g", "h", "i", "j", "m"))

# Nos quedamos con las columnas que usamos (el archivo trae 610)
icils <- icils %>%
  select(pv1cil:pv5cil,            # los 5 valores plausibles de CIL
         nisb,                     # nivel socioeconómico
         s_geneff,                 # autoeficacia TIC general
         s_intnet,                 # internet en casa (0/1)
         s_excomp,                 # años usando computador (categorías 0 a 4)
         all_of(items),            # ítems de autoeficacia
         totwgts,                  # peso final del alumno
         starts_with("srwgt")) %>% # los 75 pesos replicados
  zap_labels() %>%                 # quita las etiquetas de SPSS y deja solo números
  as.data.frame()


# 2. Preparar las variables ------------------------------------------------

# NISB usa -98 para "no aplicado"; quitamos esos casos y los NA
icils <- icils %>%
  filter(!is.na(nisb), nisb > -10)

nrow(icils)   # 11.598 alumnos

icils <- icils %>%
  mutate(
    # Bajo el Nivel 2 (< 492 puntos), uno por valor plausible.
    # TRUE/FALSE por 100 queda en 0/100: su media es un porcentaje.
    below1 = 100 * (pv1cil < 492),
    below2 = 100 * (pv2cil < 492),
    below3 = 100 * (pv3cil < 492),
    below4 = 100 * (pv4cil < 492),
    below5 = 100 * (pv5cil < 492),
    # Internet en casa y 5+ años usando computador (códigos 3 y 4), en 0/100
    internet = 100 * s_intnet,
    exp5     = 100 * (s_excomp >= 3),
    # Muestra de P2 y P3: alumnos con dato en las tres variables (CIL nunca falta)
    in_rq2   = !is.na(s_geneff) & !is.na(internet) & !is.na(exp5)
  )

# Puntaje z ponderado. Media y DE salen solo de las filas con keep = TRUE.
z_w <- function(x, w, keep) {
  m <- weighted.mean(x[keep], w[keep])
  s <- sqrt(weighted.mean((x[keep] - m)^2, w[keep]))
  (x - m) / s
}

# CIL y autoeficacia en unidades de DE dentro de la muestra de P2 y P3,
# para comparar dos variables que usan escalas distintas
icils <- icils %>%
  mutate(
    z_s_geneff = z_w(s_geneff, totwgts, in_rq2),
    z_pv1cil   = z_w(pv1cil,   totwgts, in_rq2),
    z_pv2cil   = z_w(pv2cil,   totwgts, in_rq2),
    z_pv3cil   = z_w(pv3cil,   totwgts, in_rq2),
    z_pv4cil   = z_w(pv4cil,   totwgts, in_rq2),
    z_pv5cil   = z_w(pv5cil,   totwgts, in_rq2)
  )


# 3. Diseño de la encuesta -------------------------------------------------

# El diseño junta los datos con el peso del alumno y los 75 pesos replicados.
# Desde aquí todos los cálculos usan "des". type, scale, rscales y mse
# reproducen el cálculo de errores estándar de la IEA para ICILS.
des <- svrepdesign(data = icils, weights = ~totwgts, repweights = "srwgt[0-9]+",
                   type = "other", scale = 1, rscales = 1, mse = TRUE,
                   combined.weights = TRUE)

# Comprobación: NISB ya viene estandarizado en España (media 0, DE 1)
svymean(~nisb, des)              # -4e-16 es 0 en notación científica
sqrt(coef(svyvar(~nisb, des)))   # 1

# Cuartiles ponderados de NISB (Q1 = más bajo). update() agrega la variable al diseño.
cuts <- coef(svyquantile(~nisb, des, quantiles = c(.25, .50, .75)))
des  <- update(des, nisb_q = cut(nisb, c(-Inf, cuts, Inf),
                                 labels = c("Q1", "Q2", "Q3", "Q4")))

# Valores plausibles: "cil" representa a las 5 columnas. withPV() corre cada
# análisis 5 veces y MIcombine() combina los resultados y sus errores estándar.
cil_pv   <- cil   ~ pv1cil + pv2cil + pv3cil + pv4cil + pv5cil
below_pv <- below ~ below1 + below2 + below3 + below4 + below5
zcil_pv  <- zcil  ~ z_pv1cil + z_pv2cil + z_pv3cil + z_pv4cil + z_pv5cil


# 4. Pregunta 1 ------------------------------------------------------------

# Modelo: CIL según NISB. La pendiente son los puntos de CIL por 1 DE de NISB.
# quote() guarda el análisis sin correrlo, para que withPV() lo corra 5 veces.
m1 <- MIcombine(withPV(cil_pv, des, action = quote(svyglm(cil ~ nisb, design = des))))
summary(m1)

# Evaluación del modelo con el primer valor plausible:
# R² (proporción de diferencias en CIL que explica NISB) y residuos
# (la línea roja debe quedar plana; una curva indicaría relación no lineal)
check <- lm(pv1cil ~ nisb, data = icils, weights = totwgts)
summary(check)$r.squared
plot(check, which = 1)

# CIL media y % bajo el Nivel 2: total de España y por cuartil
all_cil   <- MIcombine(withPV(cil_pv,   des, action = quote(svymean(~cil,   des))))
all_below <- MIcombine(withPV(below_pv, des, action = quote(svymean(~below, des))))
mean_cil  <- MIcombine(withPV(cil_pv,   des,
                              action = quote(svyby(~cil, ~nisb_q, des, svymean))))
pct_below <- MIcombine(withPV(below_pv, des,
                              action = quote(svyby(~below, ~nisb_q, des, svymean))))

# Brecha Q4 - Q1 con su error estándar: en una regresión con los cuartiles,
# Q1 es la referencia y el coeficiente de Q4 (nisb_qQ4) es la brecha
gap_cil   <- MIcombine(withPV(cil_pv, des,
                              action = quote(svyglm(cil ~ nisb_q, design = des))))
gap_below <- MIcombine(withPV(below_pv, des,
                              action = quote(svyglm(below ~ nisb_q, design = des))))

# Table 1. CIL achievement by socioeconomic quartile
table1 <- data.frame(
  group    = c("Q1", "Q2", "Q3", "Q4", "All", "Gap Q4-Q1"),
  cil      = c(coef(mean_cil),  coef(all_cil),   coef(gap_cil)["nisb_qQ4"]),
  cil_se   = c(SE(mean_cil),    SE(all_cil),     SE(gap_cil)["nisb_qQ4"]),
  below    = c(coef(pct_below), coef(all_below), coef(gap_below)["nisb_qQ4"]),
  below_se = c(SE(pct_below),   SE(all_below),   SE(gap_below)["nisb_qQ4"]),
  row.names = NULL
)
table1[-1] <- round(table1[-1], 1)
table1
write.csv(table1, "table1.csv", row.names = FALSE)

# Figura 1: % bajo el Nivel 2 por cuartil, con IC al 95 % (± 1,96 EE)
fig1 <- table1 %>%
  filter(group %in% c("Q1", "Q2", "Q3", "Q4"))

ggplot(fig1, aes(x = group, y = below, group = 1)) +
  geom_line(colour = "grey25") +
  geom_errorbar(aes(ymin = below - 1.96 * below_se, ymax = below + 1.96 * below_se),
                width = 0.15, colour = "grey25") +
  geom_point(shape = 21, size = 3, fill = "grey60", colour = "grey25") +
  geom_text(aes(y = below + 1.96 * below_se + 3, label = paste0(round(below), "%")),
            size = 4) +
  labs(x = "Socioeconomic background (quartiles)",
       y = "Students below Level 2 (%)") +
  scale_x_discrete(expand = expansion(add = 0.1)) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "grey80", linewidth = 0.1),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))
ggsave("figure1.png", width = 6, height = 4, dpi = 300, bg = "white")


# 5. Pregunta 2 ------------------------------------------------------------

# Diseño solo con los alumnos de P2 y P3. Se filtra el diseño con subset()
# (filter() no funciona con un diseño de survey); así se mantienen los
# cuartiles de P1 y los errores estándar quedan bien calculados.
rq2 <- subset(des, in_rq2)
nrow(rq2)   # 10.671 alumnos

# Media de cada capa por cuartil. svyby() recibe: 1) las variables, 2) la
# variable que forma los grupos, 3) el diseño y 4) lo que se calcula.
# Internet, experiencia y autoeficacia tienen un solo valor por alumno;
# CIL tiene cinco y va aparte, con withPV().
layers    <- svyby(~internet + exp5 + s_geneff, ~nisb_q, rq2, svymean)
layers_se <- SE(layers)   # columnas en el mismo orden: internet, exp5, s_geneff
cil2 <- MIcombine(withPV(cil_pv, rq2,
                         action = quote(svyby(~cil, ~nisb_q, rq2, svymean))))

# Brechas Q4 - Q1, igual que en P1
gap_net  <- svyglm(internet ~ nisb_q, design = rq2)
gap_exp  <- svyglm(exp5     ~ nisb_q, design = rq2)
gap_eff  <- svyglm(s_geneff ~ nisb_q, design = rq2)
gap_cil2 <- MIcombine(withPV(cil_pv, rq2,
                             action = quote(svyglm(cil ~ nisb_q, design = rq2))))

# Table 2. ICT access, experience, self-efficacy and CIL by socioeconomic quartile.
# Cada capa va en su unidad (%, puntos), así que aquí las brechas no se
# comparan entre sí; eso se hace en la Tabla 3, en unidades de DE.
table2 <- data.frame(
  group       = c("Q1", "Q2", "Q3", "Q4", "Gap Q4-Q1"),
  internet    = c(layers$internet, coef(gap_net)["nisb_qQ4"]),
  internet_se = c(layers_se[, 1],  SE(gap_net)["nisb_qQ4"]),
  exp5        = c(layers$exp5,     coef(gap_exp)["nisb_qQ4"]),
  exp5_se     = c(layers_se[, 2],  SE(gap_exp)["nisb_qQ4"]),
  geneff      = c(layers$s_geneff, coef(gap_eff)["nisb_qQ4"]),
  geneff_se   = c(layers_se[, 3],  SE(gap_eff)["nisb_qQ4"]),
  cil         = c(coef(cil2),      coef(gap_cil2)["nisb_qQ4"]),
  cil_se      = c(SE(cil2),        SE(gap_cil2)["nisb_qQ4"]),
  row.names = NULL
)
table2[-1] <- round(table2[-1], 1)
table2
write.csv(table2, "table2.csv", row.names = FALSE)

# Modelo P2: correlación de NISB con CIL y con autoeficacia.
# Las dos están en unidades de DE y NISB tiene DE 1: la pendiente es la correlación.
r_cil <- MIcombine(withPV(zcil_pv, rq2,
                          action = quote(svyglm(zcil ~ nisb, design = rq2))))
r_eff <- svyglm(z_s_geneff ~ nisb, design = rq2)

# Figura 2: CIL y autoeficacia por cuartil, en unidades de DE (0 = media de España).
# La inclinación de cada línea muestra el tamaño de la brecha.
z_cil <- MIcombine(withPV(zcil_pv, rq2,
                          action = quote(svyby(~zcil, ~nisb_q, rq2, svymean))))
z_eff <- svyby(~z_s_geneff, ~nisb_q, rq2, svymean)

fig2 <- data.frame(
  group   = rep(c("Q1", "Q2", "Q3", "Q4"), 2),
  outcome = factor(rep(c("Measured CIL", "ICT self-efficacy"), each = 4),
                   levels = c("Measured CIL", "ICT self-efficacy")),
  mean    = c(coef(z_cil), coef(z_eff)),
  se      = c(SE(z_cil),   SE(z_eff))
)

dodge <- position_dodge(width = 0.2)   # separa los puntos para que no se tapen
tonos <- c("Measured CIL" = "grey25", "ICT self-efficacy" = "grey60")

ggplot(fig2, aes(x = group, y = mean, group = outcome,
                 colour = outcome, fill = outcome)) +
  geom_hline(yintercept = 0, colour = "grey80") +
  geom_line(position = dodge) +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.15, position = dodge) +
  geom_point(shape = 21, size = 3, colour = "grey25", position = dodge) +
  scale_colour_manual(values = tonos) +
  scale_fill_manual(values = tonos) +
  labs(x = "Socioeconomic background (quartiles)", y = "Mean (SD units)",
       colour = NULL, fill = NULL) +
  scale_x_discrete(expand = expansion(add = 0.1)) +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "grey80", linewidth = 0.1),
        axis.title  = element_text(size = 14),
        axis.text   = element_text(size = 12),
        legend.text = element_text(size = 12),
        legend.position = "bottom")
ggsave("figure2.png", width = 6, height = 4, dpi = 300, bg = "white")


# 6. Pregunta 3 ------------------------------------------------------------

# Modelo P3: correlación entre autoeficacia y CIL medida.
# Las dos están en unidades de DE, así que la pendiente también es la correlación.
# CIL va como respuesta: si un valor plausible va como predictor, cada una de las
# 5 regresiones nombra distinto su coeficiente y MIcombine() no puede juntarlas.
r_link <- MIcombine(withPV(zcil_pv, rq2,
                           action = quote(svyglm(zcil ~ z_s_geneff, design = rq2))))

# Table 3. Correlations (P2: filas 1 y 2; P3: fila 3).
# Si los intervalos de las filas 1 y 2 no se tocan, las brechas son distintas.
table3 <- data.frame(
  pair = c("NISB - CIL", "NISB - ICT self-efficacy", "ICT self-efficacy - CIL"),
  r    = c(coef(r_cil)["nisb"], coef(r_eff)["nisb"], coef(r_link)["z_s_geneff"]),
  se   = c(SE(r_cil)["nisb"],   SE(r_eff)["nisb"],   SE(r_link)["z_s_geneff"])
) %>%
  mutate(ci_low  = r - 1.96 * se,
         ci_high = r + 1.96 * se)
table3[-1] <- round(table3[-1], 3)
table3
write.csv(table3, "table3.csv", row.names = FALSE)

# Figura 3: CIL media por decil de autoeficacia.
# La correlación resume la relación en un número; la figura muestra su forma.
# Deciles ponderados de autoeficacia, calculados en la muestra de P2 y P3.
cuts_eff <- coef(svyquantile(~s_geneff, rq2, quantiles = seq(.1, .9, .1)))
rq2 <- update(rq2, eff_dec = cut(s_geneff, c(-Inf, cuts_eff, Inf), labels = FALSE))
dec_cil <- MIcombine(withPV(cil_pv, rq2,
                            action = quote(svyby(~cil, ~eff_dec, rq2, svymean))))

fig3 <- data.frame(decile = seq_along(coef(dec_cil)),
                   mean   = coef(dec_cil),
                   se     = SE(dec_cil))
round(fig3, 1)

# % de alumnos en el tope de la escala ("muy bien" en todas las tareas)
svymean(~I(s_geneff == max(s_geneff)), rq2)

ggplot(fig3, aes(x = decile, y = mean)) +
  geom_hline(yintercept = 492, linetype = "dotted", colour = "grey50") +   # Nivel 2
  geom_line(colour = "grey25") +
  geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se),
                width = 0.2, colour = "grey25") +
  geom_point(shape = 21, size = 3, fill = "grey60", colour = "grey25") +
  annotate("text", x = 1.3, y = 498, label = "Level 2", size = 3.5,
           colour = "grey40", hjust = 0) +
  scale_x_continuous(breaks = 1:10) +
  coord_cartesian(ylim = c(400, 600)) +
  labs(x = "ICT self-efficacy (deciles, 1 = lowest)", y = "Mean CIL score") +
  theme_classic() +
  theme(panel.grid.major = element_line(colour = "grey80", linewidth = 0.1),
        axis.title = element_text(size = 14),
        axis.text  = element_text(size = 12))
ggsave("figure3.png", width = 6, height = 4, dpi = 300, bg = "white")



# 7. Comprobaciones --------------------------------------------------------

# a) Error de medición. Las escalas de autoinforme tienen más error que una
#    prueba, y eso baja las correlaciones. Alfa de Cronbach de los 10 ítems
#    y correlación NISB-autoeficacia corregida (dividida por la raíz del alfa).
it <- icils %>%
  filter(in_rq2) %>%
  select(all_of(items)) %>%
  na.omit()
k <- ncol(it)
alpha <- k / (k - 1) * (1 - sum(apply(it, 2, var)) / var(rowSums(it)))
alpha                        # alrededor de 0,84
table3$r[2] / sqrt(alpha)    # alrededor de 0,10

# b) Alumnos que quedan fuera de P2 y P3 (FALSE) frente a los que entran (TRUE)
out_nisb <- svyby(~nisb, ~in_rq2, des, svymean)
out_cil  <- MIcombine(withPV(cil_pv, des,
                             action = quote(svyby(~cil, ~in_rq2, des, svymean))))
tableA1 <- data.frame(
  group   = c("Excluded", "Included"),
  nisb    = coef(out_nisb),
  nisb_se = SE(out_nisb),
  cil     = coef(out_cil),
  cil_se  = SE(out_cil),
  row.names = NULL
)
tableA1[-1] <- round(tableA1[-1], 2)
tableA1
write.csv(tableA1, "tableA1.csv", row.names = FALSE)

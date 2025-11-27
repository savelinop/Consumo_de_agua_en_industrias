# Establecer carpeta de trabajo donde está el CSV
setwd("C:/Users/aalci/Downloads") 

# Cargar el archivo CSV
datos <- read.csv("consumo_agua_industrial.csv", sep = ",", header = TRUE)

# Verificar que se cargó correctamente
head(datos)      # muestra primeras filas
str(datos)       # estructura de las variables
summary(datos)   # estadísticos básicos


# =========================================================
# DESCRIPCION DEL CONJUNTO DE DATOS
# =========================================================

# ---- Paquetes ----
paquetes <- c("dplyr","ggplot2","forcats","scales","car","reshape2","readr",
              "gridExtra","tidyverse","boot","QuantPsyc","readxl","stringr",
              "tidyr","broom")
instalar <- setdiff(paquetes, rownames(installed.packages()))
if(length(instalar)) install.packages(instalar)
lapply(paquetes, library, character.only = TRUE)

# Tema base
theme_set(theme_minimal(base_size = 15))

# Función helper para etiquetas %
lab_pct <- function(x) scales::percent(x, accuracy = 0.01)

# --- VARIABLES CATEGÓRICAS ---
# --- Gráfico 1. Distribución de industrias según ciudad ---
datos %>%
  count(ciudad) %>%
  mutate(prop = n/sum(n)*100) %>%
  ggplot(aes(x = ciudad, y = n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes(label = paste0(n, " (", round(prop,2), "%)")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Distribución de industrias según ciudad",
       x = "Ciudad", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),       
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))

# --- Gráfico 2. Distribución de industrias según actividad industrial ---
datos %>%
  count(actividad_industrial) %>%
  mutate(prop = n/sum(n)*100) %>%
  ggplot(aes(x = actividad_industrial, y = n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes(label = paste0(n, " (", round(prop,2), "%)")),
            vjust = -0.25, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  labs(title = "Distribución de industrias según actividad industrial",
       x = "Actividad industrial", y = "Frecuencia") +
  theme_minimal() +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),     
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9)) +
  theme(plot.title = element_text(hjust = 0.5))

# --- Gráfico 3. Uso de agua en procesos ---
datos %>%
  count(usa_agua_proceso) %>%
  mutate(prop = n/sum(n)*100) %>%
  ggplot(aes(x = usa_agua_proceso, y = n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes(label = paste0(n, " (", round(prop,2), "%)")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Distribución de industrias según uso de agua en procesos",
       x = "Uso de agua en procesos", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),       
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))

# --- Gráfico 4. Tratamiento de aguas ---
datos %>%
  count(tratamiento_aguas) %>%
  mutate(prop = n/sum(n)*100) %>%
  ggplot(aes(x = tratamiento_aguas, y = n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes(label = paste0(n, " (", round(prop,2), "%)")),
            vjust = -0.5, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.3))) +
  labs(title = "Distribución de industrias según tratamiento de aguas",
       x = "Tratamiento de aguas", y = "Frecuencia") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),        
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))

# --- Gráfico 5. Fuente de agua ---
datos %>%
  count(fuente_agua) %>%
  mutate(prop = n/sum(n)*100) %>%
  ggplot(aes(x = fuente_agua, y = n)) +
  geom_col(fill = 'steelblue') +
  geom_text(aes(label = paste0(n, " (", round(prop,2), "%)")),
            vjust = -0.25, size = 3) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(title = "Distribución de industrias según fuente de agua",
       x = "Fuente de agua", y = "Frecuencia") +
  theme_minimal() +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),        
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)) +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.9)) +
  theme(plot.title = element_text(hjust = 0.5))


#VARIABLES NUMÉRICAS
#  --- HIS: Gráfico 7. Volumen consumido ---
g6a <- ggplot(datos, aes(x = volumen_consumido_m3)) +
  geom_histogram(binwidth = 50,
                 fill = "steelblue",  
                 color = "black") + 
  scale_x_continuous(limits = c(0, 3000), 
                     breaks = seq(0, 3000, 400)) + 
  labs(title = "Distribución del volumen de agua consumido (m³)",
       x = "volumen_consumido_m3", y = "Frecuencia") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),       
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))
g6a

#  --- HIS: Gráfico 8. Costo consumo mensual ---
g6b <- ggplot(datos, aes(x = costo_consumo_mensual)) +
  geom_histogram(binwidth = 50, fill = 'steelblue', color = "black") +
  scale_x_continuous(limits = c(0, 3000), 
                     breaks = seq(0, 3000, 320)) + 
  labs(title = "Distribución del costo de consumo mensual",
       x = "Costo (USD)", y = "Frecuencia") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),        
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))
g6b

#  --- HIS: Gráfico 6. Costo mantenimiento ---
g6c <- ggplot(dplyr::filter(datos, !is.na(costo_mantenimiento_tratamiento),
                            costo_mantenimiento_tratamiento >= -20),
              aes(x = costo_mantenimiento_tratamiento)) +
  geom_histogram(binwidth = 100, fill = 'steelblue'  ,  color = "black") +
  scale_x_continuous(limits = c(-200, 7000), 
                     breaks = seq(-200, 7000, 1000)) + 
  labs(title = "Distribución del costo de mantenimiento de tratamiento",
       x = "Costo de mantenimiento (USD)", y = "Frecuencia") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),     
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))
  
g6c

#  --- HIS: Gráfico 9. Número de empleados ---
g6d <- ggplot(datos, aes(x = numero_empleados)) +
  geom_histogram(binwidth = 10, fill = 'steelblue', color = "black") +
  scale_x_continuous(limits = c(50, 400), 
                     breaks = seq(50, 400, 50)) + 
  labs(title = "Distribución del número de empleados",
       x = "Empleados", y = "Frecuencia") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),        
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))

g6d

#  --- BP: Gráfico 10. Costo mantenimiento ---
g_box3 <- ggplot(datos, aes(x = "", y = costo_mantenimiento_tratamiento)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.3) +
  labs(title = "Boxplot del costo de mantenimiento de tratamiento",
       y = "Costo de mantenimiento (USD)", x = "") +
  scale_y_continuous(breaks = seq(0, 7000, 1000)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5),  
    panel.grid.minor = element_blank(),      
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)
  )

g_box3

#  --- BP: Gráfico 11. Volumen consumido por m3 ---
g_box1 <- ggplot(datos, aes(x = "", y = volumen_consumido_m3)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.3) +
  labs(title = "Boxplot del volumen de agua consumido",
       y = "Volumen consumido (m³)", x = "") +
  scale_y_continuous(breaks = seq(0, 3500, 500)) +
  theme_bw(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),       
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)
  )

g_box1


#  --- BP: Gráfico 12. Costo por consumo mensual ---
g_box2 <- ggplot(datos, aes(x = "", y = costo_consumo_mensual)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.3) +
  labs(title = "Boxplot del costo de consumo mensual",
       y = "Costo de consumo (USD)", x = '') +
  scale_y_continuous(breaks = seq(0, 3000, 500)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),       
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)
  )
g_box2


#  --- BP: Gráfico 13. Número de empleados ---
g_box4 <- ggplot(datos, aes(x = "", y = numero_empleados)) +
  geom_boxplot(fill = "steelblue", color = "black", width = 0.3) +
  labs(title = "Boxplot del número de empleados",
       y = "Número de empleados", x = '') +
  scale_y_continuous(breaks = seq(50, 400, 50)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),       
    panel.border = element_rect(colour = "black", fill = NA, linewidth = .5)
  )
g_box4


# =========================================================
# ANÁLISIS PARA CADA OBJETIVO
# =========================================================

#  --- Objetivo 1: Prueba de hipótesis para comparación de medias ---

df <- subset(
  datos[, c("volumen_consumido_m3", "usa_agua_proceso")],
  !is.na(volumen_consumido_m3) & !is.na(usa_agua_proceso)
)

## Asegurar factor con niveles "no" y "sí"
df$usa_agua_proceso <- factor(df$usa_agua_proceso, levels = c("no","sí"))

x_no <- df$volumen_consumido_m3[df$usa_agua_proceso == "no"]
x_si <- df$volumen_consumido_m3[df$usa_agua_proceso == "sí"]

n_no <- length(x_no); n_si <- length(x_si)
m_no <- mean(x_no);   m_si <- mean(x_si)
s2_no <- var(x_no);   s2_si <- var(x_si)
sd_no <- sd(x_no);    sd_si <- sd(x_si)

## 1. Descriptivos por grupo
tabla_desc <- data.frame(
  Grupo = c("No usa agua en proceso","Sí usa agua en proceso"),
  n = c(n_no, n_si),
  Media = c(m_no, m_si),
  Mediana = c(median(x_no), median(x_si)),
  Desv.Est = c(sd_no, sd_si),
  Varianza = c(s2_no, s2_si),
  Q1 = c(quantile(x_no, .25), quantile(x_si, .25)),
  Q3 = c(quantile(x_no, .75), quantile(x_si, .75))
)
print(tabla_desc, row.names = FALSE)

## 2. PRUEBA F CLÁSICA: igualdad de varianzas 
alpha <- 0.05

## Para prueba bilateral, la varianza más grande en el numerador
if (s2_no >= s2_si) {
  Fcalc <- s2_no / s2_si
  gl1 <- n_no - 1; gl2 <- n_si - 1
  comp <- "s2_no / s2_si"
} else {
  Fcalc <- s2_si / s2_no
  gl1 <- n_si - 1; gl2 <- n_no - 1
  comp <- "s2_si / s2_no"
}

## Valores críticos para prueba bilateral (región de rechazo en dos colas)
Fcrit_inf <- qf(alpha/2, df1 = gl1, df2 = gl2)          # límite inferior
Fcrit_sup <- qf(1 - alpha/2, df1 = gl1, df2 = gl2)      # límite superior

decision_var_crit <- if (Fcalc > Fcrit_sup) "Rechazar H0" else "No rechazar H0"

## p-valor bilateral
pval_F <- 2 * (1 - pf(Fcalc, df1 = gl1, df2 = gl2))
pval_F <- min(1, pval_F)  # por seguridad

tabla_F <- data.frame(
  Comparacion = comp,
  F_calculada = round(Fcalc, 4),
  gl_num = gl1, gl_den = gl2,
  `Valor_critico_inf(qF α/2)` = round(Fcrit_inf, 4),
  `Valor_critico_sup(qF 1-α/2)` = round(Fcrit_sup, 4),
  `p_valor(bilateral)` = signif(pval_F, 4),
  Decision = decision_var_crit
)
cat("\n--- PRUEBA F CLÁSICA (igualdad de varianzas) ---\n")
print(tabla_F, row.names = FALSE)

## 3. PRUEBA t DE DOS MUESTRAS INDEPENDIENTES (WELCH)
usar_var_iguales <- (decision_var_crit == "No rechazar H0")
t_res <- t.test(x_no, x_si, var.equal = usar_var_iguales, alternative = "two.sided")

## Valores críticos para t (bilateral)
gl_t <- if (usar_var_iguales) (n_no + n_si - 2) else t_res$parameter
tcrit <- qt(1 - alpha/2, df = gl_t)
decision_t_crit <- if (abs(t_res$statistic) > tcrit) "Rechazar H0" else "No rechazar H0"

tabla_t <- data.frame(
  Comparacion = "μ_no vs μ_sí",
  t_calculada = round(as.numeric(t_res$statistic), 4),
  gl = round(as.numeric(gl_t), 2),
  `Valor_critico_±t(1-α/2)` = round(as.numeric(tcrit), 4),
  `p_valor(bilateral)` = signif(t_res$p.value, 4),
  Varianzas_iguales = if (usar_var_iguales) "Sí (F no rechazó H0)" else "No (F rechazó H0)",
  Decision = decision_t_crit
)
cat("\n--- PRUEBA t (dos muestras) ---\n")
print(tabla_t, row.names = FALSE)

## 4. Gráfico comparativo 
## Boxplot simple por grupo (sin paquetes)
par(mar = c(4.2,4.5,2.5,1))
boxplot(volumen_consumido_m3 ~ usa_agua_proceso, data = df,
        col = "lightblue", border = "black",
        ylab = "Volumen consumido (m³)", xlab = "Usa agua en proceso",
        main = "Volumen consumido por uso de agua en proceso")
grid(nx = NA, ny = NULL, col = "lightgray")


#  --- Objetivo 2: Análisis de varianza (ANOVA de un factor) ---
str(datos)
summary(datos$volumen_consumido_m3)
table(datos$actividad_industrial)

# 4. ANOVA de un factor
modelo <- aov(volumen_consumido_m3 ~ actividad_industrial, data = datos)
summary(modelo)

# 5. Verificación de supuestos
## Normalidad de residuos
shapiro.test(residuals(modelo))

## QQ plot
qqnorm(residuals(modelo))
qqline(residuals(modelo), col = "red")

## Homogeneidad de varianzas
leveneTest(volumen_consumido_m3 ~ actividad_industrial, data = datos)

# 6. Post-hoc si es significativo (Tukey HSD)
TukeyHSD(modelo)

# 7. Gráficas
## Boxplot del consumo por actividad industrial
ggplot(datos, aes(x = actividad_industrial, y = volumen_consumido_m3, fill = actividad_industrial)) +
  geom_boxplot() +
  labs(title = "Consumo de agua por actividad industrial", x = "Actividad Industrial", y = "Volumen Consumido (m³)") +
  theme_minimal()

## Medias con intervalos de confianza
ggplot(datos, aes(x = actividad_industrial, y = volumen_consumido_m3, color = actividad_industrial)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Medias de consumo con IC 95%", x = "Actividad Industrial", y = "Consumo (m³)") +
  theme_minimal()

## Residuos vs Ajustados
plot(modelo, which = 1)

#  --- Objetivo 3: Modelo de regresión lineal simple ---

# 1. Crear subconjunto con variables necesarias
df3 <- datos[, c("numero_empleados", "volumen_consumido_m3")]
df3 <- na.omit(df3)

# 2. Visualización inicial con gráfico de dispersión
ggplot(df3, aes(x = numero_empleados, y = volumen_consumido_m3)) +
  geom_point(color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1) +
  labs(
    title = "Relación entre número de empleados y volumen de agua consumido",
    x = "Número de empleados",
    y = "Volumen consumido (m³)"
  ) +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5))

# 3. Ajustar modelo de regresión lineal simple
modelo3 <- lm(volumen_consumido_m3 ~ numero_empleados, data = df3)

# Mostrar el resumen del modelo
summary(modelo3)

# 4. Mostrar la ecuación del modelo
coeficientes <- coef(modelo3)
intercepto <- coeficientes[1]
pendiente <- coeficientes[2]

cat("La ecuación del modelo ajustado es:\n")
cat("Volumen_consumido_m3 =", round(intercepto, 2),
    "+", round(pendiente, 2), "* numero_empleados\n")

# 5. Gráficos de diagnóstico
par(mfrow = c(2, 2))  # Mostrar los 4 gráficos estándar de regresión
plot(modelo3)


#  --- Objetivo 4: Prueba de hipótesis para comparación de medias ---

df4 <- subset(
  datos[, c("tratamiento_aguas", "actividad_industrial")],
  !is.na(tratamiento_aguas) & !is.na(actividad_industrial)
)

# 1. Estadística descriptiva básica
cat("\n--- Frecuencias marginales ---\n")
print(table(df4$tratamiento_aguas))
print(table(df4$actividad_industrial))

# 2. Tabla de contingencia
tab_cont <- table(Tratamiento = df4$tratamiento_aguas,
                  Actividad  = df4$actividad_industrial)
cat("\n--- Tabla de contingencia (Tratamiento x Actividad) ---\n")
print(tab_cont)

cat("\n--- Proporciones por fila (distribución de Actividad dentro de cada Tratamiento) ---\n")
print(round(prop.table(tab_cont, margin = 1), 3))

cat("\n--- Proporciones por columna (distribución de Tratamiento dentro de cada Actividad) ---\n")
print(round(prop.table(tab_cont, margin = 2), 3))

# 3. Prueba Chi-cuadrado de independencia
chi_res <- chisq.test(tab_cont, correct = FALSE)
cat("\n--- Prueba Chi-cuadrado de independencia ---\n")
print(chi_res)

# 3.1. Revisar frecuencias esperadas
exp_min   <- min(chi_res$expected)
frac_lt5  <- mean(chi_res$expected < 5)
cat(sprintf("\nMínimo esperado: %.3f | Proporción de celdas con esperado < 5: %.3f\n", exp_min, frac_lt5))

# 4. Extraer elementos clave para tabla de informe y para el gráfico
alpha      <- 0.05
chi_stat   <- as.numeric(chi_res$statistic)  # χ² observado
df_chi     <- as.numeric(chi_res$parameter)  # gl = (r-1)*(c-1)
p_value    <- chi_res$p.value
crit_value <- qchisq(1 - alpha, df = df_chi) # valor crítico para cola superior (α)

cat("\n--- Resumen para informe ---\n")
cat(sprintf("χ² = %.3f | gl = %d | Valor crítico χ²(%.2f, %d) = %.3f | p-valor = %.4f\n",
            chi_stat, df_chi, 1 - alpha, df_chi, crit_value, p_value))

# 5. Gráfico de la distribución χ²
x_max <- max(qchisq(0.999, df = df_chi), chi_stat * 1.15)
x_seq <- seq(0, x_max, length.out = 400)
dens  <- dchisq(x_seq, df = df_chi)
chi_df <- data.frame(x = x_seq, density = dens)

ggplot(chi_df, aes(x = x, y = density)) +
  # Curva de densidad teórica
  geom_line(color = "#1E88E5", linewidth = 1.2) +
  # Área de no rechazo
  geom_area(data = subset(chi_df, x <= crit_value),
            aes(y = density), fill = "#BBDEFB", alpha = 0.7) +
  # Área de rechazo (cola superior)
  geom_area(data = subset(chi_df, x > crit_value),
            aes(y = density), fill = "#FFCDD2", alpha = 0.7) +
  # Línea del valor crítico
  geom_vline(xintercept = crit_value, linetype = "dashed",
             color = "#D32F2F", linewidth = 1) +
  # Línea del χ² observado
  geom_vline(xintercept = chi_stat, color = "#00796B", linewidth = 1.2) +
  # Anotaciones
  annotate("text", x = crit_value, y = max(dens)*0.85,
           label = paste0("Valor crítico = ", round(crit_value, 3)),
           hjust = 1.05, color = "#D32F2F", size = 4.2) +
  annotate("text", x = chi_stat, y = max(dens)*0.6,
           label = paste0("χ² = ", round(chi_stat, 2),
                          "\n p = ", format.pval(p_value, digits = 3)),
           hjust = -0.1, color = "#00796B", size = 4.2, fontface = "bold") +
  # Ejes y títulos
  scale_x_continuous(expand = expansion(mult = c(0, 0.02))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  labs(
    title = "Prueba Chi-cuadrado de independencia",
    subtitle = paste0("Distribución χ² con gl = ", df_chi, "  (α = 0.05)"),
    x = "Valor de χ²", y = "Densidad"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(hjust = 0.5, face = "bold", color = "#0D47A1"),
    plot.subtitle = element_text(hjust = 0.5, color = "#424242"),
    panel.grid.minor = element_blank(),
    panel.border     = element_rect(colour = "black", fill = NA, linewidth = .5)
  )

#  --- Objetivo 5: Prueba de hipótesis para comparación de proporciones ---

# 1.Filtrar solo Quito y Guayaquil + crear indicador "usa_pozo"
df <- datos %>%
  filter(ciudad %in% c("Quito", "Guayaquil"),
         !is.na(fuente_agua)) %>%
  mutate(
    usa_pozo = grepl("pozo", fuente_agua, ignore.case = TRUE)
  )

# 2.Conteos y proporciones por ciudad
resumen <- df %>%
  group_by(ciudad) %>%
  summarise(
    n_total = n(),
    n_pozo  = sum(usa_pozo),
    prop_pozo = n_pozo / n_total
  )
print(resumen)

# 3.Tabla 2x2 (ciudad x usa_pozo)
tab_2x2 <- df %>%
  count(ciudad, usa_pozo) %>%
  complete(ciudad, usa_pozo, fill = list(n = 0)) %>%
  pivot_wider(names_from = usa_pozo, values_from = n) %>%
  as.data.frame()

# Extraer conteos en el orden: Quito, Guayaquil
nQ  <- resumen$n_total[resumen$ciudad == "Quito"]
xQ  <- resumen$n_pozo [resumen$ciudad == "Quito"]
nG  <- resumen$n_total[resumen$ciudad == "Guayaquil"]
xG  <- resumen$n_pozo [resumen$ciudad == "Guayaquil"]

## 4. Prueba de dos proporciones (bilateral, con corrección de continuidad por defecto)
pr <- prop.test(x = c(xQ, xG), n = c(nQ, nG), alternative = "two.sided", correct = TRUE)
print(pr)
tidy(pr)  # ordenado


## 5. Tamaño del efecto
pQ <- xQ/nQ; pG <- xG/nG
RD <- pQ - pG                    # Diferencia de proporciones (Quito - Guayaquil)
RR <- (pQ/(1-pQ)) / (pG/(1-pG))  # Riesgo relativo aproximado usando odds? (mejor: RR = pQ/pG)
RR <- pQ / pG
OR <- (xQ*(nG - xG)) / ((nQ - xQ)*xG)

cat("\n--- Tamaños de efecto ---\n")
cat(sprintf("p_Quito = %.4f  |  p_Guayaquil = %.4f\n", pQ, pG))
cat(sprintf("Diferencia de proporciones (Q - G): RD = %.4f\n", RD))
cat(sprintf("Razón de riesgos (Q/G): RR = %.4f\n", RR))
cat(sprintf("Odds Ratio (Q vs G): OR = %.4f\n", OR))

## 6. Gráfico de barras con IC 95% (Wilson vía prop.test por ciudad)
# Función que devuelve IC de una proporción con prop.test
ic_prop <- function(x, n) {
  tmp <- prop.test(x, n)
  c(est = x/n, lwr = tmp$conf.int[1], upr = tmp$conf.int[2])
}

plot_df <- resumen %>%
  rowwise() %>%
  mutate(ci = list(ic_prop(n_pozo, n_total))) %>%
  unnest_wider(ci)

ggplot(plot_df, aes(x = ciudad, y = est)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = lwr, ymax = upr), width = .15) +
  geom_text(aes(label = scales::percent(est, accuracy = 0.1)),
            vjust = -0.6, size = 4) +
  labs(title = "Proporción de industrias que usan agua de pozo",
       x = "Ciudad", y = "Proporción (IC 95%)") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.minor = element_blank(),        
        panel.border = element_rect(colour = "black", fill = NA, linewidth = .5))



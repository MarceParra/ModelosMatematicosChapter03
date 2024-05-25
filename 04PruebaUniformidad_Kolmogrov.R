# Datos proporcionados
datos <- c(0.97, 0.11, 0.65, 0.26, 0.98, 0.03, 0.13, 0.89, 0.21, 0.69)

# Realizar la prueba de Kolmogorov-Smirnov
ks_result <- ks.test(datos, "punif", min = 0, max = 1)

# Calcular D+ y D-
n <- length(datos)
datos_ordenados <- sort(datos)
cdf_teorica <- punif(datos_ordenados, min = 0, max = 1)
empirical_cdf <- seq(1, n) / n

D_plus <- max(empirical_cdf - cdf_teorica)
D_minus <- max(cdf_teorica - (seq(0, n - 1) / n))

# Nivel de confianza
conf_level <- 0.90

# Calcular el valor crítico
alpha <- 1 - conf_level
ks_critical_value <- sqrt(-log(alpha / 2) / (2 * n))

# Imprimir valores de D+ y D-
cat("D+: ", D_plus, "\n")
cat("D-: ", D_minus, "\n")

# Imprimir el valor crítico
cat("Valor crítico:", ks_critical_value, "\n")

# Imprimir la conclusión
if (ks_result$statistic < ks_critical_value) {
  cat("No rechazamos la hipótesis nula.\n")
} else {
  cat("Rechazamos la hipótesis nula.\n")
}

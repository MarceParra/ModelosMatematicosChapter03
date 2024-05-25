# Datos proporcionados
datos <- c(0.34, 0.67, 0.46, 0.11, 0.83, 0.62, 0.22, 0.19, 0.96, 0.05, 0.99, 0.58, 0.47, 0.49, 0.78, 0.34, 0.79, 0.59, 0.39, 0.42, 0.99, 0.42, 0.18, 0.37, 0.37, 0.05, 0.75, 0.31, 0.72, 0.02, 0.73, 0.73, 0.06, 0.74, 0.79, 0.74, 0.18, 0.67, 0.29, 0.21)

# Número de datos
n <- length(datos)
n

# Calcular el número de corridas observadas (S)
S <- sum(diff(datos > mean(datos)) != 0)
S

# Calcular la media y varianza de S
media_S <- (2 * n - 1) / 3
media_S
varianza_S <- (16 * n - 29) / 90
varianza_S

# Calcular el valor esperado de Z
valor_observado_Z = abs((S-media_S)/sqrt(varianza_S))
valor_observado_Z

# Calcular el valor crítico
alpha <- 0.05  # Nivel de confianza del 95%
z_critico <- qnorm(1 - (alpha/2))

# Imprimir los resultados
cat("Número de corridas observadas (S):", S, "\n")
cat("Media de S:", media_S, "\n")
cat("Varianza de S:", varianza_S, "\n")
cat("Valor esperado de S:", valor_observado_Z, "\n")
cat("Valor crítico:", z_critico, "\n")

# Imprimir conclusión
if (valor_observado_Z <= z_critico) {
  cat("No se rechaza la hipótesis nula.\n")
} else {
  cat("Se rechaza la hipótesis nula.\n")
}

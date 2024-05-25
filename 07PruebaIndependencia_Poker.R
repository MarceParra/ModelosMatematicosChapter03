# Datos proporcionados
r <- c(0.06141, 0.72484, 0.94107, 0.56766, 0.14411, 0.87648, 0.81792, 0.48999, 0.18590, 0.06060, 
       0.11223, 0.64794, 0.52953, 0.50502, 0.30444, 0.70688, 0.25357, 0.31555, 0.04127, 0.67347, 
       0.28103, 0.99367, 0.44598, 0.73997, 0.27813, 0.62182, 0.82578, 0.85623, 0.51483, 0.09099)

# Función para clasificar los números en manos de póker
categorize_poker <- function(numbers) {
  categories <- c("TD" = 0, "1P" = 0, "2P" = 0, "T" = 0, "TP" = 0, "P" = 0, "Q" = 0)
  for (num in numbers) {
    digits <- substr(num, 3, 7)  # Tomamos los 5 dígitos decimales
    digit_count <- table(strsplit(digits, NULL)[[1]])
    counts <- sort(digit_count, decreasing = TRUE)
    
    if (length(counts) == 5) {
      categories["TD"] <- categories["TD"] + 1
    } else if (length(counts) == 4) {
      categories["1P"] <- categories["1P"] + 1
    } else if (length(counts) == 3) {
      if (max(counts) == 3) {
        categories["T"] <- categories["T"] + 1
      } else {
        categories["2P"] <- categories["2P"] + 1
      }
    } else if (length(counts) == 2) {
      if (max(counts) == 4) {
        categories["P"] <- categories["P"] + 1
      } else {
        categories["TP"] <- categories["TP"] + 1
      }
    } else if (length(counts) == 1) {
      categories["Q"] <- categories["Q"] + 1
    }
  }
  return(categories)
}

# Calcular las frecuencias observadas
observed_frequencies <- categorize_poker(r)

# Frecuencias esperadas según las probabilidades proporcionadas
n <- length(r)
expected_frequencies <- c("TD" = 0.3024 * n, 
                          "1P" = 0.5040 * n, 
                          "2P" = 0.1080 * n, 
                          "TP" = 0.0090 * n, 
                          "T" = 0.0720 * n, 
                          "P" = 0.0045 * n, 
                          "Q" = 0.0001 * n)

# Calcular el estadístico chi-cuadrado
chi_squared <- sum((observed_frequencies - expected_frequencies)^2 / expected_frequencies)

# Grados de libertad
df <- length(expected_frequencies) - 1

# Valor crítico de chi-cuadrado para una confiabilidad del 95% y seis grados de libertad
critical_value <- qchisq(0.95, df)

# Valor p
p_value <- pchisq(chi_squared, df, lower.tail = FALSE)

# Mostrar resultados
print("Frecuencias observadas:")
print(observed_frequencies)

print("Frecuencias esperadas:")
print(expected_frequencies)

print(paste("Chi-cuadrado calculado:", chi_squared))
print(paste("Chi-cuadrado crítico (95% de confianza, 6 grados de libertad):", critical_value))

print(paste("Valor p:", p_value))

# Interpretar el resultado
if (p_value < 0.05) {
  print("Rechazamos la hipótesis nula: Los números no son independientes.")
} else {
  print("No podemos rechazar la hipótesis nula: Los números son independientes.")
}

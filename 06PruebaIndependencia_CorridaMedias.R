# Datos proporcionados
r <- c(0.809, 0.042, 0.432, 0.538, 0.225, 0.88, 0.688, 0.772, 0.036, 0.854, 0.397, 0.268, 0.821, 0.897, 0.07, 0.721, 0.087, 0.35, 0.779, 0.482, 0.136, 0.855, 0.453, 0.197, 0.444, 0.799, 0.809, 0.691, 0.545, 0.857, 0.692, 0.055, 0.348, 0.373, 0.436, 0.29, 0.015, 0.834, 0.599, 0.724, 0.564, 0.709, 0.946, 0.754, 0.677, 0.128, 0.012, 0.498, 0.6, 0.913)

# Número de datos
n <- length(r)
print(paste("Número de datos:", n))

# Media de referencia
media_ref <- 0.5

# Convertir los datos a 1s y 0s
secuencia <- ifelse(r > media_ref, 1, 0)
secuencia

# Calcular el número de corridas observadas (S)
C0 <- sum(diff(r > media_ref) != 0) + 1
C0

# Contar el número de ceros y unos
n0 <- sum(secuencia == 0)
n1 <- sum(secuencia == 1)

print(paste("Número de ceros (n0):", n0))
print(paste("Número de unos (n1):", n1))

# Calcular la media de la corrida
mediaC0=((2*n0*n1)/n)+0.5
print(paste("uC0:", mediaC0))

# Calcular la varianza de la corrida
varianzaC0=((2*n0*n1)*((2*n0*n1)-n))/((n^2)*(n-1))
print(paste("sigmaC0:", varianzaC0))

#Calcular el valor esperado de Z
valor_observado_Z = (C0-mediaC0)/(sqrt(varianzaC0))
valor_observado_Z

# Calcular el valor crítico
alpha <- 0.05  # Nivel de confianza del 95%
Z_critico_pos <- qnorm(1 - (alpha/2))
Z_critico_neg = (-1)*Z_critico_pos
print(paste("Zcritico negativo:", Z_critico_pos))
print(paste("Zcritico positivo:", Z_critico_neg))

# Imprimir conclusión
if ((valor_observado_Z > -Z_critico)&&(valor_observado_Z < Z_critico)) {
  cat("No se rechaza la hipótesis nula.\n")
} else {
  cat("Se rechaza la hipótesis nula.\n")
}

#Se desarrollo por la Ing. Marcela Parra, Mg.
#Prueba de Varianza para Números Aleatorios del 0 al 1

#ingresar el valor de n
n=100
n

#Definir el valor de m
m = sqrt(n) 
m

#Ingresar el valor de alpha
alpha=0.05
alpha

#Ingresar los valores de r
r <- c(0.347, 0.832, 0.966, 0.472, 0.797, 0.101, 0.696, 0.966, 0.404, 0.603, 0.993, 0.371, 0.729, 0.067, 0.189, 0.977, 0.843, 0.562, 0.549, 0.992, 0.674, 0.628, 0.055, 0.494, 0.494, 0.235, 0.178, 0.775, 0.797, 0.252, 0.426, 0.054, 0.022, 0.742, 0.674, 0.898,0.641, 0.674, 0.821, 0.19, 0.46, 0.224, 0.99, 0.786, 0.393, 0.461, 0.011, 0.977, 0.246, 0.881, 0.189, 0.753, 0.73, 0.797, 0.292, 0.876, 0.707, 0.562, 0.562, 0.821, 0.112, 0.191, 0.584, 0.347, 0.426, 0.057, 0.819, 0.303, 0.404, 0.64, 0.37, 0.314, 0.731, 0.742, 0.213, 0.472, 0.641, 0.944, 0.28, 0.663, 0.909, 0.764, 0.999, 0.303, 0.718, 0.933, 0.056, 0.415, 0.819, 0.444, 0.178, 0.516, 0.437, 0.393, 0.268, 0.123, 0.945, 0.527, 0.459, 0.652)
r

#Graficar histograma de los datos
hist(r)

# Crear los límites de los intervalos
interval_size = 1/m
breaks <- seq(0, 1, by = interval_size)

#Crear la tabla de frecuencias observada
data_cutr <- cut(r, breaks = breaks, include.lowest = TRUE, right = FALSE)
freq_obs_table <- table(data_cutr)
freq_obs_table

#Graficar las frecuencias observadas en barra
barplot(freq_obs_table)

#Graficar la frecuencia relativa
freq_espe_table = table(n/m)
freq_espe_table

#Calcular X0 de cada uno
chi_calculado_table = ((freq_obs_table-(n/m))^2/(n/m))
chi_calculado_table

# Sumar los valores de chi cuadrado calculados para obtener el chi cuadrado total
chi_total <- sum(chi_calculado_table)

# Imprimir chi cuadrado total
chi_total


# Calcular la frecuencia esperada
freq_esperada <- n / m

# Calcular chi cuadrado para cada intervalo
chi_calculado <- ((freq_obs_table - freq_esperada)^2) / freq_esperada

# Crear un data frame con los resultados
resultados <- data.frame(
    Intervalo = levels(data_cutr),
    Frecuencia_Observada = as.vector(freq_obs_table),
    Frecuencia_Esperada = rep(freq_esperada, length(freq_obs_table)),
    Chi_Cuadrado_Calculado = as.vector(chi_calculado)
)

# Imprimir la tabla de resultados
print(resultados)

#obtener el valor critico de Chi Cuadrado
# Grados de libertad (m - 1)
gl <- m - 1
# Valor crítico de chi cuadrado
chi_critico <- qchisq(1 - alpha, df = gl)
# Imprimir el valor crítico de chi cuadrado
chi_critico

# Comparar chi_total con chi_critico
if (chi_total < chi_critico) {
    conclusion <- "No se rechaza la hipótesis nula. Los datos se distribuyen uniformemente."
} else {
    conclusion <- "Se rechaza la hipótesis nula. Los datos no se distribuyen uniformemente."
}

# Imprimir la conclusión
conclusion

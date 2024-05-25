# Datos proporcionados
datos <- c(0.872, 0.950, 0.343, 0.058, 0.384, 0.219, 0.041, 0.036, 0.213, 0.946,
           0.570, 0.842, 0.706, 0.809, 0.300, 0.618, 0.512, 0.462, 0.005, 0.203,
           0.291, 0.151, 0.596, 0.443, 0.868, 0.913, 0.511, 0.586, 0.608, 0.879)

# Definir límites alpha y beta
alpha <- 0.8
beta <- 1

# Generar la secuencia binaria
secuencia_binaria <- ifelse(datos >= alpha & datos <= beta, 1, 0)

# Inicializar variables para contar huecos y sus tamaños
huecos <- numeric(0)
tamano_hueco <- 0

# Variable para indicar si el último valor fue un uno
ultimo_uno <- FALSE

# Recorrer la secuencia binaria
for (i in 1:length(secuencia_binaria)) {
  # Si el valor es 0, aumentar el tamaño del hueco
  if (secuencia_binaria[i] == 0) {
    tamano_hueco <- tamano_hueco + 1
  } else {
    # Si el valor es 1 y el último valor también fue 1, añadir un hueco de tamaño 0
    if (ultimo_uno) {
      huecos <- c(huecos, 0)
    }
    # Si el valor es 1 y hay un hueco, almacenar el tamaño del hueco y reiniciar
    if (tamano_hueco > 0) {
      huecos <- c(huecos, tamano_hueco)
      tamano_hueco <- 0
    }
    # Actualizar la variable último_uno
    ultimo_uno <- TRUE
  }
  # Si el valor es 0, actualizar último_uno
  if (secuencia_binaria[i] == 0) {
    ultimo_uno <- FALSE
  }
}

# Contar cuántas observaciones hay para cada tamaño de hueco
conteo_huecos <- table(huecos)

# Crear una tabla para los tamaños de huecos del 0 al 4
tamano_hueco <- 0:4
observaciones <- rep(0, length(tamano_hueco))

# Llenar las observaciones correctas
for (i in 1:length(tamano_hueco)) {
  if (as.character(tamano_hueco[i]) %in% names(conteo_huecos)) {
    observaciones[i] <- conteo_huecos[as.character(tamano_hueco[i])]
  }
}

# Sumar el número de observaciones para tamaños de hueco mayores o iguales a 5
observaciones_5_o_mas <- sum(conteo_huecos[names(conteo_huecos) >= "5"])

# Crear una tabla para el tamaño de hueco de 5 o más
observaciones_5_o_mas <- sum(conteo_huecos[as.numeric(names(conteo_huecos)) >= 5])

# Crear la tabla de observaciones
tamano_hueco <- c(tamano_hueco, "5 o más")
observaciones <- c(observaciones, observaciones_5_o_mas)

# Imprimir la tabla
tabla_huecos <- data.frame(Tamano_Hueco = tamano_hueco, Observaciones = observaciones)
print(tabla_huecos)

# Calcular el número total de huecos
numero_total_huecos <- length(huecos)

# Imprimir el resultado
print(numero_total_huecos)

# Convertir la columna Tamano_Hueco a tipo numérico
tabla_huecos$Tamano_Hueco <- as.numeric(as.character(tabla_huecos$Tamano_Hueco))

# Calcular la frecuencia esperada según la fórmula
frecuencia_esperada <- numeric(nrow(tabla_huecos))
for (i in 1:(nrow(tabla_huecos) - 1)) {
  frecuencia_esperada[i] <- numero_total_huecos * (beta - alpha) * ((1 - (beta - alpha))^tabla_huecos$Tamano_Hueco[i])
}
frecuencia_esperada[nrow(tabla_huecos)] <- numero_total_huecos * ((1 - (beta - alpha))^5)

# Agregar la columna de frecuencia esperada a la tabla_huecos
tabla_huecos$Frecuencia_Esperada <- frecuencia_esperada

# Imprimir la tabla actualizada
print(tabla_huecos)

# Calcular el error estadístico chi
error_chi <- ((tabla_huecos$Frecuencia_Esperada - tabla_huecos$Observaciones)^2) / tabla_huecos$Frecuencia_Esperada

# Agregar la columna de error estadístico chi a la tabla_huecos
tabla_huecos$Error_Chi <- error_chi

# Imprimir la tabla actualizada
print(tabla_huecos)

# Sumar todos los valores de error chi
error_chi_total <- sum(tabla_huecos$Error_Chi)

# Imprimir el resultado
print(error_chi_total)

#Calcular el error critico 
# Nivel de significancia (en porcentaje)
nivel_significancia <- 5  # Por ejemplo, para un nivel de significancia del 5%

# Grados de libertad
grados_libertad <- 5

# Calcular el nivel de confianza
nivel_confianza <- 1 - (nivel_significancia / 100)

# Calcular el valor crítico de chi cuadrado
chi_critico <- qchisq(nivel_confianza, df = grados_libertad)

# Imprimir el valor crítico de chi cuadrado
print(chi_critico)

# Comparar chi_critico y error_chi_total
if (chi_critico > error_chi_total) {
  conclusion <- "Se aceptan los valores aleatorios."
} else {
  conclusion <- "Se rechazan los valores aleatorios."
}

# Imprimir la conclusión
print(conclusion)

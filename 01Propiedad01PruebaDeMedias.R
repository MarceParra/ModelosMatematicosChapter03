#Se desarrollo por la Ing. Marcela Parra, Mg.
#Prueba de Medias para Números Aleatorios del 0 al 1


#ingresar los valores de r

x <- c(0.0449, 0.1733, 0.5746, 0.049, 0.8406, 0.8349, 0.92, 0.2564, 0.6015, 0.6694, 0.3972, 0.7025, 0.1055, 0.1247, 0.1977, 0.0125, 0.63, 0.2531, 0.8297, 0.6483, 0.6972, 0.9582, 0.9085, 0.8524, 0.5514, 0.0316, 0.3587, 0.7041, 0.5915, 0.2523, 0.2545, 0.3044, 0.0207, 0.1067, 0.3557, 0.1746, 0.3362, 0.1589, 0.3727, 0.4145)

#ingresar el valor de n
n <- length(x)
n


#calcular la media de x
promedio<-mean(x)
promedio

#obtener el valor de z = alpha/2
zeta=qnorm(1-(0.05/2))

#obtener el límites inferior
li=0.5-((zeta)*(1/(sqrt(12*n))))
li

#obtener el límites superior
ls=0.5+((zeta)*(1/(sqrt(12*n))))
ls

#conclusión

if((promedio>=li) && (promedio<=ls)) {
    print("Se acepta el conjunto de números aleatorios")
  } else {
    print("Se rechaza el conjunto de números aleatorios")
  }

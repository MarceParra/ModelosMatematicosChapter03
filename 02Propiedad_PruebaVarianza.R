#Se desarrollo por la Ing. Marcela Parra, Mg.
#Prueba de Varianza para Números Aleatorios del 0 al 1

#ingresar el valor de alpha
alpha=0.05
alpha

#ingresar los valores de r
x <- c(0.0449, 0.1733, 0.5746, 0.049, 0.8406, 0.8349, 0.92, 0.2564, 0.6015, 0.6694, 0.3972, 0.7025, 0.1055, 0.1247, 0.1977, 0.0125, 0.63, 0.2531, 0.8297, 0.6483, 0.6972, 0.9582, 0.9085, 0.8524, 0.5514, 0.0316, 0.3587, 0.7041, 0.5915, 0.2523, 0.2545, 0.3044, 0.0207, 0.1067, 0.3557, 0.1746, 0.3362, 0.1589, 0.3727, 0.4145)
x

#ingresar el valor de n
n <- length(x)
n

#calcular la varianza x
varianza<-var(x)
varianza

#calcular Chi Cuadrada para límite superior
chisup=qchisq((1-(alpha/2)),(n-1))
chisup


#calcular Chi Cuadrada para límite inferior 
chiinf=qchisq(((1-alpha)/2),(n-1))
chiinf



#Calcular límite inferior
LIV=(chiinf)/(12*(n-1))
LIV

#Calcular límite superior
LSV=(chisup)/(12*(n-1))
LSV

#Conclusión
if((varianza>=LIV)&&(varianza<=LSV)){
  "Se acepta el conjunto de números aleatorios"
} else {
  "Se rechaza el conjunto de números aleatorios"
}
library(astsa)
library(tseries)
library(MASS)
library(forecast)
library(stats)
library(tsoutliers)
library(fpp)

#importando archivo CSV de los datos 

ventaspiegigante <- read.csv("~/seminario/RSSI_dataset/series de tiempo/ventaspiegigante.csv", header=FALSE, sep=";")
View(ventaspiegigante)

#convirtiendo el data en una serie de tiempo
ventaspiegigante.ts=ts(ventaspiegigante$V2,start=c(2017,5),frequency = 12)
plot(main="Ventas mensuales de pizzas de 2017 a 2021",ventaspiegigante.ts)

#buscando y limpiando outliers 
ventas=tsclean(ventaspiegigante.ts)
ventasdata=data.frame(ventas)
#Graficamos la nueva serie sin outliers

plot(main="Ventas mensuales de pizzas de 2017 a 2021 sin valores atípicos ",ventas)


#prueba de que la serie es estacionaria 
pp.test(ventas)
#diferenciacion de serie de tiempo
seriediferenciada1=diff(ventas,differences = 1)
pp.test(seriediferenciada1)
plot(seriediferenciada1)


#identificacion del modelo de la serie limpia 

pacf(seriediferenciada1)

modeloD1=arima(ventas,c(1,1,0))
modeloD2=arima(ventas,c(2,1,0))
criteriosDI=data.frame(AIC(modeloD1,modeloD2),BIC(modeloD1,modeloD2))

#simulacion del proceso AR de la serie limpia
plot(main="Ventas mensuales de pizzas de 2017 a 2021",ventas)
lines(fitted(arima(ventas,c(1,1,0))),col=1,lty=2)
legend("topleft",c("Data","ARI(1,1)"),col = c(1,1),lty=1:2,bty="n")
summary(modeloD1)

#predicion de l=12 pasos. es decir 12 meses de prediccion con drift
prediccion1=forecast(Arima(ventas,c(1,1,0),include.drift=TRUE),h=4)
plot(prediccion1)
summary(prediccion1)



























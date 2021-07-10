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
outliers = tso(ventaspiegigante.ts) 
plot(outliers)
outliers2=tsclean(ventaspiegigante.ts)

#Graficamos la nueva serie sin outliers

plot(main="serie de tiempo sin valores atípicos ",outliers2)


#prueba de que la serie es estacionaria 
adf.test(outliers2)

#diferenciacion de serie de tiempo

seriediferenciada3=diff(outliers2,differences = 3)
plot(seriediferenciada3)
adf.test(seriediferenciada3)

#identificacion del modelo de la serie limpia 
pacf(seriediferenciada3)
modeloB1=arima(outliers2,c(1,3,0))
modeloB2=arima(outliers2,c(2,3,0))
modeloB3=arima(outliers2,c(3,3,0))
modeloB4=arima(outliers2,c(4,3,0))
criteriosBI=data.frame(AIC(modeloB1,modeloB2,modeloB3,modeloB4),BIC(modeloB1,modeloB2,modeloB3,modeloB4))

#simulacion del proceso AR de la serie limpia
summary(modeloB4)
plot(main="Ventas mensuales de pizzas de 2017 a 2021",outliers2)
lines(fitted(arima(outliers2,c(4,3,0))),col=1,lty=2)
legend("topleft",c("Data","AR(4)"),col = c(1,1),lty=1:2,bty="n")
summary(modeloB4)

#-------------------------------------------------------------------------------------------------------------#

#++obcional para efectos de investigacion y autocritica. serie original sin intervencion++

#prueba de que la serie es estacionaria   
adf.test(ventaspiegigante.ts)

#diferenciacion de serie de tiempo original
seriediferenciada=diff(diff(ventaspiegigante.ts))
plot(seriediferenciada)
adf.test(seriediferenciada)

#identificacion del modelo de la serie original 
pacf(seriediferenciada)
modeloA1=arima(ventaspiegigante.ts,c(1,2,0))
modeloA2=arima(ventaspiegigante.ts,c(2,2,0))
modeloA3=arima(ventaspiegigante.ts,c(3,2,0))
criteriosAI=data.frame(AIC(modeloA1,modeloA2,modeloA3),BIC(modeloA1,modeloA2,modeloA3))

#simulacion del proceso AR
summary(modeloA3)
plot(main="Ventas mensuales de pizzas de 2017 a 2021",ventaspiegigante.ts)
lines(fitted(arima(ventaspiegigante.ts,c(3,2,0))),col=1,lty=2)
legend("topleft",c("Data","AR(3)"),col = c(1,1),lty=1:2,bty="n")



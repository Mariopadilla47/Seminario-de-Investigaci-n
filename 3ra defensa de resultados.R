library(astsa)
library(tseries)
library(MASS)
library(forecast)
library(stats)
library(tsoutliers)
library(fpp)
library(fpp2)

# forecasting de la serie estacionaria lynx  
data(lynx)
class(lynx)
plot(lynx)
#como ya teniamos analizada esta serie solo basta hacer la prediccion 
#bajo el modelo AR(4) 
#prediccion de 12 saltos 
plot(forecast(arima(lynx,c(4,0,0)),h=12))

#forecasting de la serie con tendencia 

data(elecsales)
class(elecsales)
plot(elecsales)
adf.test(elecsales)
pp.test(elecsales)
#graficamente y por los test se nota que la serie no es estacionaria
#se estacionara acontinuacion 

elecdiferenciada1=diff(elecsales,differences = 1)
plot(elecdiferenciada1)
adf.test(elecdiferenciada1)
pp.test(elecdiferenciada1)
#vemos que ya con la prueba de Phillips-perron ya se alcanza la estacionariedad
#ya que es mas robusta que la de dickey-fuller

#identificacion del modelo 
pacf(elecdiferenciada1)
#por la funcion de autocorrelacion parcial sabemos que basta en AR(1)
modeloC1=arima(elecdiferenciada1,c(1,1,0))
plot(main="Ventas anuales de electricidad para Australia del Sur en GWh de 1989 a 2008.",elecsales)
lines(fitted(arima(elecsales,c(1,1,0))),col=1,lty=2)
legend("topleft",c("Data","ARI(1,1)"),col = c(1,1),lty=1:2,bty="n")
par(mfrow=c(1,2))
plot(forecast(arima(elecsales,c(1,1,0)),h=12))
plot(forecast(Arima(elecsales,c(1,1,0),include.drift=TRUE),h=12))


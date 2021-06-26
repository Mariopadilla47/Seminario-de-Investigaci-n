library(astsa)
library(tseries)
library(MASS)
library(forecast)
library(stats)
#grafica de la serie de tiempo 
data(lynx)
class(lynx)
plot(lynx)
#pruebas de estacionariedad
adf.test(lynx)
#identificacion del modelo 
pacf(lynx)
modelo1=arima(lynx,c(1,0,0))
modelo2=arima(lynx,c(2,0,0))
modelo3=arima(lynx,c(3,0,0))
modelo4=arima(lynx,c(4,0,0))
criteriosI=data.frame(AIC(modelo1,modelo2,modelo3,modelo4),BIC(modelo1,modelo2,modelo3,modelo4))
#simulacion del modelo terminado
summary(modelo4)
plot(main="Números anuales de trampas de lince para 1821-1934 en Canadá",lynx)
lines(fitted(arima(lynx,c(4,0,0))),col=1,lty=2)
legend("topleft",c("Data","AR(4)"),col = c(1,1),lty=1:2,bty="n")
auto=auto.arima(lynx,stepwise=FALSE,approximation=FALSE)
summary(auto)
modelo4$model

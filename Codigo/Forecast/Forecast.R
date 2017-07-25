# Librerías auxiliares
library(zoo)
library(ggplot2)

# Instalamos y cargamos forecast
install.packages('forecast')
library(forecast)
# Instalando forecast desde el repositorio oficial en GitHub (se recomienda instalar del CRAN)
install.packages("githubinstall")
library(githubinstall)
# Necesario instalar RTools (en caso de no tenerlo se instala por defecto)
githubinstall('forecast') # robjhyndman/forecast  forecast package for R
library(forecast)

# Cargamos los datos desde la API de datamarket (necesaria conexión a internet)
library(rdatamarket)
accidentes <- as.ts(dmseries('http://data.is/1yFXOBi')) # En caso de error se incluye csv en directorio
# Training y test 
acc.train <- window(accidentes, start = c(1960,1), end = c(1973,12))
acc.test <- window(accidentes, start = c(1974,1))
# Plotting
plot(accidentes)
# Plotting con ggplot del training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train)), y = coredata(as.zoo(acc.train))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test))), col = 'red')
p
# Función genérica para plotting de test vs pred a través de ggplot2
combine <- function(test, pred) {
  require(ggplot2)
  p <- ggplot() +
    geom_line(aes(x = index(as.zoo(test)), y = coredata(as.zoo(test)), colour = 'Test')) +
    geom_line(aes(x = index(as.zoo(acc.test)), y = pred, colour = 'Prediccion')) +
    scale_color_manual(name = '', values = c('Test' = 'black', 'Prediccion' = 'red'),
                       labels = c('Test','Predicción')) + ylab('Número de accidentes de tráfico') +
    xlab('Año 1974') + scale_x_continuous(breaks = c(), labels = c())
  p
}
# Frecuencia estacional de nuestros datos
findfrequency(x = acc.train)
monthdays(x = acc.train)
seasonplot(x = acc.train, s = 12)
seasonplot(x = acc.train, s = findfrequency(acc.train))
# Descomposición de una serie temporal e independencia de los residuos (STATS PACKAGE)
acc.train.decomp <- decompose(x = acc.train, type = 'multiplicative')
seasonal(object = acc.train.decomp)
trendcycle(object = acc.train.decomp)
remainder(object = acc.train.decomp)
Box.test(x = acc.train.decomp$random, type = 'Ljung-Box') # Son ruido blanco
# Desestacionalización a través de objectos decompose
acc.train.seasadj <- seasadj(acc.train.decomp)
plot(acc.train.seasadj)
ndiffs(acc.train.seasadj, test = 'kpss') # 1
ndiffs(acc.train.seasadj, test = 'adf') # 0
ndiffs(acc.train.seasadj, test = 'pp') # 0
# Visualizando la tendencia
tend <- ma(acc.train, order = 12)
autoplot(tend)
# Prediciendo nuevos valores con la media
model <- meanf(acc.train, h = 12)
autoplot(model)
model$mean[1] == mean(acc.train) # TRUE
combine(model$mean, acc.test)
# Medimos la precisión del modelo
accuracy(model, acc.test) # 36.28770
# Revisamos los residuos
Box.test(model$residuals, lag = 12, type = 'Ljung-Box')
checkresiduals(model) # No son ruido blanco
#  Modelo ingenuo en el que se predice con el valor anterior observado
model <- naive(acc.train, h = 12)
autoplot(model)
combine(model$mean, acc.test)
accuracy(model, acc.test)
checkresiduals(model)
#  Modelo ingenuo en el que se predice con el valor anterior observado con constante != 0
model <- rwf(acc.train, h = 12, drift = TRUE)
autoplot(model)
combine(model$mean, acc.test)
accuracy(model, acc.test)
checkresiduals(model)
# Modelo ingenuo que tiene en cuenta la estacionalidad
model <- snaive(acc.train, h = 12)
autoplot(model)
combine(model$mean, acc.test)
accuracy(model, acc.test) # 22.58333
checkresiduals(model) # No son ruido blanco
# Ajuste de un modelo lineal con tendencia y estacionalidad a los datos
model <- tslm(acc.train ~ trend + season)
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # 19.44597
checkresiduals(model) # No son ruido blanco
# Damped HoltWinters Mutiplicativo
model <- hw(acc.train, h = 12, damped = TRUE, seasonal = 'multiplicative',
            initial = 'optimal') 
summary(model)
autoplot(model)
combine(model$mean, acc.test)
accuracy(model, acc.test) # 15.04762
checkresiduals(model) # No son ruido blanco
# Damped HoltWinters Aditivo
model <- hw(acc.train, h = 12, damped = TRUE, seasonal = 'additive',
            initial = 'optimal') 
summary(model)
autoplot(model)
combine(model$mean, acc.test)
accuracy(model, acc.test) # 19.67722
checkresiduals(model) # No son ruido blanco
# Red Neuronal por defecto
# (Las medidas de precisión pueden variar debido al caracter aleatorio de las redes neuronales)
model <- nnetar(acc.train)
pred <- forecast(model, h = 12)
summary(model)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # 26....
checkresiduals(model) # no hay grados de libertad
Box.test(model$residuals, lag = 12, type = 'Ljung-Box')
# Red Neuronal personalizada 1
model <- nnetar(acc.train, decay = 9.5)
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # 14....
checkresiduals(model) # no hay grados de libertad
Box.test(model$residuals, lag = 12, type = 'Ljung-Box')
# Red Neuronal personalizada 2
model <- nnetar(acc.train, repeats = 25, size = 20, decay = 9.5, p = 20, P = 4)
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # 13....
checkresiduals(model) # no hay grados de libertad
Box.test(model$residuals, lag = 12, type = 'Ljung-Box')
# Técnicas ARIMA
autoplot(acc.train)
# Ajustamos la varianza con log y con Box Cox
autoplot(log(acc.train))
acc.train.trans <- BoxCox(acc.train, lambda = BoxCox.lambda(acc.train))
autoplot(acc.train.trans)
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(log(acc.train))), y = coredata(as.zoo(log(acc.train)))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(log(acc.test))), y = coredata(as.zoo(log(acc.test)))), col = 'red')
p
# ¿Nuestra serie es estacionaria?
ndiffs(acc.train) # 1 
nsdiffs(acc.train) # 0
# Diferenciamos la serie (no estacionalmente)
autoplot(diff(acc.train))
autoplot(Acf(diff(acc.train))) # MA -> 0 | 2 (12)
autoplot(Pacf(diff(acc.train))) # AR -> 0 | 0 (12)
# Ajustamos un SARIMA(0,1,0)(0,0,2)12
model <- Arima(acc.train, order = c(0,1,0), seasonal = c(0,0,2))
summary(model) # AIC = 1550.38
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 22.19949
checkresiduals(model) # No son ruido blanco
# Diferenciamos estacionalmente (se pierde la estacionalidad)
accidentes.diff12 <- diff(accidentes, 12) # perdemos un año de observaciones
acc.train.diff12 <- window(accidentes.diff12, start = c(1961,1), end = c(1973,12))
acc.test.diff12 <- window(accidentes.diff12, start = c(1974,1))
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.diff12)),
                y = coredata(as.zoo(acc.train.diff12))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.diff12)),
                y = coredata(as.zoo(acc.test.diff12))), col = 'red')
p

autoplot(Acf(acc.train.diff12)) # MA -> 1 | 1 (12)
autoplot(Pacf(acc.train.diff12)) # AR -> 1 | 2 (12)
# Ajustamos un SARIMA(1,0,1)(2,1,1)12 (Sin drift)
model <- Arima(acc.train, order = c(1,0,1), seasonal = c(2,1,1), include.drift = FALSE) 
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 19.22651
checkresiduals(model) # 0.04321 No ruido blanco
# Ajustamos un SARIMA(1,0,1)(2,1,1)12 (Con drift)
model <- Arima(acc.train, order = c(1,0,1), seasonal = c(2,1,1), include.drift = TRUE) 
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 17.0999
checkresiduals(model) # 0.2748 Ruido blanco
# Selección automática del modelo (SARIMA(1,0,0)(1,0,0)12)
model <- auto.arima(acc.train, test = 'adf')
summary(model) # AIC = 1514.22
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test[1:12])
accuracy(pred, acc.test) # MAE = 14.86938
checkresiduals(model) # No son ruido blanco
# Selección automática del modelo (sin ahorrar coste computacional) (SARIMA (2,0,0)(2,0,0)12)
model <- auto.arima(acc.train, test = 'adf', max.order = 7, stepwise = FALSE, approximation = FALSE)
summary(model) # AICc = 1499.24
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 14.96979
checkresiduals(model) # No son ruido blanco
# Selección automática del modelo (sin ahorrar coste computacional) (SARIMA (1,1,4)(2,0,0)12)
model <- auto.arima(acc.train, max.order = 7, stepwise = FALSE, approximation = FALSE)
summary(model) # AICc = 1485.29
pred <- forecast(model, h = 12)
autoplot(pred)
combine(pred$mean, acc.test)
accuracy(pred, acc.test) # MAE = 23.44705
checkresiduals(model) # No son ruido blanco






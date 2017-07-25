
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)
library(FitARMA)
library(opera)

# Preparamos los datos
datos <- read.csv('monthly-traffic-fatalities-in-on.csv')
accidentes <- ts(datos, start = c(1960, 1), frequency = 12)

acc.train <- window(accidentes, start = c(1960,1), end = c(1973,12))
acc.test <- window(accidentes, start = c(1974,1))
# Función genérica para plotting de test vs pred a través de ggplot2
combine <- function(test, pred) {
  require(ggplot2)
  p <- ggplot() +
    geom_line(aes(x = index(as.zoo(test)), y = coredata(as.zoo(test)), colour = 'Test')) +
    geom_line(aes(x = index(as.zoo(test)), y = pred, colour = 'Prediccion')) +
    scale_color_manual(name = '', values = c('Test' = 'black', 'Prediccion' = 'red'),
                       labels = c('Test','Predicción')) +
    xlab('Año 1974') + ylab('Número de accidentes de tráfico') +
    scale_x_continuous(breaks = c(), labels = c())
  p
}

# Figura 34
red <- nnetar(acc.train, repeats = 25, size = 20, decay = 9.5, p = 20, P = 4)
pred.red <- forecast(red, h = 12)
accuracy(pred.red, acc.test)

sarima <- auto.arima(acc.train, test = 'adf')
pred.sarima <- forecast(sarima, h = 12)
accuracy(pred.sarima, acc.test)

pred.hw <- hw(acc.train, h = 12, damped = TRUE, seasonal = 'multiplicative',
              initial = 'optimal')
accuracy(pred.hw, acc.test)

X <- cbind(red = pred.red$mean, sarima = pred.sarima$mean, hw = pred.hw$mean)
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(as.zoo(acc.test)), colour = 'Original')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = X[, 1], colour = 'Red Neuronal')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = X[, 2], colour = 'SARIMA')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = X[, 3], colour = 'Holt-Winters')) +
  scale_color_manual(name = '', values = c('Original' = 'black',
                                                  'Red Neuronal' = 'blue',
                                                  'SARIMA' = 'red',
                                                  'Holt-Winters' = 'green'),
                     labels = c('Holt-Winters','Original', 'Red Neuronal', 'SARIMA')) +
  xlab('Año 1974') + ylab('Número de accidentes de\ntráfico') +
  scale_x_continuous(breaks = c(), labels = c())
p

# Figura 35
oracle.convex <- oracle(Y = acc.test, experts = X, loss.type = "absolute", model = "convex")
plot(oracle.convex)

# Figura 36
oracle.linear <- oracle(Y = acc.test, experts = X, loss.type = "absolute", model = "linear")
plot(oracle.linear)

# Figura 37
oracle.convex <- oracle(Y = acc.test, experts = X, loss.type = "absolute", model = "convex")
oracle.linear <- oracle(Y = acc.test, experts = X, loss.type = "absolute", model = "linear")
p <- ggplot() +
  geom_line(aes(x = index(acc.test),
                y = coredata(as.zoo(oracle.convex$prediction)), colour = 'Convex')) +
  geom_line(aes(x = index(acc.test),
                y = coredata(as.zoo(oracle.linear$prediction)), colour = 'Linear')) +
  geom_line(aes(x = index(as.zoo(acc.test)), y = coredata(acc.test), colour = 'Test')) +
  scale_color_manual(name = '', values = c('Test' = 'black', 'Convex' = 'red', 'Linear' = 'blue'),
                     labels = c('Convex','Linear', 'Test')) +
  xlab('Año 1974') + ylab('Número de accidentes de tráfico') +
  scale_x_continuous(breaks = c(), labels = c())
p

# Figura 38
mix <- mixture(model = 'EWA', loss.type = 'absolute', coefficients = 'Uniform')
model <- mix
for (i in 1:length(as.vector(acc.test))) {
  model <- predict(model, X[i,], as.vector(acc.test)[i])
}
print(model)
combine(acc.test, model$prediction)

# Figura 39
plot(model, pause = TRUE)



# Cargamos librerias auxiliares
library(ggplot2)
library(zoo)
library(forecast)
library(tseries)

# Cargamos los datos
datos <- read.csv('monthly-traffic-fatalities-in-on.csv')
accidentes <- ts(datos, start = c(1960, 1), frequency = 12)
accidentes.zoo <- zoo(datos, order.by = seq.Date(from = as.Date('1960/01/01'), by = 'month',
                                             length.out = nrow(datos)), frequency = 12)
colnames(accidentes.zoo) <- 'accidentes'
# Función genérica para plotting de test vs pred a través de ggplot2
combine <- function(test, pred) {
  require(ggplot2)
  p <- ggplot() +
    geom_line(aes(x = index(as.zoo(test)), y = coredata(as.zoo(test)), colour = 'Test')) +
    geom_line(aes(x = index(as.zoo(test)), y = pred, colour = 'Prediccion')) +
    scale_color_manual(name = 'Leyenda', values = c('Test' = 'black', 'Prediccion' = 'red'),
                       labels = c('Test','Prediccion'))
  p
}
# Ajustamos estacionalmente la serie y volvemos a dividir en dos conjuntos
decomposition <- decompose(accidentes, type = 'additive')
accidentes.adj <- seasadj(decomposition) # Forecast
acc.train.adj <- window(accidentes.adj, start = c(1960,1), end = c(1973,12))
acc.test.adj <- window(accidentes.adj, start = c(1974,1))
# Plotting con ggplot del training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.adj)), y = coredata(as.zoo(acc.train.adj))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.adj)), y = coredata(as.zoo(acc.test.adj))), col = 'red')
p
# ¿Es estacionaria?
adf.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
adf.test(acc.train.adj, alternative = 'explosive') # No es explosiva
pp.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
pp.test(acc.train.adj, alternative = 'explosive') # No es explosiva
kpss.test(acc.train.adj, null = 'Trend') # 0.055
# Diferenciamos para hacerla estacionaria en tendencia (evidencias visuales)
accidentes.dif.adj <- diff(accidentes.adj) 
acc.train.dif.adj <- window(accidentes.dif.adj, start = c(1960,2), end = c(1973,12)) # Perdemos una observación
acc.test.dif.adj <- window(accidentes.dif.adj, start = c(1974,1))
# Repetimos los tests
adf.test(acc.train.dif.adj, alternative = 'stationary') # Es estacionaria
adf.test(acc.train.dif.adj, alternative = 'explosive') # No es explosiva
pp.test(acc.train.adj, alternative = 'stationary') # Es estacionaria
pp.test(acc.train.adj, alternative = 'explosive') # No es explosiva
kpss.test(acc.train.dif.adj, null = 'Trend') # Es estacionaria en tendencia
# Plotting con ggplot del nuevo training y el test
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)), y = coredata(as.zoo(acc.train.dif.adj))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.test.dif.adj)), y = coredata(as.zoo(acc.test.dif.adj))), col = 'red')
p
# Linealidad en media
terasvirta.test(acc.train.dif.adj) # Es lineal en media
white.test(acc.train.adj) # Es lineal en media
# Calculamos la mayor diferencia en las diferencias
maxdrawdown(acc.train.dif.adj)
acc.train.dif.adj.drop <- window(acc.train.dif.adj, start = c(1967, 9), end = c(1969, 7))
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj.drop)),
                y = coredata(as.zoo(acc.train.dif.adj.drop))), col = 'red')
p
# Bootstrap
boots.block <- tsbootstrap(acc.train.dif.adj, type = 'block')
boots.stationary <- tsbootstrap(acc.train.dif.adj, type = 'stationary')
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(boots.block)), y = coredata(as.zoo(boots.block))), col = 'blue')
p
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(boots.stationary)), y = coredata(as.zoo(boots.stationary))), col = 'blue')
p
initial.acf <- function(serie) {
  return(acf(serie, plot = FALSE)$acf[2:8])
}
tsbootstrap(acc.train.dif.adj, nb= 500, type = 'stationary',
            statistic = initial.acf)
# Surrogate
surr <- surrogate(acc.train.dif.adj, ns = 1, fft = TRUE, amplitude = TRUE)
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(surr)), y = coredata(as.zoo(surr))), col = 'blue')
p
# Estudiamos los correlogramas
autoplot(Acf(acc.train.dif.adj))
autoplot(Pacf(acc.train.dif.adj))
# Ajustamos un ARIMA(1,1,1)
model.1 <- arma(acc.train.dif.adj, order = c(1,1), include.intercept = FALSE) # AIC =  1419.3
summary(model.1)
autoplot(Acf(model.1$residuals))
jarque.bera.test(na.remove(model.1$residuals))
bds.test(na.remove(model.1$residuals))
# Ajustamos un ARIMA(2,1,1)
model.2 <- arma(acc.train.dif.adj, order = c(2,1), include.intercept = FALSE) # AIC = 1414.81
summary(model.2)
autoplot(Acf(model.2$residuals))
jarque.bera.test(na.remove(model.2$residuals))
bds.test(na.remove(model.2$residuals))
# Graficamos ambos modelos
p <- ggplot() +
  geom_line(aes(x = index(as.zoo(acc.train.dif.adj)),
                y = coredata(as.zoo(acc.train.dif.adj))), col = 'black') +
  geom_line(aes(x = index(as.zoo(fitted(model.1))), y = coredata(as.zoo(fitted(model.1)))), col = 'blue') +
  geom_line(aes(x = index(as.zoo(fitted(model.2))), y = coredata(as.zoo(fitted(model.2)))), col = 'red')
p

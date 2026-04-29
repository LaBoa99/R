data("cars")
data <- cars

help(data)
data
summary(data)

plot(data$speed, data$dist)


regresion <- lm(formula = data$dist ~ data$speed, data = data)
abline(regresion)

# r2 ajustada = 0.6438
summary(regresion)

### Calculo 25 mph
mph <- 25
coeficiente_std <- -17.5791
pendiente <- 3.9324

respuesta <- coeficiente_std + ( pendiente * mph)
print(respuesta)

residuos_std <- rstandard(regresion)
residuos_std

anomalias_idx <- which(abs(residuos_std) > 2)
anomalias_idx

cars[anomalias_idx, ]

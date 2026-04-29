data("mtcars")
data <- mtcars

regresion <- lm(formula = data$mpg ~ data$wt + data$hp, data = data)
regresion

summary(regresion)
confint(regresion)
fitted(regresion)
residuals(regresion)

plot(regresion)

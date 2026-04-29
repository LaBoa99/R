data <- read.csv("NobelChocolate.csv")

# equis con guion es la media 
# S_x es la desviacion estandar

mi_escala <- function(datos_columna) {
  m <- mean(datos_columna)
  d <- sd(datos_columna)
  
  col <- c()
  
  for(x in datos_columna){
    r <- (x - m) / d
    col <- c(col, r)
  }
  
  return(col)
}

data$zx <- mi_escala(data$CHOCOLATE)
data$zy <- mi_escala(data$NOBEL)
data$zx_por_zy <- data$zx * data$zy

r <- sum(data$zx_por_zy) / (nrow(data) - 1)
r <- round(r, 3)

b1 <- r * (sd(data$NOBEL) / sd(data$CHOCOLATE))
b1 <- round(b1, 3)

b0 <- mean(data$NOBEL) - (b1 * mean(data$CHOCOLATE))
b0 <- round(b0, 3)

data$y_predic <- b0 + (b1*data$CHOCOLATE)
data$residuos <- data$NOBEL - data$y_predic

plot(data$CHOCOLATE, data$residuos)

s_pre <- sum((data$y_predic - mean(data$NOBEL)) ** 2)
s_y <- sum((data$NOBEL - mean(data$NOBEL)) ** 2)
r2 <- s_pre / s_y
r2

N <- nrow(data) # Total de observaciones
k <- 1          # Solo 'CHOCOLATE'

# Fórmula estándar del R2 ajustado
r22 <- 1 - ((1 - r2) * (N - 1) / (N - k - 1))
r22

regresion <- lm(formula = data$NOBEL ~ data$CHOCOLATE)
regresion
summary(regresion)
confint(regresion)
fitted(regresion)
residuals(regresion)



# APRENDIZAJE EN ARBOL PARTE 1 


library(rpart)
library(rpart.plot)

data("mtcars")
head("mtcars")

mtcars$consumo <- ifelse(mtcars$mpg > 20, "Eficiente", "Gastalon")
mtcars$consumo <- as.factor(mtcars$consumo)
print(mtcars)

set.seed(123)

indices <- sample(1:nrow(mtcars), 0.7 * nrow(mtcars))
train_data <- mtcars[indices, ]
test_data  <- mtcars[-indices, ]

modelo_car <- rpart(
  consumo ~ wt + hp, 
  data = train_data, 
  method = "class"
)

rpart.plot(
  modelo_car, 
  main = "Clasificación de Consumo (mtcars)", 
  type = 2, 
  extra = 104, 
  box.palette = "RdYlGn"
)

predicciones <- predict(modelo_car, test_data, type = "class")
mc <- table(test_data$consumo, predicciones)
print(mc)

clase_objetivo <- 1
TP <- mc[clase_objetivo, clase_objetivo]
FP <- sum(mc[, clase_objetivo]) - TP
FN <- sum(mc[clase_objetivo, ]) - TP
TN <- sum(mc) - (TP + FP + FN)

sensibilidad <- (TP / (TP + FN)) * 100
especificidad <- (TN / (TN + FP)) * 100
exactitud <- (sum(diag(mc)) / sum(mc)) * 100
precision_pos <- (TP / (TP + FP)) * 100
precision_neg <- (TN / (TN + FN)) * 100
precision_gral <- (sum(diag(mc)) / sum(mc)) * 100

mc
data.frame(
  Metrica = c("Sensibilidad", "Especificidad", "Exactitud", "Precision Pos", "Precision Neg", "Precision Gral"),
  Valor = c(sensibilidad, especificidad, exactitud, precision_pos, precision_neg, precision_gral)
)

# Hallazgo / Explicacion
# Como se ve en el dataset de mtcars, se ve claramente que mientras mas pesado el vehiculo
# Este es menos eficiente. debido a que el dataset es de 1981 y las variables que tomamos
# wt ( Peso en libras ) y hp ( Caballos de fuerza) se relacionan a eso.
# Pero eso no excente a corres ligeros pero con un motor muy potente (los deportivos)
# entonces eso hace que nuestro modelo a veces falle y no obtengamos un 100 en exactitud
# aunque es muy bueno detectando negativos afirmado en que puede fallar
# en coches deportivos.

# FIN DE PARTE UNO!

# COMIENZO DE PARTE 2

library(factoextra)

data("mtcars")
head("mtcars")

datos_cars <- mtcars[, c("wt", "hp")]
datos_cars_scaled <- scale(datos_cars)


fviz_nbclust(datos_cars_scaled, kmeans, method = "wss")

set.seed(123)
k <- 5

modelo_kmeans <- kmeans(datos, centers = k, nstart = 25)

fviz_cluster(
  modelo_kmeans,
  data = datos,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
)

# FIN DE PARTE 2 




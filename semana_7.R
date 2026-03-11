library(rpart)
library(rpart.plot)

data("iris")

indices <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[indices, ]
test_data  <- iris[-indices, ]

modelo_tree <- rpart(Species ~ ., data = train_data, method = "class")
predicciones <- predict(modelo_tree, test_data, type = "class")
mc <- table(test_data$Species, predicciones)
mc

clase_objetivo <- 2 
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

library(readr)

entropia <- function(p, n) {
  total <- p + n
  if (p == 0 || n == 0) {
    return(0)  # Nodo puro
  }
  p_prop <- p / total
  n_prop <- n / total
  - (p_prop * log2(p_prop) + n_prop * log2(n_prop))
}


arboles_decision <- read_csv("arboles_decision.csv")
head(arboles_decision)

p <- sum(arboles_decision$Compra == "si") 
n <- sum(arboles_decision$Compra == "no")  

I <- entropia(p, n)

# Ponderada
edades <- unique(arboles_decision$Edad)

total <- nrow(arboles_decision)
I_Edad <- 0

for ( v in edades){
  subnodo <- arboles_decision[arboles_decision$Edad == v, ]
  p <- sum(subnodo$Compra == "si")
  n <- sum(subnodo$Compra == "no")
  
  I_Edad <- I_Edad + ((p+n) / total) * entropia(p, n)
}

print(I_Edad)

# Ganancia
ganancia <- I - I_Edad
print(ganancia)

################
# ARBOLES
################
if(!require(rpart)){
  install.packages("rpart")
} 
if (!require(rpart.plot)){
  install.packages("rpart.plot")
} 

library(rpart)
library(rpart.plot)
library(caret)
library(Momocs)

wine <- read.csv(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", 
  header = FALSE
)

colnames(wine) <- c("Clase", "Alcohol",
                    "Ácido_málico", "Cenizas",
                    "Alcalinidad_de_las_cenizas", "Magnesio",
                    "Fenoles_totales", "Flavanoides",
                    "Fenoles_no_flavonoides", "Proantocianinas",
                    "Intensidad_del_color", "Tonalidad",
                    "OD280_OD315", "Prolina")
wine_prueba <- wine                   
modelo_arbol <- rpart(Clase ~ ., data = wine, method = "class")

summary(modelo_arbol) 
rpart.plot(modelo_arbol, main = "Árbol de Clasificación para el Conjunto de Datos Wine")

prediccion_1 <- predict(modelo_arbol, newdata = wine_prueba, type = "class")
predicciones <- predict(modelo_arbol, wine_prueba, type = "class")

matriz_confusion <- table(Predicted = predicciones, Actual = wine$Clase)
# confusionMatrix(prediccion_1, wine_prueba[["Clase"]])

print(matriz_confusion)

precision <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
print(paste("Precisión del modelo:", round(precision, 4)))


### Accuracy
matriz_confusion <- matrix(
  c(90, 140,   # fila 1: predicho sí → TP y FN
    210, 9560), # fila 2: predicho no → FP y TN
  nrow = 2,
  byrow = TRUE
)

# Nombra filas y columnas para claridad
rownames(matriz_confusion) <- c("Predicho_Si", "Predicho_No")
colnames(matriz_confusion) <- c("Real_Si", "Real_No")


matriz_confusion
accuracy <- sum(diag(matriz_confusion)) / sum(matriz_confusion)
error <- 1 - accuracy
print(accuracy)


### sensibilidad y especifidad
cm <- as.matrix(matriz_confusion)
clases <- rownames(cm)

sensibilidad <- especificidad <- numeric(length(clases))

for (i in seq_along(clases)) {
  TP <- cm[i, i]
  FN <- sum(cm[i, ]) - TP
  FP <- sum(cm[, i]) - TP
  TN <- sum(cm) - TP - FN - FP
  
  sensibilidad[i] <- TP / (TP + FN)
  especificidad[i] <- TN / (TN + FP)
}

# Mostrar resultados
resultados <- data.frame(
  Clase = clases,
  Sensibilidad = round(sensibilidad, 4),
  Especificidad = round(especificidad, 4)
)

print(resultados)

# RESULTADOOOOOOOOOOOOOS
calcular_metricas <- function(M) {
  clases <- rownames(M)
  n <- length(clases)
  
  # Inicializar vectores para las métricas
  TP <- FN <- FP <- TN <- numeric(n)
  sensibilidad <- especificidad <- exactitud <- precision <- precision_neg <- numeric(n)
  
  total <- sum(M)
  suma_TP <- 0
  
  for (i in seq_len(n)) {
    TP[i] <- M[i, i]
    FN[i] <- sum(M[i, ]) - TP[i]
    FP[i] <- sum(M[, i]) - TP[i]
    TN[i] <- total - TP[i] - FN[i] - FP[i]
    
    sensibilidad[i] <- (TP[i] / (TP[i] + FN[i])) * 100
    especificidad[i] <- (TN[i] / (TN[i] + FP[i])) * 100
    exactitud[i] <- ((TP[i] + TN[i]) / total) * 100
    precision[i] <- (TP[i] / (TP[i] + FP[i])) * 100
    precision_neg[i] <- (TN[i] / (TN[i] + FN[i])) * 100
    
    suma_TP <- suma_TP + TP[i]
  }
  
  precision_general <- (suma_TP / total) * 100
  
  # Crear un data frame con resultados
  resultados <- data.frame(
    Clase = clases,
    TP, FN, FP, TN,
    Sensibilidad = round(sensibilidad, 2),
    Especificidad = round(especificidad, 2),
    Exactitud = round(exactitud, 2),
    Precision = round(precision, 2),
    Precision_Negativos = round(precision_neg, 2)
  )
  
  list(
    resultados_clase = resultados,
    Precision_General = round(precision_general, 2)
  )
}

matriz_confusion <- matrix(
  c(68, 0, 1,
    1, 63, 2,
    0, 1, 76),
  nrow = 3, byrow = TRUE)

rownames(matriz_confusion) <- colnames(matriz_confusion) <- c("Maduro", "Pintón", "Verde")

res <- calcular_metricas(matriz_confusion)
print(res$resultados_clase)
cat("Precisión General:", res$Precision_General, "%\n")



data("ChickWeight")
head(ChickWeight)
str(ChickWeight)

datos <- ChickWeight[, c("weight", "Time")]
datos <- scale(datos)

fviz_nbclust(datos, kmeans, method = "wss")

set.seed(123)
k <- 4

modelo_kmeans <- kmeans(datos, centers = k, nstart = 25)

fviz_cluster(
  modelo_kmeans,
  data = datos,
  ellipse.type = "convex",
  palette = "jco",
  ggtheme = theme_minimal()
)

resultado <- as.data.frame(datos)
resultado$cluster <- modelo_kmeans$cluster
resultado

write.csv(resultado, "clusters_ChickWeight.csv", row.names = TRUE)
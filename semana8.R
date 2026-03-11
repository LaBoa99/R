euclides <- function(ti, tj, w){
  sqrt( w * (ti[1] - tj[1])^2 + w * (ti[2] - tj[2])^2)
}

tabla <- data.frame(
  Longitud = c(1, 1, 2, 2, 100, 101, 98, 95, 27, 26),
  Altura = c(2, 1, 1, 2, 74, 75, 76, 78, 29, 28)
)

w = 1
t1 <- c(2, 2)
t2 <- c(100, 74)

print(euclides(t1, t2, w))

# OTRA COSAAAAAAAA
library(factoextra)

data("USArrests")
head(USArrests)
str(USArrests)

datos <- scale(USArrests)

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

write.csv(resultado, "clusters_USArrests.csv", row.names = TRUE)
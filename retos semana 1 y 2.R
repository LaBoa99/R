
# ======================================
# Sección 1
# Archivo: clase_mediana.R
# Fecha: 2026-02-03 22:37:45
# ======================================

x1 <- seq(1, 100, by = 1)
x2 <- x1[1:10]

fnMediana <- function(listOfNumber){
  sort(listOfNumber)
  mid <- floor(length(listOfNumber) / 2)
  
  esPar <- length(listOfNumber) %% 2 == 0
  if(esPar){
    return( (listOfNumber[mid + 1] + listOfNumber[mid]) / 2)
  }
  
  return(listOfNumber[mid])
}

cat(median(x2), fnMediana(x2))

# ======================================
# Sección 2
# Archivo: reto_2.R
# Fecha: 2026-02-03 22:37:45
# ======================================

H <- 15
h <- 5
H:h


# ======================================
# Sección 3
# Archivo: reto_mediana-recordata_7.R
# Fecha: 2026-02-03 22:37:45
# ======================================

x <- seq(1, 20, 0.5)
l <- length(x)

#mediana recortada
fnMedianaRecortada <- function(percent){
  
  toTakeInPercent = percent
  percent <- (toTakeInPercent * l) / 100
  toDiscard <- ceiling(percent)
  cutted = x[toDiscard: l - toDiscard]
  
  return(mean(cutted))
}

cat(fnMedianaRecortada(10))


# ======================================
# Sección 4
# Archivo: reto_media_6.R
# Fecha: 2026-02-03 22:37:45
# ======================================

x = seq(1, 20, by = 0.5)


getMedia <- function(listOfNums) {
  media <- sum(listOfNums) / length(listOfNums)
  return(media)
}

m <- getMedia(x)
cat(m, mean(x))

# ======================================
# Sección 5
# Archivo: reto_TablaDeMultiplciar.R
# Fecha: 2026-02-03 22:37:45
# ======================================

fnMultiplicar <- function(x){
  cat("---------------------------------------\n")
  cat("Tabla de multiplicar:", x, "\n")
  cat("---------------------------------------\n")
  for(i in 1:10){
    mult <- i * x;
    cat("|", x, "*", i, "=", mult, "\n");
  }
}

fnMultiplicarRange <- function(num){
  for(i in 1:num){
    fnMultiplicar(i)
  }
}

num_str <- readline(prompt = "Enter a number: ")
num <- as.numeric(num_str)
fnMultiplicarRange(num)




# ======================================
# Sección 6
# Archivo: reto_1.R
# Fecha: 2026-02-03 22:37:45
# ======================================

n <- 15
n
5 -> n
n
a <- 1
A <- 10
a: A; a; A


# ======================================
# Sección 7
# Archivo: media.R
# Fecha: 2026-02-03 22:37:45
# ======================================

x = seq(1, 20, by = 0.5)


getMedia <- function(listOfNums) {
  media <- sum(listOfNums) / length(listOfNums)
  return(media)
}

m <- getMedia(x)
cat(m, mean(x))

# ======================================
# Sección 8
# Archivo: reto_3.R
# Fecha: 2026-02-03 22:37:45
# ======================================

rm(list = ls())


# ======================================
# Sección 9
# Archivo: ejercicio_3.R
# Fecha: 2026-02-03 22:37:45
# ======================================

n1 <- 10
n2 <- 100
m <- 0.5

M <- data.frame(n1, n2, m)

M

ls()
rm(n1)
ls()

# ======================================
# Sección 10
# Archivo: clase_1.R
# Fecha: 2026-02-03 22:37:45
# ======================================



# ======================================
# Sección 11
# Archivo: reto_4.R
# Fecha: 2026-02-03 22:37:45
# ======================================

data <- read.csv("Analisis.csv")
data$V1
data["V1"]
data["V2"]


# ======================================
# Sección 12
# Archivo: reto_5.R
# Fecha: 2026-02-03 22:37:45
# ======================================

rep("Hi!", 3)


# ======================================
# Sección 13
# Archivo: ejercicio_0.R
# Fecha: 2026-02-03 22:37:45
# ======================================

R.version
installed.packages()
old.packages()
update.packages()
ls()
Vector <- c(4, 5, 6, 8)
length(Vector)
mode(Vector)




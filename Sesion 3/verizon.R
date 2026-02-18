verizon <- read.csv("./AirportDataSpeeds.csv")
verizon <- verizon$VERIZON
cat(verizon, "\n\n")
verizon <- sort(verizon)
cat(verizon, "\n\n")

percentil <- function(p, numbers) {
  return ( sum(numbers < p) / length(numbers) * 100 )
}

revpercentil <- function(k, numbers) {
  
  L <- (k / 100) * length(numbers)
  if(round(L) != L){
    r <- round(L)
    return (verizon[r])
  } 
  
  return((numbers[L] + numbers[L + 1] / 2))
}

a <- percentil(11.8, verizon)
b <- revpercentil(40, verizon)

cat(a, b, sd(verizon), "\n\n")

summary(verizon, "\n\n")


# Quartiles y cosas
x <- (8.4 - 1.5) * (21.4-8.4)
x1 <- (21.4 - 1.5) * (8.4 - 21.4)
cat("QDS", x, x1, "\n")

boxplot(verizon, horizontal = TRUE, col = "blue")

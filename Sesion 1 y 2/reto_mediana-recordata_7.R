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

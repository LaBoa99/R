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
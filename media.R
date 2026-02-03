x = seq(1, 20, by = 0.5)


getMedia <- function(listOfNums) {
  media <- sum(listOfNums) / length(listOfNums)
  return(media)
}

m <- getMedia(x)
cat(m, mean(x))
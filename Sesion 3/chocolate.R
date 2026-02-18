chocolate <- read.csv("NobelChocolate.csv")
chocolate <- chocolate[2:3]

c <- chocolate[1]
nobel <- chocolate[2]

plot(chocolate, col = "red", main = "Chocolate vs Nobel")

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



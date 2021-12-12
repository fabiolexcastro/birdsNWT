

makeSum <- function(rasterStack){
  rasList = lapply(X = meanStack, FUN = function(stack){
    sumRas <- lapply(stack, FUN = function(x){
      message(crayon::blue('Calculating sum'))
      ras <- cellStats(x, sum, na.rm = TRUE) 
    })
    return(sumRas)
  })
  return(rasList)
} 


data <- a
a
flatten(a)
map(data, class)

# Unlist 
data <- flatten(data)
data
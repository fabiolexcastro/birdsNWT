

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
data <- map(data, as.data.frame)
data <- map(data, rownames_to_column)
data <- map(1:length(data), function(k){
  data[[k]] |> 
    setNames(c('name', 'value'))
})

data <- bind_rows(data) |> as_tibble()

data <- data |> 
  mutate(value = value * 6.25) |> 
  separate(data = ., col = name, into = c('mean', 'specie', 'year', 'gcm'), sep = '_')

data <- data |> 
  dplyr::select(-mean)

qs::qsave(x = data, file = './tables/totalAbundance.qs')

# To make the scatterplot -------------------------------------------------

# x <- 2011
# y <- 2091
 
# gcm1 - gcm2 
# gcm1 - gcm3
# gcm2 - gcm3

# A simple scatterplot 
data |> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value)

gsct <- ggplot(data = tble, aes(x = a)) 



















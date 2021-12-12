
# Load libraries
require(pacman)
p_load(raster, rgdal, rgeos, stringr, tidyverse, qs, fs)


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
data <- data |> 
  filter(year %in% c(2011, 2091)) |> 
  spread(year, value) |> 
  setNames(c('specie', 'gcm', 'y2011', 'y2091'))

data <- qs::qread(file = '../qs/totalAbundance1191.qs')


# Functions ---------------------------------------------------------------
make_graph <- function(gcm1, gcm2){
  
  # gcm1 <- 'CanESM2'
  # gcm2 <- 'CCSM4'
  
  cat('Start\n')
  tble <- data |> filter(gcm %in% c(gcm1, gcm2))
  corl <- tble |> 
    group_by(gcm) |> 
    summarise(corr = cor(y2011, y2091, method = 'pearson')) |> 
    ungroup() |> 
    mutate(corr = round(corr, 2))
  
  cat('Making the graph\n')
  gsct <- ggplot(data = tble, 
                 aes(x = y2011, y = y2091, col = gcm)) + 
    geom_point() + 
    scale_color_manual(values = c('#BC679B', '#3E51E3')) + 
    geom_text(aes(x = 40000000, y = 20000000, label = glue('R = {corl[1,2]}')), col = '#BC679B') +
    geom_text(aes(x = 40000000, y = 19000000, label = glue('R = {corl[2,2]}')), col = '#3E51E3') +
    geom_smooth(method = 'lm', se = TRUE) +
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          axis.text.y = element_text(angle = 90, vjust = 0.5,  hjust = 0.5)) + 
    labs(x = 2011, y = 2091, col = 'GCM')
  
  return(gsct)
  
  
}

# Apply the function ------------------------------------------------------
gcms <- unique(data$gcm)
g1g2 <- make_graph(gcm1 = gcms[1], gcm2 = gcms[2])
g1g3 <- make_graph(gcm1 = gcms[1], gcm2 = gcms[3])
g2g3 <- make_graph(gcm1 = gcms[2], gcm2 = gcms[3])

# Join all into only one
gall <- ggpubr::ggarrange(g1g2, g2g3, g2g3, ncol = 1, nrow = 3)
ggsave(plot = gall, filename = '../png/corr_graph.jpg', units = 'in', width = 9, height = 17, dpi = 300)















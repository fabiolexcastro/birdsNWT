

# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst, exactextractr,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
fles <- dir_ls('./qs/zonal', regexp = 'log')

# Function ----------------------------------------------------------------
make_graph <- function(fle){
  
  fle <- fles[1]
  
  cat('Start\n')
  tbl <- qs::qread(file = fle)
  tbl
  
  grp <- ggplot(data = tbl, aes(x = region, y = average, col = model)) + 
    geom_point(size = 3) + 
    coord_flip() +
    theme_ipsum_es() + 
    theme(legend.position = 'bottom') + 
    labs(x = 'EcoRegion', y = 'Average')
  
}



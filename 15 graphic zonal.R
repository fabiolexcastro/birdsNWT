

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
  
  # fle <- fles[1]
  
  cat('Start\n')
  spc <- str_sub(basename(fle), start = 1, end = 4)
  tbl <- qs::qread(file = fle)
  tbl
  
  grp <- ggplot(data = tbl, aes(x = region, y = average, col = model)) + 
    geom_point(size = 3) + 
    coord_flip() +
    ggtitle(label = glue('Zonal average {spc}')) +
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          plot.title = element_text(size = 14)) + 
    labs(x = 'EcoRegion', y = 'Average', col = '')
  
  ggsave(plot = grp,
         filename = glue('Dondeloquierasguardar/graph_{spc}.png'), 
         units = 'in', width = 12, height = 9, dpi = 300)
  
  cat('Done!\n')
  
  
}


# Apply the function ------------------------------------------------------

map(.x = fles, .f = make_graph)



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
root <- './inputs/predictions'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('limiteareadeestudio.shp')
targetCRS <-  paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                    "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
limt <- st_transform(x = limt, targetCRS)

# Function to use ---------------------------------------------------------
make_sum <- function(spc){
  
  spc <- spcs[1] # Run and erase
  
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, directory = '.tif$')
  
  cat('To get the name of each gcm\n')
  gcm <- str_split(basename(fls), '_')
  gcm <- sapply(gcm, function(x) x[1])
  gcm <- unique(gcm)
  
  cat('To apply to each gcm\n')
  system.time(expr = {rsl <- map(.x = 1:length(gcm), function(k){
    
    prd <- str_sub(fls, start = nchar(fls) - 7, end = nchar(fls) - 4)
    prd <- unique(prd)
    fl <- grep(gcm[k], fls, value = TRUE)
    
    rs <- map(.x = 1:length(prd), .f = function(i){
      
      cat(gcm[k], '\n')
      fl <- grep(prd[i], fl, value = TRUE)
      rs <- terra::rast(fl)
      tb <- terra::as.points(rs)
      df <- terra::as.data.frame(x = tb)
      names(df) <- paste0('y', yrs)
      gm <- terra::geom(tb)
      df <- cbind(gm[,3:4], df)
      df <- as_tibble(df)
      
      return(df)
      
      
    })
    
    return(rs)

  })})
  
}



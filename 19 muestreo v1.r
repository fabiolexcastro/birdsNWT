

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
nmrs <- read_csv('')[,1:4] # Leer la tabla de las proporciones de area por pixel 
mask <- raster('') # Leer el raster de los poligonos 
shpf <- st_read('') # Leer el shape de las ecoregiones
shpf$gid <- 1:nrow(shpf)
crs(mask) <- targetCRS
znes <- unique(shpf$gid)

# Function to use ----------------------------
make_sample <- function(zne){
  
  zne <- 1
  cat('Start ', zne, '\n')
  nmr <- filter(nmrs, PolyID == zne)
  pxl <- ceiling(nmr$n)
  lim <- filter(shpf, gid == zne)
  lim <- as(lim, 'Spatial')
  rst <- raster::crop(mask, lim)
  rst <- raster::mask(rst , lim)
  tbl <- rasterToPoints(rst, spatial = FALSE)
  tbl <- slice_sample(tbl, n = pxl)
  head(tbl)
  
  
  cat('Done!\n')
  
  
}







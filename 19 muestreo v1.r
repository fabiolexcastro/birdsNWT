

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
nmrs <- read_csv('') # Leer la tabla de las proporciones de area por pixel 
mask <- raster('') # Leer el raster de los poligonos 
shpf <- shapefile('') # Leer el shape de las ecoregiones

# Function to use ----------------------------








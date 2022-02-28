



# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(dplyr, fs, fst, gdata, glue, quantreg, rasterVis, reproducible,
               stringr, tidyverse, terra, yaImpute )

g <- gc(reset = TRUE)
rm(list = ls())

# Load data ---------------------------------------------------------------
limt <- sf::st_read('inputs/NT1_BCR6/NT1_BCR6_poly.shp') 
ecrg <- sf::st_read('inputs/ecoregions/EcoRegions_NWT_gov/ecoRegionsNT1_BCR6.shp')
cntr <- sf::st_read('inputs/world/all_countries.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

# To make the map ---------------------------------------------------------
limt_geog <- sf::st_transform(x = limt, crs = st_crs(4326))
ecrg_geog <- sf::st_transform(x = ecrg, crs = st_crs(4326))
cntr_geog <- sf::st_transform(x = cntr, crs = st_crs(4326))

gwrl <- ggplot() + 
  geom_sf(data = cntr_geog, fill = NA, col = 'grey50')












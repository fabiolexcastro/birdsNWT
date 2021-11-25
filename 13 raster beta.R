


# Load libraries ----------------------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------
spcs <- dir_ls('./outputs', type = 'directory')
yea1 <- 2011
yea2 <- 2100

# For  ---------------------------------------------------------------------
fles <- map(.x = 1:length(spcs), .f = function(i){
  
  # i <- 1 # Run and erase
  
  cat('Start ', i, '\n')
  fls <- dir_ls(spcs[i], regexp = '.tif$')
  fl1 <- grep(yea1, fls, value = TRUE)
  fl2 <- grep(yea2, fls, value = TRUE)
  fl1 <- as.character(fl1)
  fl2 <- as.character(fl2)
  cat('Done!\n')
  return(list(fl1, fl2))
  
})

library(raster)
library(BAT)

sp1 <- raster::raster(matrix(c(NA,1,1,1,1,0,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))
sp2 <- raster::raster(matrix(c(0,0,0,0,1,1,1,1,1), nrow = 3, ncol = 3, byrow = TRUE))
sp3 <- raster::raster(matrix(c(0,0,0,1,1,1,0,0,0), nrow = 3, ncol = 3, byrow = TRUE))
spp <- raster::stack(sp1, sp2, sp3)
tree <- hclust(dist(c(1:3), method="euclidean"), method="average")
tree$labels = c("Sp1", "Sp2", "Sp3")
names(spp) = tree$labels
rslt <- raster.beta(spp)
raster.beta(spp, tree)

Window()
par(mfrow = c(1, 2))
plot(spp)
plot(rslt)

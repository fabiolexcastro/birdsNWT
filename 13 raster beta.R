


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
gcms <- c('CanESM2', 'CCSM4', 'INM-CM4')

alld <- map(.x = spcs, .f = dir_ls)

# GCMs --------------------------------------------------------------------

allf <- map(.x = 1:length(allf), .f = function(i){as.character(grep(yea1, allf[[i]], value = TRUE))})
cns1 <- map(.x = 1:length(allf), function(k) allf[[k]][1]) %>% unlist()
css1 <- map(.x = 1:length(allf), function(k) allf[[k]][2]) %>% unlist()
inm1 <- map(.x = 1:length(allf), function(k) allf[[k]][3]) %>% unlist()

# Read as a stack ---------------------------------------------------------
cns1 <- raster::stack(cns1)
css1 <- raster::stack(css1)
inm1 <- raster::stack(inm1)

# Bat processing ----------------------------------------------------------
beta.cns1 <- raster.beta(cns1)


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



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, qs, data.table, tidyverse, stringr)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
rstr <- raster::raster('../rstr/rstLCC_MB.tif')
tble <- qs::qread('../qs/pixelCohortData')
spcs <- unique(tble$speciesCode) %>% as.character()
length(spcs)
tbl1 <- dplyr::filter(tble, speciesCode == 'Popu_tre')

# Erase the pixels [Pice_mar] ---------------------------------------------
length(pull(tbl1, pixelIndex))
length(unique(pull(tbl1, pixelIndex)))

rst1 <- rstr
cls1 <- pull(tbl1, pixelIndex)
rst1[cls1] <- NA

plot(rst1)

windows()
par(mfrow = c(1, 2))
plot(rstr, main = 'raw')
plot(rst1, main = 'prc')
par(mfrow = c(1, 1))

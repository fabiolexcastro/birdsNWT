

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
mask <- raster('')
shpf <- st_read('') 
shpf$gid <- 1:nrow(shpf)
crs(mask) <- targetCRS

# Fasterize 
mask <- mask * 0 
fstr <- fasterize::fasterize(shpf, mask, field = 'gid')
znes <- sort(as.numeric(na.omit(unique(fstr[]))))

# Getting the sample n
cntr <- fstr %>% 
  rasterToPoints() %>% 
  as_tibble() %>% 
  setNames(c('x', 'y', 'value')) %>% 
  group_by(value) %>% 
  summarise(count = n()) %>% 
  ungroup() %>%
  mutate(porc = count / sum(count) * 100, 
         n = (count * porc) / 100)
nmrs <- cntr
head(cntr)

# Get the cell IDS for each polygon ---------------------------------------
test <- fstr
test[which(test[] != 1)] <- NA
test
plot(test)

temp <- raster::extract(test, rasterToPoints(test)[,1:2], cellnumbers = TRUE)
sample_n(tbl = as.data.frame(temp), size = 28, replace = FALSE)


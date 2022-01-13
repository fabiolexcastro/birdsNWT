

# Load libraries ----------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, stringr, sf, fasterize, tidyverse, fs, gtools, glue)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------
nmrs <- read_csv('')[,1:4] # Leer la tabla de las proporciones de area por pixel 
mask <- raster('') # Leer el raster de los poligonos 
shpf <- st_read('') # Leer el shape de las ecoregiones
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
  mutate(porc = count / sum(count) * 100)
head(cntr)

# Function to use ----------------------------
make_sample <- function(zne){
  
  cat('Start ', zne, '\n')
  nmr <- filter(nmrs, PolyID == zne)
  pxl <- ceiling(nmr$n)
  lim <- filter(shpf, gid == zne)
  lim <- as(lim, 'Spatial')
  rst <- raster::crop(mask, lim)
  rst <- raster::mask(rst , lim)
  tbl <- rasterToPoints(rst, spatial = FALSE)
  tbl <- as_tibble(tbl)
  
  cat('To replicate 10000 times\n')
  dfm <- map(.x = 1:10000, .f = function(k){
    cat('Replicating :', k, '\n')
    rsl <- slice_sample(tbl, n = pxl)
    colnames(rsl) <- c('x', 'y', 'PolyID')
    rsl <- mutate(rsl, rep = k)
    return(rsl)
  })
  dfm <- bind_rows(dfm)
  cat('To write the result\n')
  out <- glue('./qs/sample')
  dir.create(out) # Run and erase
  qsave(x = dfm, file = glue('{out}/PolyID_{zne}.qs'))
  cat('Done!\n')
  return(dfm)
}

# Apply the function ------------------------------------------------------
smpls <- map(.x = znes, .f = make_sample)

# Read the results --------------------------------------------------------
smpls <- dir_ls('./qs/sample')
smpls
smpls <- map(smpls, qread)

nrw <- map(smpls, nrow) %>% unlist() %>% as.numeric() 
which(nrw == 0)

miss <- parse_number(dir_ls('./qs/sample')[which(nrw == 0)])
# 20, 3, 4
miss <- c(20, 3, 4)

# Check the results -------------------------------------------------------






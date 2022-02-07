# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rcartocolor, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspace, ggspatial, ggpubr, gridExtra, hrbrthemes, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, fs, future.apply, fst, trend, crayon)


g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('./inputs/NT1_BCR6/NT1_BCR6_poly.shp')

targetCRS <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                   "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

limt <- sf::st_transform(x = limt, crs = targetCRS)

# Make   --------------------------------------------------------
make_slopeMaps<- function(spc){
  
  # Proof
  #spc <- spcs[1] # Run and comment (after)
  message(crayon::green("Loading data for: ", spc))
  dir <- grep(spc, dirs, value = TRUE)
  fls <- fs::dir_ls(dir, regexp = '.tif$')
  pvl <- grep('pvle', fls, value = TRUE)
  fls <- grep('slpe', fls, value = TRUE)
  gcm <- str_sub(basename(fls), start = 6, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  
  slopes <- list()
  pvalue <- list()
  stackn <- list()
  for (i in 1:length(fls)){
    slopes[[i]] <- raster(fls[i])
    pvalue[[i]] <- raster(pvl[i])
    stackn[[i]] <- raster::stack(slopes[[i]], pvalue[[i]])
  }
  
  slpe.tble <- map(.x = 1:length(slopes), .f = function(k){
    cat(k, '\n')
    rsl <- stackn[[k]] %>% 
      rasterToPoints(., spatial = FALSE) %>% 
      as_tibble() %>% 
      mutate(model = gcm[k]) %>% 
      setNames(c('x', 'y', 'slp', 'pvl', 'model')) %>% 
      mutate(model = gcm[k])
    return(rsl)
  })
  
  slpe.tble <- bind_rows(slpe.tble)
  ##getting the min and max values for the limits in ggplot filling scale
  min <- min(slpe.tble$slp) 
  max <- max(slpe.tble$slp)
  
  # If the pvalue is bigger than 0.05 change the slope value to NA
  slpe.tble <- mutate(slpe.tble, slp = ifelse(test = pvl > 0.05, NA, slp))
  
  cat('Making the map\n')
  gslp <- ggplot() + 
    geom_tile(data = slpe.tble, aes(x = x, y = y, fill = slp)) + 
    geom_sf(data = limt, fill = NA, col ='#bfbfbf') +
    facet_wrap(.~model, ncol = 3, nrow = 1) +
    scale_fill_gradient2(limits = c(min, max), low = '#D73027', high = '#1A9850', mid = '#f4f4f4', midpoint = 0) +
    theme_bw() +
    coord_sf() +
    ggtitle(label = spc, ) +
    theme(plot.title = element_text(size = 20, face = 'bold'),
          legend.position = 'bottom',
          legend.key.width = unit(3, 'line')) +
    labs(x = 'Longitud', y = 'Latitude', fill = 'Slope') 
  
  ggsave(plot = gslp, 
         filename = glue('./graphs/maps/slopes/{spc}_slpe.png'), 
         units = 'in', width = 13, height = 10, dpi = 700)
  
  message(crayon::green("Done: ", spc))
}

# Apply the function ------------------------------------------------------
map(.x = spcs, .f = make_slopeMaps)

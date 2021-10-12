
# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(raster, rgdal, rgeos, future, furrr, reproducible, RColorBrewer, 
               colorspaces, ggspatial, ggpubr, gridExtra, terra, stringr, glue, 
               sf, tidyverse, RStoolbox, spatialEco, fs, future.apply, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())

install.packages('exactextractr')
library(exactextracr)

# Load data ---------------------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
limt <- sf::st_read('limiteareadeestudio.shp')
ecrg <- sf::st_read('path.shp')

# Extract by mask for the ecoregions ---------------------------------------
plot(st_geometry(ecrg))
ecrg <- sf::st_transform(x = ecrg, crs = st_crs(limt))
ecrg_limt <- sf::st_intersection(x = ecrg, y = limt)
plot(st_geometry(ecrg_limt))

# Function to use ---------------------------------------------------------
see_changes <- function(spc){
  
  spc <- spcs[1]
  
  cat('To start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.fst')
  tbl <- fst::read_fst(path = fle)
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  cat('To see the average in a raster file\n')
  rst.avg <- map(.x = 1:length(gcm), .f = function(k){
    cat('Start -- ', k, '\n')
    rs <- tbl %>% 
      filter(gc == gcm[k]) %>% 
      dplyr::select(lon, lat, avg) %>% 
      rasterFromXYZ()
    return(rs)
  })
  
  cat('To make a simple map\n')
  gavg <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = avg)) + 
    geom_sf(data = limt, fill = NA, col = 'grey') +
    geom_sf(data = ecrg, fill = NA) +
    coord_sf() + 
    facet_wrap(.~gc, nrow = 1, ncol = 3) +
    # scale_fill_gradientn(colors = RColorBrewer::brewer.pal(n = 8, name = 'YlOrBr')) + 
    scale_fill_binned_sequential(palette = 'Heat') +
    theme_bw() + 
    theme(panel.grid.major = element_blank(),
          axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5), 
          legend.position = 'bottom') + 
    labs(x = 'Longitude', y = 'Latitude', fill = 'Mean') 
  
  ggsave(plot = gavg, filename = glue('./graphs/maps/avg_gcm_{spc}.png'), 
         units = 'in', width = 13, height = 8, dpi = 300)
  
  cat('To estimate the change (ration), initial and final year\n')
  tbl <- mutate(tbl, ratio = (y2100 - y2011) / y2011 * 100)
  std <- tbl %>% group_by(gc) %>% summarise(std = sd(avg)) %>% ungroup()
  tbl <- mutate(tbl, rt_bn = ifelse(ratio < 0, 'Negative', ifelse(ratio == 0, 'None', 'Positive')))
  tbl <- mutate(tbl, rt_bn = factor(rt_bn, levels = c('Negative', 'None', 'Positive')))
  tbl %>% group_by(gc, rt_bn) %>% summarise(count = n()) %>% ungroup()
  
  
  cat('To make the map binary\n')
  gbn <- ggplot() + 
    geom_tile(data = tbl, aes(x = lon, y = lat, fill = rt_bn)) + 
    facet_wrap(.~gc, ncol = 3, nrow = 1) + 
    scale_fill_manual(values = c('#A82525', '#6E6E6E', '#0B6121')) + 
    ggtitle(label = spc) +
    theme_ipsum_es() + 
    theme(legend.position = 'bottom', 
          axis.text.y = element_text(angle = 90, vjust = 0.5)) +
    labs(x = 'Longitude', y = 'Latitude', fill = 'Change')
  
  ggsave(plot = gbn, filename = glue('./graphs/maps/bin_gcm_change_{spc}.png'),
         units = 'in', width = 12, height = 9, dpi = 300)
  
  cat('Now to make the zonal statistical\n')
  znl <- map(.x = 1:length(rst.avg), .f = function(k){
    
    cat('To start\n')
    cat(k, '\n')
    znl <- exact_extract(rst.avg[[k]], ecrg_limt, c('mean', 'stdev'))
    znl <- round(znl, digits = 2)
    znl <- mutate(znl, mdl = gcm[k], ecoprovince = ecrg_limt$REGION_NAM)
    cat('Done\n')
    return(znl)
    
  })
  
  znl <- bind_rows(znl) 
  znl <- drop_na(znl)
  
  cat('To make the graph\n')
  gbr <- ggplot(data = znl, aes(x = ecoregion, y = mean)) + 
    geom_errorbar(aes(ymin = mean - stdev, ymax = mean + stdev), width = .2, position = position_dodge(.9)) +
    geom_bar(position = position_dodge(), stat = 'identity') + 
    scale_fill_manual(values = c('#38610B', '#FF8000', '#29088A')) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 6)) +
    facet_wrap(.~mdl, ncol = 1, nrow = 3) +
    theme_bw() +
    theme(legend.position = 'bottom', 
          axis.text.x = element_text(size = 7)) + 
    labs(x = 'Ecoprovince', y = 'Change', fill = 'GCM')
  
  ogb <- glue('./graphs/figs/bar_ratio_{spc}.png')
  ggsave(plot = gbr, filename = ogb, units = 'in', width = 13, height = 6.8, dpi = 300)
  
  cat('Table to raster\n')
  rst <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(k)
    sub <- tbl %>% 
      filter(gc == gcm[k]) %>% 
      dplyr::select(lon, lat, y2011:y2100)
    
    rsr <- map(.x = 3:ncol(sub), .f = function(z){
      sub %>% dplyr::select(1, 2, z) %>% rasterFromXYZ()
    })
    
    rsr <- raster::stack(rsr)
    cat('Done\n')
    return(rsr)
    
  })
  
  cat('To calculate the slopes\n')
  stck <- rst[[1]]
  midb <- ecrg %>% filter(REGION_NAM == 'Mid-Boreal Uplands')
  midb <- as(midb, 'Spatial')
  crs(stck) <- crs(midb)
  stck <- raster::crop(stck, midb)
  stck <- raster::mask(stck, midb)
  slpe <- raster.kendall(x = stck, p.value = TRUE)
  
  
  
  
  
  cat('To calculate the slopes\n')
  tbl <- map(.x = 1:3, function(k){tbl %>% filter(gc == gcm[k]) %>% mutate(gid = 1:nrow(.))}) %>% bind_rows()
  gds <- tbl %>% pull(gid) %>% unique()
  
  cat('To sentence the function\n')
  run_slope <- function(pix){
    
    cat(j, '\n')
    tb <- tbl %>% filter(gid == gds[j])
    
    rs <- map(.x = 1:3, .f = function(g){
      
      cat(gcm[g], '\n')
      df <- tb %>% filter(gc == gcm[g]) 
      ts <- df %>% dplyr::select(contains('y'))
      ts <- ts %>% gather(year, value) %>% mutate(year = parse_number(year))
      tm <- ts %>% pull(value) %>% ts()
      sl <- sens.slope(tm)
      df <- data.frame(gcm = gcm[g], gid = gds[j], slp = sl$estimates, pvl = sl$p.value)
      df <- as_tibble(df)
      
    })
    
    rs <- bind_rows(rs)
    cat('Done\n')
    return(rs)
    
  }
  
  cat('To estimate the slopes\n')
  plan(multicore, workers = 30)
  options(future.globals.maxSize = 3460300800)
  rsl <- future.apply::future_lapply(X = gds, FUN = run_slope)
  # Update
  
 }

# Apply the function ------------------------------------------------------



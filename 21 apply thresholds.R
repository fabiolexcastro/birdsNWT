# Load libraries --------------------------------------------------------
require(pacman)

pacman::p_load(raster, rgdal, rgeos, terra, stringr, glue, sf, tidyverse, RStoolbox, fs, fst, trend)

g <- gc(reset = TRUE)
rm(list = ls())
options(scipen = 999)

# Load data ---------------------------------------------------------------
thrs <- read_csv('./inputs/prevOcc3.csv')
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)
dirs <- glue('{dirs}/occur')
dirs <- as.character(dirs)

# See the changes  --------------------------------------------------------
reclass_ras <- function(spc){
  
  # Proof
  #spc <- spcs[2] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc,dirs, value = TRUE)
  fls <- list.files(dir, pattern = 'occu', full.names = TRUE)
  fls <- grep('occu', fls, value = TRUE)
  yrs <- parse_number(basename(fls))
  yrs <- unique(yrs)
  yrs <- na.omit(yrs)
  gcm <- str_sub(basename(fls), start = 16, end = nchar(basename(fls)) - 4)
  gcm <- unique(gcm)
  thr <- filter(thrs, spec == spc)
  vle <- unique(thr$pOccMean)
  
  cat('Reclassifying\n')
  dfm <- map(.x = 1:length(gcm), .f = function(k){
    
    cat(k, '\n')
    fl <- grep(gcm[k], fls, value = TRUE)
    cat(gcm[k],'\n')
    rs <- terra::rast(fl)
    rs[rs < vle] <- 0
    df <- terra::as.data.frame(x = rs, xy = TRUE, na.rm = TRUE)
    colnames(df) <- c('x', 'y', yrs)
    df <- as_tibble(df)
    df <- mutate(df, gc = gcm[k])
    return(df)
    
  })
  
  rsl <- bind_rows(dfm)
  qs::qsave(x = rsl, file = glue('./outputs/{spc}/occur/occmsk_yrs_{spc}.qs'))
  
  cat('------- Done -------\n')
  return(rsl)
} 
# Apply the function ------------------------------------------------------
rslt <- map(.x = spcs[58:75], .f = reclass_ras)

# Now raster to table -----------------------------------------------------
fles <- glue('./outputs/{spcs}/occur/occmsk_yrs_{spcs}.qs')
fles <- as.character(fles)
test <- qs::qread(fles[1])
unique(test$gc)
dout <- './graphs/figs/occur'

# Function table to raster ------------------------------------------------
tbl2rst <- function(fle){
  
  fle <- fles[1] # Run and erase (after)
  cat('Start ', basename(fle), '\n')
  tbl <- qs::qread(fle)
  spc <- basename(fle) %>% str_split(., '_') %>% sapply(., `[[`, 3) %>% gsub('.qs', '', .) 
  gcm <- unique(tbl$gc)
  
  rsl <- map(gcm, function(i){
    cat(i, '\n')
    tb <- filter(tbl, gc == i)
    colnames(tb) <- c('x', 'y', 'y2011', 'y2031', 'y2051', 'y2071', 'y2091', 'y2100', 'gc')
    tb <- mutate(tb, change = y2091 - y2011, ratio = (y2091/y2011), logRatio = log2(ratio))
    tb <- dplyr::select(tb, -gc)
    tr <- terra::rast(tb[1:9], type = 'xyz')
    do <- glue('{dout}/{spc}')
    ifelse(!dir.exists(do), dir_create(do), print('Fld alrdy exists'))
    terra::writeRaster(x = tr, filename = glue('{do}/occ_change_msk_{spc}_{i}.tif'), overwrite = TRUE)
    cat('Done!\n')
    return(tb[,c(1, 2, 9)])
  })
  rsl <- map(.x = 1:length(gcm), .f = function(i){
    mutate(rsl[[i]], gc = gcm[i])
  })
  rsl <- bind_rows(rsl)
  rsl <- mutate(rsl, class = ifelse(change < 0, 'Loss', ifelse(change == 0, 'No change', 'Gain')))
  rsl <- mutate(rsl, class = factor(class, levels = c('Loss', 'No change', 'Gain')))
  
  cat('To make the map\n')
  ggp <- ggplot() +
    geom_tile(data = rsl, aes(x = x, y = y, fill = class)) + 
    scale_fill_manual(values = c('#2596be', '#b87180', '#c8c8c8')) +
    facet_wrap(.~gc) + 
    labs(x = 'Lon', y = 'Lat', title = glue('Change {spc}'), fill = '') +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          axis.text.y = element_text(angle = 90, hjust = 0.5), 
          plot.title = element_text(size = 16, hjust = 0.5), 
          legend.position = 'bottom', 
          legend.key.width = unit(1.5, 'cm')) +
    coord_sf()
  
  dou <- glue('./graphs/figs/occur/{spc}/map_change_{spc}.png')
  ggsave(plot = ggp, filename = dou, units = 'in', width = 12, height = 9, dpi = 300)
  cat('------ Done ---------!\n')
  
}

##




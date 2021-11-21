
# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, ggpubr, gridExtra, hrbrthemes, colorspace)

rm(list = ls())


# Load data ---------------------------------------------------------------
root <- '../tables/qs'
fles <- dir_ls(root, regexp = '.qs')
fles <- grep('diff', fles, value = TRUE)

gcms <- str_split(fles, pattern = '_')
gcms <- lapply(gcms, `[[`, 3)
gcms <- unique(gcms)
gcms <- gsub('.qs', '', gcms)

# Function ----------------------------------------------------------------
join_tble <- function(gcm){
  
  gcm <- gcms[1]
  
  fle <- grep(gcm, gcm, value = TRUE)
  fle <- mixedsort(fle)
  fle <- as.character(fle)
  tbl <- map(fle, qs::qread)
  all <- tbl %>% reduce(., inner_join, by = c('x', 'y'))
  map(tbl, colnames)
  
}


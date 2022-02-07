# Load libraries --------------------------------------------
library(pacman)

pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler)

rm(list = ls())

# Load data --------------------------------------------------
root <- './outputs'
dirs <- fs::dir_ls(root, type = 'directory')
spcs <- basename(dirs)

change_rasters <- function(spc){
  
  # spc <- spcs[2] # Run and comment (after)
  cat('Start ', spc, '\n')
  dir <- grep(spc, dirs, value = TRUE)
  fle <- fs::dir_ls(dir, regexp = '.qs')
  tbl <- qs::qread(file = glue('./outputs/{spc}/tbl_yrs_{spc}.qs'))
  tbl <- dplyr::select(tbl, x, y, gc, everything())
  names(tbl)[1:2] <- c('lon', 'lat')
  tbl <- mutate(tbl, avg = rowMeans(tbl[,4:9]))
  tbl <- as_tibble(tbl)
  gcm <- unique(tbl$gc)
  
  
  cat('Estimating change from initial (2011) and final (2091) year\n')
  tbl <- mutate(tbl, change = y2091 - y2011 )  #change 2100 for 2091
  tbl <- mutate(tbl, ratio = ((y2091 - y2011) / y2011) * 100) 
  std <- tbl %>% group_by(gc) %>% summarise(std = sd(ratio)) %>% ungroup()
  tbl <- map(.x = 1:3, .f = function(i){
    st <- std %>% filter(gc == gcm[i]) %>% pull(std)
    st <- st / 4
    tb <- tbl %>%
      filter(gc == gcm[i]) %>%
      mutate(rt_bn = case_when(ratio >= st * -1 & ratio <= st ~ 'None',
                               ratio > st ~ 'Positive',
                               ratio < st * -1 ~ 'Negative'))
  })
  tbl <- bind_rows(tbl)
  qs::qsave(x = tbl, file = glue('./qs/{spc}_table_changes.qs'))
}

# Apply the function -----------------------------------------------------
map(spcs, change_rasters)

files <- fs::dir_ls('./qs')
files <- grep('changes', files, value = TRUE)
dat_list <- lapply(files, qs::qread)
saveRDS(dat_list,'./tables/qs/changesTables.RDS')

dat_list <- loadRDS('./tables/qs/changesTables.RDS')
g
a <- dat_list[[1]]
a %>% summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))
a %>% 
  
  sum_table <- function(specie){
    # specie <- spcs[1]
    message(crayon::blue('Starting\n',specie, '\n'))
    table <- qs::qread(file = glue('./qs/{specie}_table_changes.qs'))
    sumDF <- table %>% group_by(gc) %>% summarise(across(y2011:y2100, sum))
    sumDF <- mutate(sumDF, species = specie)
    cat('Done \n')
    return(sumDF)
  }  

sumTable <- map(.x = spcs, .f = sum_table)

totalTable <- bind_rows(sumTable)  ## join all rows into a same table 

diffTable <- totalTable %>% mutate(diff = y2100 - y2011)


long <- totalTable %>%  gather(Year, Value, - c(gc, species)) %>% 
  mutate(Year = as.factor(Year)) %>%  group_by(gc) %>%
  arrange(levels(year), by_group = TRUE)


# create table for plotting rate of change --------------------------------

yearOneVal <- long[1, c('Value')]

long <- long %>% 
  arrange(gc, species, Year)

change <- long %>% mutate(Previous = lag(Value), 
                          Next = lead(Value),
                          Change = Value - Previous,
                          pctRate = (Value/lag(Value) -1) * 100,
                          changeFromYrOne = (Value))

change <- long %>% 
  group_by(gc, species) %>% 
  mutate(Previous = lag(Value), 
         Next = lead(Value),
         Change = Value - Previous,
         pctRate = (Value/lag(Value) -1) * 100,
         changeFromYrOne = (Value)) %>% 
  ungroup()

chn_cns <- change %>% filter(gc == 'CanESM2')
chn_cns_alfl <- chn_cns %>% filter(species == 'ALFL')

lag(ldeaths, 12)

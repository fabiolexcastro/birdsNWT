

# Load libraries --------------------------------------------
library(pacman)
pacman::p_load(glue, raster, rgdal, rgeos, readxl, stringr, sf, R.filesets,
               tidyverse, terra, foreach, fs, future.apply, furrr, fst,
               stringr, glue, compiler, hrbrthemes, gtools, hrbrthemes, colorspace)

rm(list = ls())

# Load data ---------------------------------------------------------------
change <- qs::read('../tables/yr_changeTables.qs')

# Testing -----------------------------------------------------------------
subd <- filter(change, species == sp)
years <- c('2011', '2031', '2051', '2071','2091','2100')

message(crayon::green('Making lolipop plot for:', sp))  

ggplot(data = subd, aes(x = Year, y = pctChange))+ 
  geom_point(size = 8, aes(col = gc, group = gc)) + 
  geom_segment(
    aes(x = Year, y = 0, xend = Year, yend = pctChange, group = gc, 
        colour = gc), size = 1) + 
  geom_hline(yintercept = 0, color = 'black', size = 0.5)+
  scale_y_continuous(labels = scales::percent) +
  facet_wrap(.~gc) +
  theme_bw() +
  ggtitle(label = sp) +
  theme(plot.title = element_text(size = 14, face = 'bold'),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.position = 'none') +
  labs( y = '% change', x = 'Year') +
  scale_x_discrete(labels = years)





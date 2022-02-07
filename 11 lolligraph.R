



# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, gtools, qs, hrbrthemes,)

# Load data ---------------------------------------------------------------
tble <- qs::qread('../qs/yr_changeTable.qs')

# Testing graph -----------------------------------------------------------
subd <- filter(tble, species == 'ALFL')

gtst <- ggplot(data = subd, aes(x = Year, y = pctChange, group = gc)) + 
  geom_point(size = 8, aes(col = gc, group = gc)) + 
  geom_segment(
    aes(x = Year, y = 0, 
        xend = Year, yend = pctChange, 
        group = gc, 
        colour = gc), size = 1) + 
  theme_bw()

ggplot(data = subd, aes(x = Year, y = pctChange,))+ 
  geom_point(size = 8, aes(col = gc, group = gc)) + 
  geom_segment(
    aes(x = Year, y = 0, 
        xend = Year, yend = pctChange, 
        group = gc, 
        colour = gc), size = 1) + 
  facet_wrap(.~gc) +
  theme_bw()

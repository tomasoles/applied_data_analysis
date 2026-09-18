#install.packages("datasauRus")
library("ggplot2")
library("datasauRus")
library("dplyr")

datasaurus_dozen <- datasaurus_dozen


datasaurus_dozen %>%
  group_by(dataset) %>%
  summarise(
    n      = n(),
    mean_x = mean(x),
    sd_x   = sd(x),
    min_x  = min(x),
    max_x  = max(x),
    mean_y = mean(y),
    sd_y   = sd(y),
    min_y  = min(y),
    max_y  = max(y)
  )


ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset))+
  geom_point() +
  theme_void() +
  theme(legend.position = "none")+
  facet_wrap(~dataset, ncol = 3)
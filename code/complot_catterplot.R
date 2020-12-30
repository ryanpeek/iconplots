# Catterplot OPtion
# adapted from Maelle's blog: http://masalmon.eu/2017/02/18/complot/

library("dplyr")
library("forcats")

# Load Data ---------------------------------------------------------------

gestation <- readr::read_csv("data/20180705_gestation_times.csv")%>%
  arrange(gestation) %>%
  mutate(animal = fct_rev(fct_inorder(animal)),
         ggcolors = fct_rev(fct_inorder(mycolors, animal)))
knitr::kable(gestation)

levels(gestation$animal)
levels(gestation$mycolors)

# Setup Plot --------------------------------------------------------------

library("ggplot2")
library("emojifont")
load.emojifont('OpenSansEmoji.ttf')
library("magick")

# test plot
p <- ggplot(gestation) +
  geom_col(aes(x = animal, y = gestation, fill=mycolors))

p <- p +
  scale_fill_manual(values = c("green" = "green",
                               "tan3" = "tan3",
                               "rosybrown4" = "rosybrown4",
                               "black" = "black",
                               "gray80" = "gray80",
                               "orangered" = "orangered",
                               "lightpink" = "lightpink",
                               "white" = "white",
                               "burlywood4" = "burlywood4",
                               "brown" = "brown",
                               "darkblue" = "darkblue",
                               "gray40" = "gray40",
                               "darkgoldenrod1" = "darkgoldenrod1"))


(p <- p + geom_text(aes(x = animal, 
                       y = gestation + 45,
                       label = emoji(label)),
                   family="OpenSansEmoji", size=18) +
    theme(axis.text.y=element_blank(),
               axis.ticks=element_blank(),
               text = element_text(size=20),
               legend.position="none") +
    scale_x_discrete(limits = levels(gestation$animal)) +
    ylim(c(0, max(gestation$gestation) + 50)) +
    coord_flip() + xlab("Animal") +
    ylab("Gestation in days"))

p

# FUNCTION for plots
plot_one_gestation <- function(gestation_time, gestation){
  now_data <- filter(gestation, ~ gestation <= gestation_time)
  p <- ggplot(now_data) 
  p <- p + geom_col(aes(x = animal,
                        y = gestation,
                        fill = mycolors))
  
  p <- p +
    scale_fill_manual(values = c("green" = "green",
                                 "tan3" = "tan3",
                                 "rosybrown4" = "rosybrown4",
                                 "black" = "black",
                                 "gray80" = "gray80",
                                 "orangered" = "orangered",
                                 "lightpink" = "lightpink",
                                 "white" = "white",
                                 "burlywood4" = "burlywood4",
                                 "brown" = "brown",
                                 "darkblue" = "darkblue",
                                 "gray40" = "gray40",
                                 "darkgoldenrod1" = "darkgoldenrod1"))
   
  p <- p + geom_text(aes(x = animal, 
                         y = gestation + 45,
                         label = emoji(label)),
                     family="OpenSansEmoji", size=18) 
  p <- p + theme(axis.text.y=element_blank(),
                 axis.ticks=element_blank(),
                 text = element_text(size=18),
                 legend.position="none")
  p <- p + ggtitle(gestation_time)
  p <- p + scale_x_discrete(limits = levels(gestation$animal))
  p <- p + ylim(c(0, max(gestation$gestation) + 50)) 
  p <- p +  coord_flip() 
  p <- p + xlab("Animal") 
  p <- p + ylab("Gestation in days") 
  
  outfil <- paste0("figs/animals_", gestation_time, ".png")
  ggsave(outfil, p, width=7, height=5, units = "in", scale = 1.4)
  
  outfil
}


# ANIMATE! ----------------------------------------------------------------

library("purrr")
c(unique(gestation$gestation),
  rep(max(gestation$gestation), 3))%>%
  map(plot_one_gestation, gestation = gestation) %>%
  map(image_read) %>%
  image_join() %>%
  image_animate(fps=1) %>%
  image_write("figs/gestation.gif")


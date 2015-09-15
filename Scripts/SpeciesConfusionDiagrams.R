### Species-Confusions

consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv")
confusion <- consdat %>% filter(., Source == "gold", GoldSpp != "impossible")
splist <- unique(consdat$Species)
errors2 <- confusion %>%
  group_by(., GoldSpp, Species) %>%
  summarise(., votes = n()) %>% mutate(., outof = sum(votes), percent = votes/outof)
d3dat <- expand.grid(GoldSpp = splist, Species = splist) %>% 
  left_join(., errors2) 
quartz()
ggplot(data = d3dat, aes(GoldSpp, Species, fill = percent)) + geom_tile() + theme_bw() +
  scale_fill_continuous(na.value = "transparent") +
  theme(axis.ticks = element_blank(), 
       axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, colour = "black"))

#scale_x_discrete(expand = c(0, 0)) + 
#  scale_y_discrete(expand = c(0, 0)) + 
#legend.position = "none", 
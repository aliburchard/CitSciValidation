library(gridExtra)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
overall_accuracy <- read.csv("Data/BootstrappedOverallFromConsensusWithTiesSinglSpp.csv")
by_consensus     <- read.csv("Data/BootstrappedBySppFromConsensusWithTiesSinglSppByConsensus2july.csv")
evenness_images  <- read.csv("Data/evenness_images.csv") %>% select(., -EvenScore)

plot_data <- overall_accuracy %>% mutate(., Data = "All Images") %>% rbind(., evenness_images)
  

overall_plot <- ggplot(data = plot_data, aes(x = n, y = percentCorrect, color = Data, shape = Data)) + 
  geom_point(size = 1.2) +
  geom_smooth(se = F, aes(color = Data, linetype = Data)) + 
  scale_color_grey() +
  theme_bw() + 
  theme(legend.position = c(.8, .25)) +
  xlab("Number Classifiers") +
  ylab("Proportion Correct") +
  geom_text(aes(x=1, y = 1), label = "A")
quartz()
overall_plot

## create three species panels
spl3 <- c( "giraffe", "zebra", "wildebeest") # high accuracy
spl4 <- c("aardwolf", "jackal", "topi") # high false negative
spl1 <-  c("rhinoceros", "koriBustard", "dikDik")# high false positive

sp <- c(spl3, spl4, spl1)
type <- c(rep("High Accuracy", 3), rep("High False Negative", 3), rep("High False Positive", 3))
spdat <- as.data.frame(cbind(sp, type))

SppPlotDat <- by_consensus %>% 
  filter(., AlgoSpp %in% spdat$sp) %>% 
  mutate(., type= spdat$type[match(.$AlgoSpp, spdat$sp)], ConsensusSpecies = factor(AlgoSpp, levels = sp, ordered = T)) %>%
  mutate(., typeID = str_replace_all(type, pattern = " ", replacement = ""))


quartz()
ggplot(data = SppPlotDat, aes(x = n, y = percentCorrect, group = ConsensusSpecies, color = ConsensusSpecies)) + 
  facet_grid(~typeID) + geom_point(size = .5) + geom_smooth(se = T) + 
  ylab("Proportion Correct") + xlab("Number Classifiers") +
  theme_bw() + 
  theme(legend.position = "bottom") + guides(col = guide_legend(title.position = "top"))


xs <- split(SppPlotDat,f = SppPlotDat$typeID)
ha <- guide_legend(title.position = "top", title = "High Accuracy")
fp <- guide_legend(title.position = "top", title = "High False ")
fn <- guide_legend(title.position = "top", title = "High False Negatives")

quartz()
p1 <- ggplot(xs$HighAccuracy, aes(x = n, y = percentCorrect, group = ConsensusSpecies, color = ConsensusSpecies, linetype = ConsensusSpecies, shape = ConsensusSpecies)) + 
  geom_point(size = .7) + geom_smooth(se = T) + scale_colour_grey() + 
  facet_grid(~type) +
  xlab("Number Classifiers") +
  ylab("Proportion Correct") +
  scale_y_continuous(limits = c(0,1.05), breaks = c(0, .25, .5, .75, 1.0)) +
  theme_bw() + 
  theme(legend.position = "bottom") +
  guides(col = ha, linetype = ha, shape = ha) +
  theme(plot.margin = unit(c(1,0,0,0), "lines"))
  
  

p2 <- p1 %+% xs$HighFalseNegative + ylab("") + scale_y_continuous(breaks = NULL) + guides(col = fn, linetype = fn, shape = fn) + ylab(NULL) +
  theme(plot.margin = unit(c(1,0,0,0), "lines"))

p3 <- p1 %+% xs$HighFalsePositive + ylab("") + scale_y_continuous(breaks = NULL) + guides(col = fp, linetype = fp, shape = fp) + ylab(NULL) +
  theme(plot.margin = unit(c(1,.25,0,0), "lines"))

grid.arrange(p1,p2,p3, ncol = 3, nrow = 1, widths = c(.85, .75, .75))




# Move to a new page
grid.newpage()

# Create layout : nrow = 2, ncol = 2
pushViewport(viewport(layout = grid.layout(2, 3)))

# A helper function to define a region on the layout
define_region <- function(row, col){
  viewport(layout.pos.row = row, layout.pos.col = col)
} 

# Arrange the plots
print(overall_plot, vp=define_region(1, 1:3))
print(p1, vp = define_region(2, 1))
print(p2, vp = define_region(2, 2))
print(p3, vp = define_region(2, 3))



############
dgood <- out.raw %>% filter(., Evenness <0.2)
dbad <- out.raw %>% filter(., Evenness >=0.5)

evennessdat <- out.raw %>% mutate(., EvenQuart = ifelse(Evenness <0.25, .25, ifelse(Evenness < .5, .5, ifelse(Evenness < .75, .75, 1))))


ggplot(data = evennessdat, aes(x = n, y = correct)) + geom_smooth(aes(group=EvenQuart))
ggplot(data = dbad, aes(x = as.factor(n), y = correct)) + geom_smooth(aes(group=1))

### given n classifiers, if evenness is < 0.5, what is the likelihood of being correct?
### begin calculating evenness at 3 spp, if it is <0.5, you're good!


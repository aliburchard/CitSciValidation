library(gridExtra)
library(stringr)
library(magrittr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

## ZooCon Figs
accuracy_temp <- read.csv("Data/Accuracy_temp.csv") %>% dplyr::select(., -X)
consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv") # this is limited to just one species
labels <- read.csv("Data/Accuracylabels.csv") %>% distinct(., Species)


## plot params
text_sizing <- theme(axis.text = element_text(size = 18), 
                     axis.title=element_text(size=20), 
                     legend.title = element_text(size=18), 
                     legend.text = element_text(size = 20))
  
## plots

accuracy <- accuracy_temp %>% na.omit %>%
  arrange(.,  FalseNeg,desc(FalsePos)) %>%
  mutate(., order = as.numeric(rownames(.))) %>%
  gather(., key = TypeError, value = accuracy, -Species, -FNSE, -FPSE, -order, -totCons, -totTrue, -totpix, -Group) %>%
  mutate(., SE = ifelse(TypeError == "FalseNeg", FNSE, FPSE), samplesize = ifelse(TypeError == "FalseNeg", totCons, totTrue)) %>%
  select(., Species, TypeError, accuracy, SE, samplesize, order, totpix) %>%
  mutate(., ErrorType = ifelse(TypeError == "FalseNeg", "False Negative (missed when there)", "False Positive (reported when not there)")) %>%
  left_join(., labels) 


quartz()
ggplot(accuracy, aes(y=order, x = accuracy, color = Label)) + xlab("Error") + ylab("")+
  geom_errorbarh(aes(xmin = accuracy - SE, xmax = accuracy + SE)) + geom_point() + theme_bw() +
  facet_grid(~ErrorType) + theme(legend.position = "none", axis.text=element_text(size=12), strip.text.x  = element_text(size = 16)) +
  scale_y_discrete(limits = accuracy$Label[min(accuracy$order):max(accuracy$order)]) 


quartz()
ggplot(filter(accuracy, accuracy >0), aes(x=totpix, y=accuracy)) + 
  geom_smooth(method = "lm", formula = y ~ x, size = 1.5, aes(group = ErrorType, linetype = ErrorType), color = "gray50", se = F) +
  geom_point(aes(colour = ErrorType, shape = ErrorType), size = 3) + 
  scale_x_log10(breaks = c(10, 10, 100, 1000, 10000, 100000), labels = comma, limits= c(10,100000)) + 
  scale_y_log10(breaks = c(.01, .1, 1.0), limits = c(0.01, 1.01)) +
  theme_bw() +
  theme(legend.position = c(.8, .85)) + text_sizing + 
  xlab("Total Pictures") + ylab("Error") 


EvennessData <- consdat %>% filter(., Source %in% "gold") %>%
  mutate(., Match = ifelse(Match == "right", "right", "wrong")) %>%
  mutate(., Match = factor(Match, levels = c("right", "wrong"), ordered = T)) %>% 
  select(., CaptureEventID, Evenness, Match, Source) 

quartz()

plot_boxes <- ggplot(data = EvennessData, aes(x=Match, y=Evenness)) + 
  geom_boxplot() +
  theme_bw() + xlab("Match to Gold Standard") + ylab("Disagreement") +
  text_sizing + theme(plot.margin = unit(c(1, 0, 1, 1), "line")) + 
  coord_cartesian(ylim = c(0,1))
  

plot_distrib <- ggplot(data  = EvennessData, aes(x = Evenness)) + 
  geom_histogram() + 
  coord_flip() +
  scale_x_continuous(expand = c(0, 0)) +
  #expand_limits(x = c(-.001, 1.01)) +
  labs(x = NULL) + theme_bw() + text_sizing +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 0), "line"), 
        axis.text.x = element_text(size = 16))
  
  
grid.arrange(plot_boxes, plot_distrib, ncol = 2, widths = c(3, 1))

## stats for plot:
consdat %>% filter(Evenness <=0.5) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness <=0.25) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())




plot_boxes
plot_distrib
head(EvennessData) 
EvennessData %>% filter(Evenness <=0.5) %>% summarise(., right = sum(ifelse(Match2 == "right", 1, 0)), n())
consdat %>% filter(Evenness >=0.75) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness <=0.25) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())



### Efficiency
overall_accuracy <- read.csv("Data/BootstrappedOverallFromConsensusWithTiesSinglSpp.csv")
evenness_images  <- read.csv("Data/evenness_images.csv") %>% select(., -EvenScore)
plot_data <- overall_accuracy %>% mutate(., Data = "All Images") %>% rbind(., evenness_images)
total_only <- plot_data %>% filter(., Data == "All Images")
all_and_difficult <- plot_data %>% filter(., Data == "All Images" | Data == "Difficult Images")

## plot all 3
overall_plot <- ggplot(data = plot_data, aes(x = n, y = percentCorrect, color = Data, shape = Data, linetype = Data)) + 
  geom_point(data = plot_data, size = 1.2) +
  geom_smooth(data = plot_data, se = F) + 
  scale_y_continuous(limits = c(0.4, 1.0)) +
  theme_bw() +
  theme(legend.position = c(.8, .25), axis.text = element_text(size = 12), axis.title=element_text(size=14), legend.title = element_text(size=14)) +
  xlab("Number Classifiers") +
  ylab("Proportion Correct")

hide_everything <-  theme(axis.line=element_blank(),axis.text.x=element_blank(),
          axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),legend.position="none",
          panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),plot.background=element_blank())

##plot first 2
all_and_difficult_plot <- ggplot(data = plot_data, aes(x = n, y = percentCorrect, color = Data, shape = Data, linetype = Data)) + 
  geom_point(data = all_and_difficult, size = 1.2) +
  geom_smooth(data = all_and_difficult, se = F) + 
  scale_y_continuous(limits = c(0.4, 1.0)) +
  theme(legend.position = c(.8, .25), axis.text = element_text(size = 12), axis.title=element_text(size=14), legend.title = element_text(size=14)) +
  xlab("Number Classifiers") +
  theme_bw() +
  ylab("Proportion Correct")

##plot first 1
all_only <- ggplot(data = plot_data, aes(x = n, y = percentCorrect, color = Data, shape = Data, linetype = Data)) + 
  theme_bw() +
  geom_point(data = total_only, size = 1.2) +
  geom_smooth(data = total_only, se = F) + 
  scale_y_continuous(limits = c(0.4, 1.0)) +
  theme(legend.position = c(.8, .25), axis.text = element_text(size = 12), axis.title=element_text(size=14), legend.title = element_text(size=14)) +  
  xlab("Number Classifiers") +
  ylab("Proportion Correct")


quartz()
overall_plot + hide_everything
all_and_difficult_plot
all_only

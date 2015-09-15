## improve with experience
rawboot <- read.csv("Data/RawBootDatNoExperts.csv")

# which gold to use?
gold <- bigGold
gold %<>% rename(., GoldNumSpp = NumSpecies, GoldSpp = Species, GoldCount = Count)

goldboot <- gold %>% filter(., GoldSpp != "impossible", GoldNumSpp == 1, Source == "gold") %>% data.table
datboot <- left_join(rawboot, goldboot) %>% group_by(., CaptureEventID) %>% mutate(n.class = n_distinct(ClassificationID))

head(datboot)
library(plyr)
#dump any user not logged in
library(stringr)
dat <- datboot %>% 
  mutate(., logged = str_detect(string = datboot$UserID, pattern = "not-logged-in"), 
         match = ifelse(as.character(Species) == as.character(GoldSpp), 1, 0)) %>% 
  filter(., logged==F, GoldSpp != "impossible", GoldNumSpp == 1)

x <- dat %>% group_by(., UserID) %>% summarise(experience = n(), accuracy = sum(match)/n()) %>%
  mutate(., experience = round(experience/10)*10)
plotdat <- x %>% group_by(experience) %>% summarise(accuracy = mean(accuracy)) %>% filter(., experience >0)
quartz()
ggplot(plotdat, aes(x = experience, y = accuracy)) + geom_jitter() + geom_smooth(se = F, formula = y ~ x, method = "lm", color = "black") +
theme_bw() +  geom_line(y = mean(plotdat$accuracy), color = "gray50", linetype=2) + scale_x_log10() + 
  xlab("Number of Photos Classified") + ylab("Average Accuracy")

summary(lm(data = plotdat, accuracy ~ log(experience)))

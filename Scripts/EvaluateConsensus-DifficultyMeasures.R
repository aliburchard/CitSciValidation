library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)
library(dplyr)

consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv") %>% # this is limited to just one species
mutate(., FractionSupport = NumVotes/NumClassifications, FractionBlanks = NumBlanks/NumClassifications)

cons <- read.csv("Data/consensus_data.csv") 
cons %<>% mutate(., DateTime = ymd_hms(.$DateTime), # date time with lubridate
                 DayNight = ifelse(hour(DateTime) %in% 6:20, "day", "night"),
                 FractionSupport = NumVotes/NumClassifications, FractionBlanks = NumBlanks/NumClassifications)

splist <- read.csv("Data/SppGroup.csv") 
splist <- cons %>% group_by(Species) %>% summarise(., totpix = n()) %>%
  left_join(splist, .) %>% mutate(., order = row_number(totpix))


#### EVENNESS IS A HUGE PREDICTOR OF ACCURACY!!!! ####
############ plotting difficulty ########
consdat %<>% mutate(., FracBlanks = NumBlanks/NumClassifications)

# 
# EvennessPlotDat <- consdat  %>% 
#   mutate(Match = ifelse(Match == "right", "correct", ifelse(Match == "wrong", "incorrect", "impossible"))) %>%
#   mutate(Match = factor(Match, levels = c("correct", "incorrect", "impossible"), ordered = T)) %>% 
#   select(., CaptureEventID, Evenness, FractionSupport, FractionBlanks, Match, Source) %>%
#   mutate(., InvertedSupport = 1-FractionSupport) %>%
#   gather(., key = Metric, value = value, -Match, -Source, -CaptureEventID)
# 

EvennessPlotDat <- consdat  %>% 
  mutate(Match = ifelse(Match == "right", "correct", ifelse(Match == "wrong", "incorrect", "impossible"))) %>%
  mutate(Match = factor(Match, levels = c("correct", "incorrect", "impossible"), ordered = T)) %>% 
  mutate(., FractionSupport = 1-FractionSupport) %>%
  select(., CaptureEventID, Evenness, FractionSupport, FractionBlanks, Match, Source) %>%
  gather(., key = Metric, value = value, -Match, -Source, -CaptureEventID) %>%
  mutate(., Metric = ifelse(as.character(Metric) == "FractionSupport", "1 - FractionSupport", as.character(Metric))) %>%
  mutate(., Metric = factor(Metric, levels = c("Evenness", "1 - FractionSupport", "FractionBlanks", ordered = T)))


ann_text <- expand.grid(Match = unique(EvennessPlotDat$Match), Metric = unique(EvennessPlotDat$Metric))
ann_text$text <- c("a", "b", "b", "a", "b", "b", "a", "b", "c")
ann_text$value <- 1.05
## figure 5 ##
quartz()
p <- ggplot(data = EvennessPlotDat, aes(x=Match, y=value)) + 
  geom_boxplot() + 
  facet_wrap(~Metric) +
  theme_bw(base_size = 14) + 
  xlab("Comparison to Expert Classifications") + 
  ylab("Uncertainty")
p + geom_text(data = ann_text, aes(label = text), size = 4)

quartz()
ggplot(data = EvennessPlotDat, aes(x=value)) + geom_histogram() + facet_wrap(~Metric) +
  theme_bw() 


anova(lm(Evenness ~ Match, dat = consdat))
anova(lm(FractionSupport ~ Match, dat = consdat))
anova(lm(FractionBlanks ~ Match, dat = consdat))

TukeyHSD(aov(Evenness ~ Match, dat = consdat))
TukeyHSD(aov(FractionSupport ~ Match, dat = consdat))
TukeyHSD(aov(FractionBlanks ~ Match, dat = consdat))

consdat %>% filter(Evenness >=0.5) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness >=0.75) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness <=0.25) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())

consdat %>% filter(FractionSupport >=0.5) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(FractionSupport >=0.6) %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness >=0.75, Species == "cheetah") %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness >=0.5, Species == "cheetah") %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())
consdat %>% filter(Evenness >=0.75, Species == "wildebeest") %>% summarise(., right = sum(ifelse(Match == "right", 1, 0)), n())


2407/2409



########### EXPLORING CONSENSUS DATA: EVENNESS VS SPP CHARACTERISTICS ######

#### In addition to the gold-standard data, we have evenness scores for ALL (so use cons, not consdat)
#for each species, mean and mediean Evenness, FracSupport, and FracBlanks

means <- cons %>% group_by(., Species) %>% select(., Species, FractionSupport, FractionBlanks, Evenness) %>%
  summarise_each(funs(mean)) %>% gather(., key = Measure, value = value, -Species)
means$order <- splist$order[match(summaries$Species, splist$Species)]
means$totpix <- splist$totpix[match(summaries$Species, splist$Species)]

medians %>% filter(Measure == "FractionSupport") %>% lm(value ~ log(totpix), data = .) %>% summary
medians %>% filter(Measure == "FractionBlanks") %>% lm(value ~ log(totpix), data = .) %>% summary
medians %>% filter(Measure == "Evenness") %>% lm(value ~ log(totpix), data = .) %>% summary

means <- cons %>% group_by(., Species) %>% select(., Species, FractionSupport, FractionBlanks, Evenness) %>%
  summarise_each(funs(mean)) %>% gather(., key = Measure, value = value, -Species)
means$order <- splist$order[match(means$Species, splist$Species)]
means$totpix <- splist$totpix[match(means$Species, splist$Species)]
means$Measure <- factor(means$Measure, levels = c("Evenness", "FractionSupport", "FractionBlanks"), ordered = T)

means %>% filter(Measure == "FractionSupport") %>% lm(value ~ log(totpix), data = .) %>% summary
means %>% filter(Measure == "FractionBlanks") %>% lm(value ~ log(totpix), data = .) %>% summary
means %>% filter(Measure == "Evenness") %>% lm(value ~ log(totpix), data = .) %>% summary

# FractionSupport (p < 0.0001, r2 = .397, df = 46)
# Evenness (p <0.001, R2 = .214, df = 46)
# FractionBlanks (p = 0.0036, R2 = 0.170, df = 46)


quartz()
ggplot(data = means, aes(x = totpix, y = value)) + 
  facet_wrap(~Measure, scale = "free_y") + theme_bw() + scale_x_log10(labels = comma, breaks = c(10, 100, 1000, 10000, 100000)) +
  geom_point() + xlab("Total captures") +
  stat_smooth(method = "lm", formula = y ~ x, aes(group = 1), color = "dark gray", se = F)
  


ggplot(data = cons, aes(y=Evenness, x=as.factor(totpix))) + geom_boxplot() + # Lots of species-specific variation
 theme(axis.text.x = element_text(angle = 45)) + stat_smooth(aes(group = 1)) + xlab("Species in order of total pictures")

quartz()
ggplot(data = cons, aes(x = totpix, y = Evenness, color = Species)) + 
  geom_jitter(alpha = 0.3, size = 0.6, position = position_jitter(width = .07, height = 0.07)) + 
  scale_x_log10() +
  theme_bw() + stat_smooth(aes(group = 1)) + xlab("total pictures") + theme(legend.position = "none") 
ggplot(data = cons, aes(x = Species, y = Evenness)) + geom_boxplot()

library(nlme)
summary(lme(Evenness ~ log(totpix), random = ~1|Species, data = cons))

consdat %>% filter(Evenness == 0 & Match == "impossible")
cons %>% filter(., CaptureEventID %in% c("ASG000f78l", "ASG000fhkz"))


#### calculate evenness with blanks ####
rawingold <- raw %>% filter(., CaptureEventID %in% consdat$CaptureEventID)
head(rawingold)

r <- SingleSppEvenness(rawingold) %>% rename(., EvenWith0 = Evenness)
head(r)
testeven <- left_join(consdat, r)
head(testeven)

ggplot(testeven, aes(x = Match, y = EvenWith0)) + geom_boxplot()

## evenness scores are overall higher, but NOTHING that is wrong has an evenness score < 0.5
consdat %>% dim
consdat %>% filter(., FracSupport > .25, Evenness < .8) %>%  summarise(., sum(Match == "right"), n())
consdat %>% filter(., Evenness < .8, FracSupport) %>% summarise(., sum(Match == "right"), n())

head(cons)
difficulty <- cons %>% mutate(., FractionBlanks = NumBlanks/NumClassifications, FractionSupport = NumVotes/NumClassifications) %>% 
  select(CaptureEventID, Evenness, FractionSupport, FractionBlanks) %>% gather(data = ., key = Measure, value = Score, -CaptureEventID)

ggplot(difficulty, aes(x = Score)) + geom_histogram() + facet_grid(~Measure) + theme_bw() + xlab("")

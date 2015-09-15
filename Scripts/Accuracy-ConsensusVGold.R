library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)
library(grid)
library(scales)

consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv") # this is limited to just one species
cons <- read.csv("Data/consensus_data.csv")



######################################################################################################
######################            EXPLORING THE DATA             ############################
######################################################################################################

### GOLD STANDARD VS. OVERALL
### species comparison: Grants gazelles seem WAY underrepresented in our gold standard data, otherwise, pretty standard.
quartz()
ggplot(data=gold, aes(x=GoldSpp)) + geom_bar() + angle #how many gold-standard classifications per spp?
quartz()
ggplot(data=cons, aes(x=Species)) + geom_bar() + angle #how many gold-standard classifications per spp?

##### Comparing Consensus to Gold Standard Classifications ####
# how many SPECIES CLASSIFICATIONS were not resolved by experts?
impossible <- consdat %>% filter(., Source == "gold", GoldNumSpp == 1, GoldSpp == "impossible") %>%
  summarise(., tot = n())
overalldat <- consdat %>% filter(., Source == "gold", GoldNumSpp == 1, GoldSpp != "impossible") %>%
  summarise(., right = sum(ifelse(Match == "right", 1,0)), tot = n())
overalldat
# overall resolved


##### Evaluating Accuracy: Consensus vs. Gold Standard



########### SPECIES SPECIFIC ACCURACY ############
splist <- read.csv("Data/SppGroup.csv") 
splist <- cons %>% group_by(Species) %>% summarise(., totpix = n()) %>%
  left_join(splist, .)



### species-specific accuracy: is the consensus answer correct? ###
species_specific <- consdat %>% 
  group_by(., Species) %>% # given consensus bit
  summarise(., correct = sum(ifelse(Match == "right", 1, 0)), tot = n(), accuracy = correct/tot, SE = SEp(accuracy, tot)) %>% 
  arrange(., desc(accuracy)) 

### CGC: correct GIVEN that's what consensus says
angle <- theme(axis.text.x = element_text(size = rel(1.5), angle = 90))

SEp <- function(p, n){ sqrt(p*(1 - p)/n)}
CgivenCons <- consdat %>% group_by(., Species) %>% 
  summarise(., correct = sum(ifelse(Match == "right", 1, 0)), tot = n(), accuracy = correct/tot, SE = SEp(accuracy, tot)) %>% 
  arrange(., desc(accuracy)) 
CGC <- CgivenCons %>% mutate(., FalsePos = 1-accuracy, FPSE = SE) %>% select(Species, FalsePos, FPSE, totCons=tot) 

FalsePositives <- ggplot(data = CGC, aes(x=Species, y = FalsePos, color = Species)) + 
  geom_point() + geom_errorbar(aes(ymin = FalsePos-FPSE, ymax = FalsePos+FPSE)) + 
  angle + ylab(label = "False Positives") + theme(legend.position = "none")


### CGT: correct GIVEN that's what it really is
CgivenTrue <- consdat %>% filter(., Source == "gold") %>% group_by(., GoldSpp) %>% 
  summarise(., correct = sum(ifelse(Match == "right", 1, 0)), tot = n(), accuracy = correct/tot, SE = SEp(accuracy, tot)) %>% 
  arrange(., desc(accuracy)) 

CGT <- CgivenTrue %>% filter(., GoldSpp != "impossible", GoldSpp != "rodent" ) %>% 
  mutate(., Species = GoldSpp, FalseNeg = 1-accuracy, FNSE = SE) %>% select(Species, FalseNeg, FNSE, totTrue = tot) 

FalseNegatives <- ggplot(data = CGT, aes(x=Species, y = FalseNeg, color = Species)) + 
  geom_point() + geom_errorbar(aes(ymin = FalseNeg-FNSE, ymax = FalseNeg+FNSE)) + 
  angle + ylab(label = "False Negatives") + theme(legend.position = "none") 

quartz(); print(FalsePositives)
quartz(); print(FalseNegatives)

CGT <- left_join(CGT, splist)
CGC <- left_join(CGC, splist)
easy_only <- left_join(CGT, CGC) %>% filter(., FalseNeg == 0, FalsePos == 0)

labels <- read.csv("Data/Accuracylabels.csv") %>% distinct(., Species)

accuracy_1 <- filter(accuracy, accuracy > 0)
starlist <- filter(accuracy, accuracy == 0) %>% select(., Species) %>% distinct() %>% filter(., !(Species %in% accuracy_1$Species))
pluslist <- filter(accuracy, accuracy == 0) %>% select(., Species) %>% distinct() %>% filter(., (Species %in% accuracy_1$Species))


accuracy_temp <- right_join(CGT,CGC) 
#write.csv(accuracy_temp, "Data/Accuracy_temp.csv")

  accuracy <- accuracy_temp %>%
  mutate(., nasort = ifelse(is.na(FalseNeg), -1, 1)) %>%
  arrange(.,  nasort, FalseNeg,desc(FalsePos)) %>%
  mutate(., order = as.numeric(rownames(.))) %>%
  gather(., key = TypeError, value = accuracy, -Species, -FNSE, -FPSE, -order, -totCons, -totTrue, -nasort, -totpix, -Group) %>%
  mutate(., SE = ifelse(TypeError == "FalseNeg", FNSE, FPSE), samplesize = ifelse(TypeError == "FalseNeg", totCons, totTrue)) %>%
  select(., Species, TypeError, accuracy, SE, samplesize, order, totpix) %>%
  mutate(., ErrorType = ifelse(TypeError == "FalseNeg", "False Negative", "False Positive")) %>%
  left_join(., labels) 

accuracy %<>% mutate(., star = ifelse(Species %in% starlist$Species, 1, 0), 
                     plus = ifelse(Species %in% pluslist$Species, 1, 0)) %>% 
  mutate(., Label2 = ifelse(star == 1, paste(Label, "(*)", sep = ""), 
                            ifelse(plus == 1, paste(Label, "(+)", sep = ""), as.character(Label))))


write.csv(accuracy, "Data/accuracy_dat.csv", row.names = F)

quartz()
ggplot(accuracy, aes(y=order, x = accuracy)) + xlab("Fraction of Error") + ylab("")+
  geom_errorbarh(aes(xmin = accuracy - SE, xmax = accuracy + SE)) + geom_point() + 
  facet_grid(~ErrorType) + theme_bw() + theme(legend.position = "none") +
  scale_y_discrete(limits = accuracy$Label2[min(accuracy$order):max(accuracy$order)]) + scale_x_sqrt()


ggplot(CGT, aes(y = FalseNeg, x = totpix)) + geom_point() + scale_x_log10() + stat_smooth()
summary(lm(data = CGT, FalseNeg ~ log(totpix)))
cor(x= CGT$totpix, y = CGT$FalseNeg, method = "spearman", )

# Table S1
# total sample size for baseline, extended, False Positive and False Negative errors. 
# "accuracy" is really 1-False Positives


## figure 4
quartz()
library(scales)
library(grid)


accuracy_dat <- accuracy %>% 
  mutate(., regression = ifelse(accuracy == 0, "No", "Yes"), modified = ifelse(accuracy == 0, accuracy + .001, accuracy))


quartz()
ggplot(accuracy_1, aes(x=totpix, y=accuracy)) + 
  #geom_point(data = accuracy_0, color = "black", aes(shape = ErrorType)) +
  geom_point(aes(colour = ErrorType, shape = ErrorType)) + 
  geom_smooth(data = accuracy_1, method = "lm", formula = y ~ x, aes(group = ErrorType, linetype = ErrorType), color = "gray50", se = F) +
  theme_bw() +
  scale_color_grey() +
  #facet_grid(~ErrorType) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000), labels = comma, limits= c(10,100000)) + 
  scale_y_log10(breaks = c(.01, .1, 1.0), limits = c(.01, 1.01)) +
  #scale_x_sqrt() + scale_y_sqrt() +
  theme(legend.position = c(.8, .85)) + 
  xlab("Total Captures") + ylab("Fraction of Error") +
  theme(panel.margin = unit(1, "lines"))



summary(lm(data = filter(CGC, FalsePos != 0), log(FalsePos) ~ log10(totpix)))
summary(lm(data = filter(CGT, FalseNeg != 0),  FalseNeg ~ log10(totpix)))

cor(x = log10(CGC$totpix), CGC$FalsePos, method = "pearson")
cor(x = log10(CGT$totpix), CGT$FalseNeg, method = "pearson")

summary(consdat)
# ggplot(CGC, aes(y = FalsePos, x = totpix, color = Species)) + geom_point() + theme(legend.position = "none") +
#   ylab("Fraction False Positives") + xlab("Total Captures (commonness)") +
#   scale_x_log10() +  stat_smooth(aes(group=1))
# 
# 
# ggplot(CGT, aes(y = FalseNeg, x = totpix, color = Species)) + geom_point() + theme(legend.position = "none") +
#   ylab("Fraction False Negatives") + xlab("Total Captures (commonness)") +
#   scale_x_log10() +  stat_smooth(aes(group=1))
#  



######### Species-by-Species assesment ###########
###### Plotting species confusions ######

goldonly <- consdat %>% filter(., Source == "gold") %>% group_by(Species) %>% tally %>% rename(GoldPix = n)
extended <- consdat %>% group_by(Species) %>% tally %>% rename(extended = n)


tabledat <- right_join(x = CGT, y=CGC) %>%
  left_join(., splist) %>%
  left_join(., goldonly) %>%
  left_join(extended) %>%
  mutate(., Accuracy = 1-FalsePos) %>%
  select(., Species, totpix, GoldPix, extended, Accuracy, FalseNeg, FalsePos)

write.csv(tabledat, "Data/TableS1.csv")

plotdat <- right_join(x = CGT, y=CGC)

# ggplot(data = plotdat, aes(x = CGT, y = CGC, color = Species, linetype = Species)) + 
#   geom_point() + xlab("Prob reported given really there") + ylab("Prob really there given reported") +
#   theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 6))
quartz()
ggplot(data = plotdat, aes(x = FalseNeg, y = FalsePos, color = Species)) + 
  xlab("False Negatives") + ylab("False Positives") +
  geom_errorbar(aes(ymin = FalsePos-FPSE, ymax = FalsePos+FPSE), linetype = 2) + 
  geom_errorbarh(aes(xmin = FalseNeg-FNSE, xmax = FalseNeg+FNSE), linetype = 2) +
  geom_jitter(position = position_jitter(width = .001, height = .001)) + 
  theme(legend.position = "bottom") + guides(colour = guide_legend(nrow = 6))



#### who is getting confused with what? ## heat map ###!!
library(circlize)

group1 <- filter(splist, Group == 1, Species != "lionMale", Species != "lionFemale")

group1 <- c("aardwolf", "jackal", "aardvark", "batEaredFox")
group2 <- c("civet", "genet", "serval", "leopard", "cheetah", "hyenaSpotted")
group3 <- c("gazelleGrants", "gazelleThomsons", "eland", "hartebeest", "buffalo", "topi", "wildebeest", "impala")

createChord <- function(group, data = consdat) {
  errors <- data %>% filter(., GoldSpp %in% group, Species %in% group) %>% 
    group_by(., GoldSpp, Species) %>%
    summarise(., votes = n()) %>% mutate(., outof = sum(votes), percent = votes/outof) %>%
    select(GoldSpp, Species, percent)

  #ggplot(errors, aes(y = GoldSpp, x = Species, fill = percent)) + geom_tile() + angle
  d3dat <- expand.grid(GoldSpp = group, Species = group) %>% distinct %>%
    left_join(., errors) %>% 
  spread(., key = Species, value = percent) # need to ensure that this creates all
  row.names(d3dat) <- d3dat$GoldSpp
  d3dat[is.na(d3dat)] <- 0
  d3dat %<>% select(., -GoldSpp) %>% as.matrix(.) 
  
  return(d3dat)
  
}

chordat <- createChord(group1, consdat) 
quartz()
library(circlize)
chordDiagram(chordat, directional = F)



write.csv(d3dat, "Data/d3fullmatrix.csv", row.names =F)

errors2 <- consdat %>% filter(., GoldSpp %in% group1, Match == "wrong") %>% 
  group_by(.,   GoldSpp, Species) %>%
  summarise(., votes = n()) %>% mutate(., outof = sum(votes), percent = votes/outof) %>%
  select(GoldSpp, Species, percent)
d3dat <- expand.grid(GoldSpp = splist$Species, Species = splist$Species) %>% 
  left_join(., errors2) %>% 
  spread(., key = Species, value = percent) %>% select(., -GoldSpp)


write.csv(d3dat, "Data/d3fullmatrixNoSelf.csv", row.names =F)

unique(d3dat$GoldSpp)
View(errors)
write.csv(errors, "Data/SppErrors.csv")



View(accuracy_dat)
accuracy_dat %>% mutate(regression = ifelse(regression == "yes"))
summary(glm(formula = as.factor(regression) ~ totpix, data = accuracy_dat, family = "binomial"))

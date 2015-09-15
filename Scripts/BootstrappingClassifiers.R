library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)

rm(list = ls())
##### calculate consensus for bootstrapping
## consensus function:
# test <- rawboot %>% filter(., CaptureEventID == "ASG000ffl7" | CaptureEventID == "ASG000b3xp" | CaptureEventID == "ASG000b48a") %>%
#   data.table
# x <- test %>% group_by(., CaptureEventID) %>% 
#   sample_n(., size = 4, replace = T) %>%
#   group_by(.) 
# #test <- raw %>% group_by(CaptureEventID) %>% sample_n(., 5)

### have all raw classifications
# 1. combine answers when single classifer reports same species as 2 classifications
  # for any subject, for any  classification ID & same species 
  # then for every classification id, count the number of different species report
  # across those classification IDs for a given subject, what is the median number of spp reported
  # that is N species for that subject
  # across all classification IDs for a subject, take top N species with most votes
  # calculate certainty (fraction of votes)
  ##

SingleSppCons <- function(dt) {
  # need to filter down to one species images
  u <- dt %>%  group_by(., CaptureEventID, Species) %>% 
    summarise(., votes = n()) %>%  #calculate votes
    top_n(., n = 1, wt=votes) %>% # take the spp with most votes
    mutate(., AlgoSpp = Species) %>% 
    select(., -Species)
  
  # check for ties
  u %<>% group_by(., CaptureEventID) %>%
    mutate(., resolve = ifelse(n() == 1, "ok", "tie")) %>% #if more than one answer per cap ID, it's a tie
    sample_n(., size = 1, replace = F) # randomly sample ties - select one answer only
  return(u)
}
 
SingleSppEvenness <- function(dt){
  u <- dt %>% group_by(., CaptureEventID, Species) %>% 
    filter(., Species != "blank") %>%
    summarise(., v = n()) %>% 
    mutate(., totv = sum(v)) %>%
    mutate(.,prop = v/totv) %>%
    mutate(., p = log(prop)*prop) %>%
    group_by(., CaptureEventID) %>% summarise(sumv = sum(p), s=n()) %>%
    mutate(., Evenness = -sumv/log(s)) %>%
    mutate(., Evenness=ifelse(s == 1, 0, Evenness)) %>%
    select(., CaptureEventID, Evenness)
  
  return(u)
}

## how many classifiers does it take to get a good answer?

#gold <- read.csv("Data/gold_standard_data.csv")
bigGold <- read.csv("Data//BigGold.csv")
cons <- read.csv("Data/consensus_data.csv")
raw <- fread("Data/raw_data.csv", sep = ",", header =T)
# rawexpert <- read.csv("Data/expert-classifications-raw.csv")
# rawboot <- filter(raw, CaptureEventID %in% goldboot$CaptureEventID, (!ClassificationID %in% rawexpert$ClassificationID)) %>%
#   select(., CaptureEventID, ClassificationID, UserID, Species, Count)  
# write.csv(rawboot, "Data/RawBootDatNoExperts.csv")
rawboot <- read.csv("Data/RawBootDatNoExperts.csv")

# which gold to use?
gold <- bigGold
gold %<>% rename(., GoldNumSpp = NumSpecies, GoldSpp = Species, GoldCount = Count)


goldboot <- gold %>% filter(., GoldSpp != "impossible", GoldNumSpp == 1) %>% data.table
datboot <- left_join(rawboot, goldboot) %>% group_by(., CaptureEventID) %>% mutate(n.class = n_distinct(ClassificationID))


## # classifiers vs. % accuracy

overall.l <- list()
accuracyByCons.l <- list()
accuracyByGold.l <- list()
rawsample.l <- list()
dropdat <- list()

for(n in 1:30){
  
  overall <- list()
  accuracyByCons <- list()
  accuracyByGold <- list()  
  rawsample <- list()
  
  for(i in 1:20){ # currently only works for one species
    x <- datboot %>% filter(., Source == "gold") %>% 
      group_by(., CaptureEventID) %>% 
      sample_n(., size = n, replace = T) %>%
      group_by(.) ## randomly samply n classificationIDs
  
    evenness <- SingleSppEvenness(x)
    consensus <- SingleSppCons(x) %>%  #calculate consensus species
      left_join(., goldboot) %>% # currently includes ties. possibly sample 1 from each? 
      mutate(., correct = ifelse(as.character(AlgoSpp) == as.character(GoldSpp), 1, 0)) %>%
      left_join(., evenness) %>% data.frame 

    
    ### ACROSS ITERATIONS, CALCULATE PERCENT CORRECT
    ## output datasets of overall and species-specific accuracies vs. # classifiers
    rawsample[[i]] <- consensus %>% mutate(., iter = i, n = n)
    overall[[i]] <- consensus %>% filter(., Source == "gold") %>% group_by(.) %>% 
      summarise(., good = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
    accuracyByGold[[i]] <- consensus %>% group_by(., GoldSpp) %>% 
      summarise(., good  = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
    accuracyByCons[[i]] <- consensus %>% group_by(., AlgoSpp) %>% 
      summarise(., good  = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
  }
  
  overall.l[[n]] <- do.call(rbind, overall)
  accuracyByGold.l[[n]] <- do.call(rbind, accuracyByGold)
  accuracyByCons.l[[n]] <- do.call(rbind, accuracyByCons)
  rawsample.l[[n]] <- do.call(rbind, rawsample)
  print(n)
}

out.overall <- do.call(rbind, overall.l)
out.Gold <- do.call(rbind, accuracyByGold.l)
out.Cons <- do.call(rbind, accuracyByCons.l)
out.raw <- do.call(rbind, rawsample.l) %>% data.table

 
#write.csv(out.overall, "Data/BootstrappedOverallFromConsensusWithTiesSinglSpp.csv", row.names = F)
#write.csv(out.Cons, "Data/BootstrappedBySppFromConsensusWithTiesSinglSppByConsensus2july.csv", row.names = F)
#write.csv(out.accuracyByGold, "Data/BootstrappedBySppFromConsensusWithTiesSinglSppByGold.csv", row.names = F)
#write.csv(out.raw, "Data/RawBootstrappedDataWithTiesSinglSpp.csv", row.names = F)
# rm(out.raw, rawsample, rawsample.l)

out.overall <- read.csv("Data/BootstrappedOverallFromConsensusWithTiesSinglSpp.csv")


quartz()
ggplot(data = out.overall, aes(x = n, y = percentCorrect)) + geom_point() + theme_bw() + xlab("Number Classifiers") +
  ylim(c(0,1)) + ylab("Proportion Correct") + geom_smooth(se = F, color = "gray50")


spl3 <- c( "giraffe", "zebra", "wildebeest") # high accuracy
spl4 <- c("aardwolf", "jackal", "topi") # high false negative
spl1 <-  c("rhinoceros", "koriBustard", "dikDik")# high false positive

sp <- c(spl3, spl4, spl1)
type <- c(rep("High Accuracy", 3), rep("High False Negative", 3), rep("High False Positive", 3))
spdat <- as.data.frame(cbind(sp, type))

SppPlotDat <- out.Cons %>% filter(., AlgoSpp %in% spdat$sp) %>% 
  mutate(., type= spdat$type[match(.$AlgoSpp, spdat$sp)], ConsensusSpecies = factor(AlgoSpp, levels = sp, ordered = T))


quartz()
ggplot(data = filter(SppPlotDat), aes(x = n, y = percentCorrect, group = ConsensusSpecies, color = ConsensusSpecies)) + 
  facet_grid(~type) + geom_point(size = .5) + geom_smooth(se = T) + ylab("P(True|Consensus)") + xlab("Number Classifiers") +
  theme_bw() + theme(legend.position = "bottom") + guides(col = guide_legend(title.position = "top"))



############
dgood <- out.raw %>% filter(., Evenness <0.2)
dbad <- out.raw %>% filter(., Evenness >=0.5)

evennessdat <- out.raw %>% mutate(., EvenQuart = ifelse(Evenness <0.25, .25, ifelse(Evenness < .5, .5, ifelse(Evenness < .75, .75, 1))))


ggplot(data = evennessdat, aes(x = n, y = correct)) + geom_smooth(aes(group=EvenQuart))
ggplot(data = dbad, aes(x = as.factor(n), y = correct)) + geom_smooth(aes(group=1))

### given n classifiers, if evenness is < 0.5, what is the likelihood of being correct?
### begin calculating evenness at 3 spp, if it is <0.5, you're good!


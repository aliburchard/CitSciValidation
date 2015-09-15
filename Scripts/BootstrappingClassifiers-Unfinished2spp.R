library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)

## consensus function:
test <- rawboot %>% filter(., CaptureEventID == "ASG000ffl7" | CaptureEventID == "ASG000b3xp" | CaptureEventID == "ASG000b48a") %>%
  data.table
#test <- raw %>% group_by(CaptureEventID) %>% sample_n(., 5)

## this is meant to run on a data frame or data table
## for n classifications, 
## 1. how many species?
## group by classificationID
calcConsensusSpp <- function(dt) {
  t <- dt %>% group_by(., CaptureEventID, ClassificationID) %>% 
    mutate(., sppcount = n()) %>% ## calculate the number of species
    group_by(., CaptureEventID) %>%
    mutate(.,numclassifiers = n_distinct(ClassificationID), consNumSpp = round(mean(sppcount)))
  
  u <- t %>%  group_by(., CaptureEventID, Species) %>% 
    summarise(., votes = n(), numclassifiers = numclassifiers[1], consNumSpp=consNumSpp[1]) 
 
  #just take the top n species
  u %>% group_by(., CaptureEventID) %>% 
      mutate(rank = row_number(-votes)) %>%
      filter()
      top_n(., n = N, wt=votes) %>% data.frame
    }) %>% select(., CaptureEventID, Species, votes, numclassifiers, N)


  return(u)
}
 
## how many classifiers does it take to get a good answer?

#gold <- read.csv("Data/gold_standard_data.csv")
bigGold <- read.csv("Data//BigGold.csv")
cons <- read.csv("Data/consensus_data.csv")
raw <- read.csv("Data/raw_data_in_gold.csv", sep = ",", header =T)



# which gold to use?
#gold <- bigGold
gold <- bigGold
gold %<>% rename(., GoldNumSpp = NumSpecies, GoldSpp = Species, GoldCount = Count)


## for any image that we know the answer
# take 1, 2, n classifiers



##gold100 <- gold1 %>% filter(., GoldSpp != "impossible") %>% sample_n(tbl = ., size = 100, replace = F)
##raw100 <- filter(raw, CaptureEventID %in% gold100$CaptureEventID) %>% select(., CaptureEventID, ClassificationID, Species, Count)
##dat100 <- left_join(raw100, gold100) %>% group_by(., CaptureEventID) %>% mutate(n.class = n_distinct(ClassificationID))

goldboot <- gold %>% filter(., GoldSpp != "impossible", GoldNumSpp == 1) %>% data.table
rawboot <- filter(raw, CaptureEventID %in% goldboot$CaptureEventID) %>% 
  select(., CaptureEventID, ClassificationID, Species, Count) 
datboot <- left_join(rawboot, goldboot) %>% group_by(., CaptureEventID) %>% mutate(n.class = n_distinct(ClassificationID))
rm(raw)

## because I want to be able to do this per species, do I need to do this for every photo? Or just in aggregate?
## # classifiers vs. % accuracy

overall.l <- list()
accuracy.l <- list()
rawsample.l <- list()
dropdat <- list()

for(n in 1:15){
  
  overall <- list()
  accuracy <- list()
  rawsample <- list()
  
  for(i in 1:10){ # currently only works for one species
    x <- datboot %>% group_by(., CaptureEventID) %>% 
      sample_n(., size = n, replace = T) %>%
      group_by(.) ## randomly samply n classificationIDs
  
    calcConsensusSpp(x) %>% data.table #calculate consensus species
    consdat <- left_join(goldboot, consensus)
      # how many species did any given person record?
      group_by(., CaptureEventID, ClassificationID) %>% mutate(NumSppRaw = n_distinct(Species)) %>% 
        ## for any given capture event, how many votes did a given species get?
        group_by(., CaptureEventID, Species) %>% 
          mutate(., votes = n()) %>% #calc max votes
          #filter(., votes == max(votes)) %>% #drop anyone who didn't win
      # now back up a level
      group_by(., CaptureEventID) %>% 
      # drop if there is a tie:
        mutate(., pass = ifelse(n_distinct(Species) == 1, "ok", "tie"),
               capSpp = paste(CaptureEventID, Species, "_"))
    
    dropdat <- consensus %>% filter(., pass == "tie") 
    consensus %<>% #filter(., pass == "ok") %>% 
      distinct(.) %>%
      mutate(., correct = ifelse(as.character(Species) == as.character(GoldSpp), 1, 0)) %>% data.frame
    
    ### ACROSS ITERATIONS, CALCULATE PERCENT CORRECT
    ## output datasets of overall and species-specific accuracies vs. # classifiers
    rawsample[[i]] <- consensus
    overall[[i]] <- consensus %>% group_by(.) %>% 
      summarise(., good = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
    accuracy[[i]] <- consensus %>% group_by(., Species) %>% 
      summarise(., good  = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
  }
  
  overall.l[[n]] <- do.call(rbind, overall)
  accuracy.l[[n]] <- do.call(rbind, accuracy)
  rawsample.l[[n]] <- do.call(rbind, rawsample)
  print(n)
}

out.overall <- do.call(rbind, overall.l)
out.accuracy <- do.call(rbind, accuracy.l)
out.raw <- do.call(rbind, rawsample.l) %>% data.table

# write.csv(out.overall, "Data/BootstrappedOverallFromConsensus.csv", row.names = F)
# write.csv(out.accuracy, "Data/BootstrappedBySppFromConsensus.csv", row.names = F)
#write.csv(out.raw, "Data/RawBootstrappedData.csv", row.names = F)
#rm(out.raw, rawsample, rawsample.l)

overall <- read.csv("Data/BootstrappedOverallFromConsensus.csv")
#byspp <- read.csv("Data/BootstrappedBySpp.csv")

overall <- out.overall
byspp <- out.accuracy

goldcounts <- gold1 %>% group_by(., GoldSpp) %>% summarise(., NumPix = n())  %>% filter(., NumPix >=5)
byspp.ltd <- byspp %>% 
  filter(., GoldSpp %in% goldcounts$GoldSpp) %>% 
  group_by(.) %>% mutate(., GoldSpp = droplevels(GoldSpp))

quartz()
ggplot(data = out.overall, aes(x = n, y = percentCorrect)) + geom_point()

quartz()
ggplot(data = byspp.ltd, aes(x = outOf, y = percentCorrect, group = GoldSpp, color = GoldSpp)) + 
  geom_point(size = .5) + geom_smooth(se = F)

spl1 <- c("aardvark", "aardwolf", "civet", "genet", "serval", "jackal", "wildcat", "mongoose",
          "lionMale", "lionFemale", "leopard", "cheetah", "hyenaSpotted", "hyenaStriped","vervetMonkey", "hare",
          "otherBird", "human", "guineaFowl", "koriBustard")
spl3 <- c("buffalo", "elephant", "giraffe", "zebra", "wildebeest", 
          "gazelleThomsons", "gazelleGrants")
spl4 <- c("reedbuck","rodent", "reptile",  "waterbuck", "rhinocerous",
          "warthog", "eland", "hippopotamus", "topi", "hartebeest", "impala")

quartz()
ggplot(data = filter(byspp.ltd, GoldSpp %in% spl4), aes(x = outOf, y = good/tot, group = GoldSpp, color = GoldSpp)) + 
  geom_point(size = .5) + geom_smooth(se = F)


##### can we say anything about species being harder than others (back to evenness and remnisicent of CoV times)
### create column for model predictions
### take minimum n for them to cross 0.8? but some just plain don't...

models <- mtcars %>% group_by(cyl) %>% do({
  model = lm(mpg ~ wt, data = .)
  rsq = summary(model)$r.squared
  data.frame(rsq)
  })

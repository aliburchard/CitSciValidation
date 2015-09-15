library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(lubridate)
library(data.table)

gold <- read.csv("Data/gold_standard_data.csv", stringsAsFactors = F)
bigGold <- read.csv("Data/BigGold.csv")
cons <- read.csv("Data/consensus_data.csv", stringsAsFactors = F)
#raw <- fread(input = "Data/raw_data_in_gold.csv", sep = ",", header =T)

### create the big gold dataset, can decide to limit or not...
extragold <- read.csv("Data//AdditionalGold.csv")
extragold %<>% group_by(., CaptureEventID) %>%
  mutate(., NumSpecies = n_distinct(Species)) %>%
  #filter(., !(Species %in% cons$Species)) %>% 
  group_by(.)

bigGold <- gold %>% mutate(., CaptureEventID = as.character(CaptureEventID), Source = "gold") %>% 
  rbind(.,{
    extragold %>% select(CaptureEventID,NumSpecies, Species, Count, Source) %>% 
      mutate(., CaptureEventID =  as.character(CaptureEventID)) 
  }) 

# # decide which to use -- regular gold or BigGOLD??
write.csv(bigGold, "Data/BigGold.csv", row.names = F)

gold <- bigGold
gold %<>% rename(., GoldNumSpp = NumSpecies, GoldSpp = Species, GoldCount = Count) %>% 
  mutate(., GoldSpp = ifelse(GoldSpp == "rodent", "rodents", GoldSpp))


cons %<>% mutate(., DateTime = ymd_hms(.$DateTime), # date time with lubridate
                 DayNight = ifelse(hour(DateTime) %in% 6:20, "day", "night"),
                 FracSupport = NumVotes/NumClassifications) # is it dark?

## gold-standard dataset provides a single final answer for every subject -- impossible is one of them!
## note that not all images are resolved -- the lack of resolution can be assessed in the "expert" classification table


### filter to just single species images
gold1 <- gold %>% filter(., GoldNumSpp == 1, CaptureEventID %in% cons$CaptureEventID)
consdat1 <- cons %>% filter(., CaptureEventID %in% gold1$CaptureEventID, NumSpecies == 1)


consdat <- left_join(consdat1, gold1) %>% 
  filter(., GoldSpp != "impossible") %>% #drop impossibles & spp not in classification interface
  mutate(., CaptureEventID = as.factor(CaptureEventID), #convert to factor
         Match = ifelse(as.character(Species) == as.character(GoldSpp), "right", "wrong")) #correct?


impossibles <- left_join(consdat1, gold1) %>% 
  filter(., GoldSpp == "impossible") %>% 
  mutate(., CaptureEventID = as.factor(CaptureEventID), #convert to factor
         Match = "impossible")

## add impossibles back in
consdat %<>% rbind_list(., impossibles) %>% 
  mutate(., Match = factor(Match, levels = c("right", "wrong", "impossible"), ordered = T))

write.csv(consdat, "Data/WorkingConsensesGold1Spp.csv", row.names = F)



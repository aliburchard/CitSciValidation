library(MASS)
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

head(consdat)

da_dat <- dplyr::select(consdat, FractionSupport, FractionBlanks, Evenness, Match)
train <- base::sample(1:dim(da_dat)[1], 100)
lda_mod <- lda(Match ~ ., subset = train, data = da_dat)
predict(lda_mod, da_dat[-train,])$class
plot(lda_mod, dimen=1, type = "both")

##### Final Analysis for COUNTS comparison #####
rm(list = ls())
gold <- read.csv("Data//BigGold.csv") >% rename(., GoldNumSpp = NumSpecies, GoldSpp = Species, GoldCount = Count)
consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv") # this is limited to just one species
rawboot <- read.csv("Data/RawBootDatNoExperts.csv")

## clean up counts from character to numbers:
rawboot %<>% mutate(., Count = as.character(Count)) %>%
  mutate(., count = as.numeric(ifelse(Count == "11-50", 11, ifelse(Count == "51+", 12, Count))))

consdat %<>% filter(., Source == "gold", Match != "impossible") %>% 
  mutate(goldcount = as.character(GoldCount)) %>%
  mutate(goldcount = as.numeric(ifelse(goldcount == "Nov-50", "11", ifelse(goldcount == "51+", "12", goldcount)))) 

####### IQR (50th percentile)
countdat <- rawboot %>% 
  group_by(., CaptureEventID) %>% 
  summarise(., trumin = min(count), trumax = max(count), trumed = median(count), 
            min = round(quantile(count, 1/4)), max = round(quantile(count, 3/4)))

countcompare <- consdat %>%
  left_join(., countdat) %>% 
  mutate(InRange = between(as.numeric(goldcount), min, max), range = max-min, exact = ifelse(trumed == goldcount, 1, 0)) %>% 
  na.omit %>% data.table

countcompare %>% summarise(., exact = sum(exact), good = sum(InRange), tot = n()) # how often is expert count in range of classifiers
quartz()
ggplot(countcompare, aes(x = range))+geom_histogram() + theme_bw()

## Figure S2
quartz()
ggplot(countcompare, aes(x = range)) + stat_ecdf() + theme_bw() + ylab("Cumulative Proportion of Images")

count(countcompare, range)
tally(countcompare)
summary(countcompare)

countcompare %>% filter(., range ==9) %>% ggplot(data = ., aes(x = Count)) + geom_histogram()

#3469/3800 = 91% of the time

# for the times when it's not:
badcounts <- countcompare %>% filter(., InRange==F) 
ggplot(badcounts, aes(x = range)) + geom_histogram()

filter(countdat, med != trumed)
filter(datboot, CaptureEventID == "ASG00012xq") %>% View

### how often is the expert count within the range provided by classifiers?
### how big is the range?
ggplot(countcompare, aes(y=range, x=as.factor(goldcount))) + geom_boxplot() 
ggplot(countcompare, aes(x=range)) + geom_histogram() 

quartz()
sporder = countcompare %>% na.omit %>% group_by(., GoldSpp) %>% 
  summarise(medrange = (median(range))) %>% arrange(., medrange) %>% 
  mutate(., order = row_number(medrange))

###### Figure S3 (bimodal count distribution)

widerange <- countcompare %>% filter(., range >= 10) %>% 
  select(., CaptureEventID, range, Species) %>% 
  data.frame %>% left_join(., rawboot)

quartz() 
ggplot(widerange, aes(x = as.factor(count))) + geom_histogram() +
  scale_x_discrete(breaks = c(1:12), labels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11-50", "51+")) +
  theme_bw() + xlab("Counts") + ylab("Frequency")

summary(countcompare)

cons %>% filter(., CaptureEventID == "ASG000byl0")


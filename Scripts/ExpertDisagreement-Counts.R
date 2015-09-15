##### Comparing Counts When Experts Disagree ####
countdat <- fread("Data/counts-differences-subset.csv") # this table only includes disagreements (85 capture events)
head(countdat)

countdat %<>% rename(., CaptureEventID = subject_zooniverse_id, Species = species)

### datatable reorganizing:
c <- countdat %>% select(., -V10) %>%
  gather(., key = Expert, Count, -CaptureEventID, -Species, -Difference) %>% 
  mutate(., Count = as.numeric(ifelse(Count == "11-50", "11", ifelse(Count == "51+", "12", Count)))) %>%
  na.omit(.)

c %<>% group_by(., CaptureEventID) %>% mutate(., NumSpp = n_distinct(Species)) # tall number of species and add in as column

ExpertSpread <- c %>% 
  group_by(., CaptureEventID, Species, NumSpp) %>% 
  summarise(., NumExperts = n_distinct(Expert), ExpCount = median(Count), MinCount = min(Count), MaxCount = max(Count), Spread = max(Count) - min(Count))


## 1. How often to experts agree?
count <- fread("Data/gold_standard_counts.csv") #already grouped by cap event and species
count %>% group_by(.) %>% summarise(n_distinct(CaptureEventID)) # why not 4,149??
count %>% group_by(., CaptureEventID, Species) %>% dim #4269 species/experts votes
count %>% filter(., NumExperts >1) %>% summarise(n()) # 135 with > 1 expert 

count %<>% 
  filter(., NumExperts >1) %>%
  select(., -subjectspecies) %>%
  group_by(., CaptureEventID) %>% 
  mutate(., NumSpp = n_distinct(Species), CEspp = paste(CaptureEventID, Species, sep = ".")) %>%
  group_by(.)

count %<>% mutate(.,ExpAgree = ifelse(MinExpert == MaxExpert, 1, 0), 
                    ExpSpread = MaxExpert - MinExpert,
                    MeanExpert = (MaxExpert + MinExpert)/2,
                    ConsAgree = ifelse(SppCountConsensus >= MinExpert & SppCountConsensus <= MaxExpert, 1, 0))
View(count)


## probability of experts agreeing ~ counts
count %<>% mutate(Expert = ifelse(ExpAgree == 0, "Disagree", "Agree"), Cons = ifelse(ConsAgree == 0, "Disagree", "Agree"),
                  SppCounts = ifelse(SppCountConsensus == 11, "11-50", ifelse(SppCountConsensus == 12, "51+", as.character(SppCountConsensus)))) %>%
            mutate(., SppCounts = factor(SppCounts, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11-50", "51+", ordered = T)))

quartz()
ggplot(filter(count, NumExperts >1), aes(x=as.factor(round(MeanExpert)), fill = Expert)) + 
  geom_bar(position = "stack") + ggtitle("Agreement Among Experts")
ggplot(filter(count, NumExperts >1), aes(x=SppCounts, fill = Expert)) + 
  geom_bar(position = "stack") + ggtitle("Agreement Among Experts")


quartz()
ggplot(count, aes(x=SppCounts, fill = Cons)) + 
  geom_bar(position = "stack") + ggtitle("Agreement Between Consensus and Experts")

count %>% filter(., NumExperts >1) %>% summarise(tot = n(), Exp = sum(ExpAgree), Cons = sum(ConsAgree), 
                                                 Exact = sum(ifelse(SppCountConsensus == round(MeanExpert), 1, 0))) # 135 with > 1 expert 


# gold.counts <- gold %>% 
#   mutate(., CEspp = paste(CaptureEventID, Species, sep =".")) %>% 
#   filter(., CEspp %in% count$CEspp) %>% #filter to just those 135 in the double counteds...
#   mutate(., Count = ifelse(Count == "11-50", "11", ifelse(Count == "51+", "12",Count))) %>% mutate(Count = as.numeric(Count))
# 
# # create measure of agreement vs total:
# ggplot(filter(count, ExpAgree == 1), aes(x = as.factor(MeanExpert))) + geom_bar() #distribution of agreement
# ggplot(filter(gold.counts), aes(x = as.factor(Count))) + geom_bar() # distribution of actual counts
# 
# gc <- gold.counts %>% select(., CaptureEventID, Species, CEspp, Count) %>% mutate(.,overall = "Overall")
# d <- count %>% filter(., ExpAgree == 1) %>% select(., CaptureEventID, Species, CEspp, Count = MeanExpert) %>%
#   mutate(.,overall = "ExpertAgree")
# 
# comparecounts <- rbind(gc, d) %>% mutate(Count = as.factor(Count))
# ggplot(comparecounts, aes(x=Count, fill = overall)) + geom_bar(position = "dodge")


head(consdat)

## use rawboot data to calculate range of answers

### 90th percentile range?
countdat <- rawboot %>% 
  mutate(count = as.numeric(ifelse(Count == "11-50", 11, ifelse(Count == "51+", 12, Count)))) %>%
  group_by(., CaptureEventID) %>% 
    mutate(., trumin = min(count), trumax = max(count), trumed = median(count), rank = ntile(count, 20)) %>% 
    filter(., rank %in% 2:19) %>%
  group_by(., CaptureEventID) %>% summarise(., min = min(count), max = max(count), med = round(median(count))) 
  
####### IQR 
countdat <- rawboot %>% 
  mutate(count = as.numeric(ifelse(Count == "11-50", 11, ifelse(Count == "51+", 12, Count)))) %>%
  group_by(., CaptureEventID) %>% 
  summarise(., trumin = min(count), trumax = max(count), trumed = median(count), 
         min = round(quantile(count, 1/4)), max = round(quantile(count, 3/4)))



countcompare <- consdat %>% filter(., Source == "gold", GoldNumSpp == 1) %>% 
  left_join(., countdat) %>% 
  mutate(goldcount = as.character(GoldCount)) %>%
  mutate(goldcount = as.numeric(ifelse(goldcount == "Nov-50", "11", ifelse(goldcount == "51+", "12", goldcount)))) %>% 
  mutate(InRange = between(as.numeric(goldcount), min, max), range = max-min, exact = ifelse(trumed == goldcount, 1, 0)) %>% 
  na.omit %>% data.table

countcompare %>% summarise(., exact = sum(exact), good = sum(InRange), tot = n()) # how often is expert count in range of classifiers
quartz()
ggplot(countcompare, aes(x = range))+geom_histogram() + theme_bw()
quartz()
ggplot(countcompare, aes(x = range))+stat_ecdf()+ theme_bw()


summary(countcompare)


#3780/3803 = 99% of the time

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

ggplot(na.omit(countcompare), aes(y=range, x=GoldSpp)) + geom_boxplot() + angle 

ggplot(countcompare, aes(x=range)) + geom_histogram() 


countcompare %>% filter(range <4) %>% summarise(., good = sum(InRange), tot = n())
countcompare %>% filter(range <4, !InRange)

rawboot %>% filter(., CaptureEventID == "ASG000bkv0") %>% ggplot(., aes(x = Count)) + geom_histogram()
summary(countcompare)

cons %>% filter(., CaptureEventID == "ASG000byl0")

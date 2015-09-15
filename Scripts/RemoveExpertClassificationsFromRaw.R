library(data.table)
library(magrittr)
library(dplyr)

experts <- fread("Data/expert-classifications-raw.csv", sep = ",", header =T)
raw <- fread("../MainData/raw_data.csv", sep = ",", header =T)
images <- fread("../MainData/all_images.csv")
effort <- fread("../MainData/search_effort.csv")
head(raw)
gold <- fread("../CitSciValidation/Data/gold_standard_data.csv")

dim(gold)
raw_no_experts <- raw %>% filter(., !(ClassificationID %in% experts$classification_id))

write.csv(raw_no_experts, "../MainData/raw_data_for_dryad.csv", row.names = F)



dim(raw_no_experts)

consensus <- read.csv("../MainData/consensus_data.csv")
table1 <- consensus %>% group_by(., Species) %>% tally()
View(table1)

gold %>% group_by(., CaptureEventID) %>% tally %>% filter(., n >1)

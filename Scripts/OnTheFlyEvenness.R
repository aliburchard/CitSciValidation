
overall.l <- list()
overall2.l <- list()


for(n in 21:30){
  
  overall <- list()
  overall2 <- list()
  
  
  for(i in 1:20){ # currently only works for one species
    x <- datboot %>% group_by(., CaptureEventID) %>% 
      sample_n(., size = n, replace = T) %>%
      group_by(.) ## randomly samply n classificationIDs
    
    evenness <- SingleSppEvenness(x) %>% data.table
    consensus <- SingleSppCons(x) %>% data.table %>% #calculate consensus species
      left_join(., evenness) %>% # add in the evenness score
      left_join(., goldboot) %>% # currently includes ties. possibly sample 1 from each? 
      mutate(., correct = ifelse(as.character(AlgoSpp) == as.character(GoldSpp), 1, 0), 
             EvenScore = ifelse(Evenness <= 0.5, "<=0.5", ">0.5")) %>%
      data.frame
  #     e25 <- consensus %>% filter(., Evenness <=0.25)
  #     e50 <-consensus %>% filter(., Evenness <=0.5)
  #     e75 <-consensus %>% filter(., Evenness <=0.75)
  #     e100 <- consensus
    
    #ggplot(data = consensus, aes(x = Evenness, y = correct)) + geom_jitter()
    
    ### ACROSS ITERATIONS, CALCULATE PERCENT CORRECT for a given evenness score?
    ## output datasets of overall and species-specific accuracies vs. # classifiers
    overall[[i]] <- consensus %>% filter(., Source == "gold") %>% group_by(., EvenScore) %>% 
      summarise(., good = sum(correct), tot = length(correct), percentCorrect = good/tot, iter = i, n = n)
    overall2[[i]] <- consensus %>% filter(., Source == "gold") %>% group_by(., correct) %>% 
      summarise(., meanEven = mean(Evenness), medEven = median(Evenness), iter = i, n = n)    
  }
  
  overall.l[[n]] <- do.call(rbind, overall)
  overall2.l[[n]] <- do.call(rbind, overall2)
  print(n)
}

out.overall <- do.call(rbind, overall.l) %>% na.omit
out.overall2 <- do.call(rbind, overall2.l)
ggplot(data = out.overall, aes(x = n, y = percentCorrect, group = EvenScore, color = EvenScore)) + 
  geom_point(size = .7) + stat_smooth() + scale_color_discrete(name = "Evenness Score\nPer 'n' classifiers") +
  theme_bw()
quartz()
View(out.overall)

evenness_images <- out.overall %>% mutate(., Data = ifelse(EvenScore == ">0.5", "Difficult Images", "Easy Images"))
write.csv(evenness_images, "Data/evenness_images.csv", row.names = F)

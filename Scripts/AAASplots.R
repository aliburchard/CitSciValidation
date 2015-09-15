consdat <- read.csv("Data/WorkingConsensesGold1Spp.csv") # this is limited to just one species


EvennessPlotDat <- consdat  %>% 
  mutate(Match = factor(Match, levels = c("right", "wrong", "impossible"), ordered = T)) %>% 
  select(., CaptureEventID, Evenness, FracBlanks, Match, Source) %>% 
  rename(., FractionBlanks = FracBlanks) %>%
  gather(., key = Metric, value = value, -Match, -Source, -CaptureEventID)

quartz()
ggplot(data = EvennessPlotDat, aes(x=Match, y=value)) + geom_boxplot() + facet_wrap(~Metric) +
  theme_bw() + xlab("") + theme(text = element_text(size=20)) + ylab("")


## have to create CGT and CGC

accuracy <- inner_join(CGT,CGC) %>%
  mutate(., nasort = ifelse(is.na(FalseNeg), -1, 1)) %>%
  arrange(.,  nasort, FalseNeg, desc(FalsePos)) %>%
  mutate(., order = as.numeric(rownames(.))) %>%
  gather(., key = TypeError, value = accuracy, -Species, -FNSE, -FPSE, -order, -totCons, -totTrue, -nasort, -totpix, -Group) %>%
  mutate(., SE = ifelse(TypeError == "FalseNeg", FNSE, FPSE), samplesize = ifelse(TypeError == "FalseNeg", totCons, totTrue)) %>%
  select(., Species, TypeError, accuracy, SE, samplesize, order, totpix) 

quartz()
ggplot(accuracy, aes(y=order, x = accuracy, color = Species)) + 
  geom_errorbarh(aes(xmin = accuracy - SE, xmax = accuracy + SE)) + geom_point() + 
  facet_grid(~TypeError) + theme_bw() + 
  theme(legend.position = "none", text = element_text(size=14), strip.text.x = element_text(size = 18)) +
  scale_y_discrete(limits = accuracy$Species[min(accuracy$order):max(accuracy$order)]) + xlab("") + ylab("")



quartz()
ggplot(accuracy_dat, aes(x=totpix, y=modified)) + 
  #geom_point(data = accuracy_0, color = "black", aes(shape = ErrorType)) +
  geom_point(aes(colour = ErrorType, shape = ErrorType)) + 
  geom_smooth(data = accuracy_dat, method = "lm", formula = y ~ x, aes(group = ErrorType, linetype = ErrorType), color = "gray50", se = F) +
  theme_bw() +
  scale_color_grey() +
  #facet_grid(~ErrorType) + 
  scale_x_log10(breaks = c(1, 10, 100, 1000, 10000, 100000), labels = comma, limits= c(10,100000)) + 
  scale_y_log10() +
  #scale_y_log10(breaks = c(.01, 1.0), limits = c(.01, 1.01)) +
  #scale_x_sqrt() + scale_y_sqrt() +
  theme(legend.position = c(.8, .85)) + 
  xlab("Total Captures") + ylab("error") +
  theme(panel.margin = unit(1, "lines"))



summary(lm(log(modified) ~ log(totpix), data=subset(accuracy_dat, TypeError == "FalseNeg")))
summary(lm(log(modified) ~ log(totpix), data=subset(accuracy_dat, TypeError == "FalsePos")))

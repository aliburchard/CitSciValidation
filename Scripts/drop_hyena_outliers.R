non_std_data <- read.csv("Data//DailyCapturesForHurdleModels-NotStandardized.csv")
hyena_no_out <- std_data %>% filter(., daily_lion < 0.05, species == "hyenaSpotted")


hyena_full_mod <- hurdle(detected ~ daily_lion + I(daily_lion^2) | 
                           Shade + AvT + Land1k + anRivd + confd + kopd + AvT*Shade + vegind +
                           buffalo_presence + wildebeest_presence + gazelle_presence + lion_presence + cheetah_presence, 
                         data=hyena_no_out, dist = "poisson", offset = log(ctdays))


ggplot(data = hyena_no_out, aes(x = daily_lion, y = detected/ctdays)) + geom_point()

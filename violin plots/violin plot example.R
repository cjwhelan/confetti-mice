

#The solution will probably involve adding a dodge position to the 
#summary layer. Example with dummy data below:
  
  library(ggplot2)
library(dplyr)

df <- data.frame(
  Locations = rep(c("Sec_51", "Sec_53"), each = 1000),
  Recovery = rnorm(2000),
  Scenarios = rep(rep(LETTERS[1:5], each = 200), 2)
)

glimpse(df)

ggplot(df, aes(Locations, Recovery,
               group = interaction(Locations, Scenarios))) +
  geom_violin(aes(fill = Scenarios)) +
  stat_summary(fun = median, geom = "point",
               position = position_dodge(0.9))

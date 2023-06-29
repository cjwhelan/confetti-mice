###make stacked bar/column plot of hotspots by month
###stack proportion goliath clades with 1 or fewer hotspots
###and goliath clades with >1 hotspots


rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggeasy)
library(easyGgplot2)



#D:\confetti_mice\goliaths
#make an object
goliaths_22_prop<-read.csv( "D:\\confetti_mice\\goliaths\\goliaths_22_prop.csv")

glimpse(goliaths_22_prop)

goliaths_22_prop$month <- as.factor(goliaths_22_prop$month)
goliaths_22_prop$treatment <- as.factor(goliaths_22_prop$treatment)
goliaths_22_prop$hotspots <- as.factor(goliaths_22_prop$hotspots)

glimpse(goliaths_22_prop)

#goliaths_22_prop %>% group_by("month", "treatment")

#      geom_col(width=.55, position=position_dodge((.65))) +

goliaths_22_prop$hotspots <- factor(goliaths_22_prop$hotspots,
                                    c("<1", ">1"))

p <- ggplot(goliaths_22_prop, aes(x=month, y = proportion)) + 
            geom_bar(aes(fill = treatment:hotspots),
                     width = 0.75, position=position_dodge((.80)), stat = "identity") +
  scale_fill_manual(values=c("darkblue", "Magenta", "blue", "red")) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 28),
    axis.title.x = element_text(color = "black", face = "bold", size = 28),
    axis.text.y = element_text(color = "black", face = "bold", size = 28),
    axis.title.y = element_blank(),
    legend.title=element_text(size=26, face = "bold"),
    legend.text=element_text(size=24, face = "bold"),
    legend.position = "FALSE"
  ) + 
  xlab("Month") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6))

p


####new info on interaction fill:
#ggplot(df, aes(x = as.numeric(interaction(sample,name)), y = count, fill = type)) + 
#geom_bar(stat = "identity",color="white") +
#  scale_x_continuous(breaks=c(1.5,3.5,5.5),labels=c("oak","birch","cedar"))

p2 <- ggplot(goliaths_22_prop, aes(x=(interaction(month,treatment)), 
                                y = proportion), fill = hotspots) + 
  geom_bar(stat = "identity",
           width = 0.75, position=position_dodge((.80))) +
  scale_fill_manual(values=c("darkblue", "Magenta", "blue", "red")) +
  theme_minimal() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 28),
    axis.title.x = element_text(color = "black", face = "bold", size = 28),
    axis.text.y = element_text(color = "black", face = "bold", size = 28),
    axis.title.y = element_blank(),
    legend.title=element_text(size=26, face = "bold"),
    legend.text=element_text(size=24, face = "bold"),
    legend.position = "FALSE"
  ) + 
  xlab("Month") +
  scale_x_discrete(breaks=c(1, 2, 3, 4, 5, 6))

p2




#try with easyGgplot2

library(easyGgplot2)

ggplot2.barplot(data=goliaths_22_prop, xName='month', yName="proportion",
                groupName='hotspots')

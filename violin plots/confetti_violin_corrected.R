##############
####with corrected data -- eliminate repeat entries
#Now for confetti mice
###try to create violin plots in single plot

rm(list = ls())

library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

#D:\confetti_mice
#make an object
confetti_violin<-read.csv( "D:\\confetti_mice\\mice all months data\\allmice_allmonths_6_June_23.csv")

glimpse(confetti_violin)

# convert month to factor
confetti_violin$month <- as.factor(confetti_violin$month)

glimpse(confetti_violin)


# Basic violin plot
allmice_violin_plot <- ggplot(confetti_violin, aes(x=month, 
                          y=ln2volume, groups = treatment, fill = treatment)) +
    geom_violin(draw_quantiles = c(0.95)) + 
    stat_summary(fun = "mean", geom="point", color = "white", 
               shape=23, size=4, position = position_dodge(0.9)) +
    stat_summary(fun = "median", geom="point", color = "yellow", 
               size=4, position = position_dodge(0.9)) +
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(
    axis.line = element_line(linewidth = 1.5, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36),
  ) + 
  xlab("Month") +
  ylab(expression(bold("Clade volume"(log["2"](μ*m^{"3"}))))) +
  ylim(12.5, 26) +
  geom_hline(aes(yintercept = 22.0), color = "black", linetype="dotted", linewidth = 2)

allmice_violin_plot

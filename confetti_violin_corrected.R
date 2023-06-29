##############
####with corrected data -- eliminate repeat entries
#Now for confetti mice
###try to create violin plots in single plot

rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

#D:\confetti_mice
#make an object
confetti_violin<-read.csv( "D:\\confetti_mice\\mice all months data\\allmice_allmonths_corrected_ecdf.csv")

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
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Month") +
  ylab("Clade volume (log2(cubic microns)") +
  ylim(12.5, 26)

allmice_violin_plot

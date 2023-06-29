###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)



#/Users/4477473/Desktop/3months
#make an object
goliath445 <-read.csv("D:/confetti_mice/mutation_frequency/3months/bubble445.csv")

glimpse(goliath445)

###make bubble plot

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath445$mutation <- as.factor(goliath445$mutation)

glimpse(goliath445)

#set order of gene names -- note: from least frequent to most frequent, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath445$mutation <- factor(goliath445$mutation,
                              levels = c("Kmt2d","Prex2","Sall1","Apob",
                                         "Muc4","Notch1","Erbb4","Fat4"))

#now create the basic bubble plot for goliath 445

bubble445_plot <- ggplot(goliath445, 
                         aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
#  scale_size_area(max_size = 2, name = "frequency") +
  scale_size_continuous(limits = c(0,1), range = c(1,20), breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

# now give it final polish 

bubble445_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 16),
    axis.title.y = element_blank(),
    legend.position = (c(0.8, 0.6)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 8)  ##adjust x-axis limits by max number of "positions"


###
#make an object
goliath9159 <-read.csv("D:/confetti_mice/mutation_frequency/3months/bubble9159.csv")

glimpse(goliath9159)

###make bubble plot

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9159$mutation <- as.factor(goliath9159$mutation)

glimpse(goliath9159)

#set order of gene names -- note: from least frequent to most frequent, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9159$mutation <- factor(goliath9159$mutation,
                        levels = c("Prex2","Epha2","Smad4","Fgfr2","Scn11a",
                                   "Nlrp1a","Akt1","Notch3","Muc4","Mir6917"))

#now create the basic bubble plot for goliath 445

bubble9159_plot <- ggplot(goliath9159, 
                         aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  #  scale_size_area(max_size = 2, name = "frequency") +
  scale_size_continuous(limits = c(0,1), range = c(1,20), breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

# now give it final polish 

bubble9159_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 16),
    axis.title.y = element_blank(),
    legend.position = (c(0.8, 0.7)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 8)  ##adjust x-axis limits by max number of "positions"

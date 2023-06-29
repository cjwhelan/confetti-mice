####create bubble plots of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months


rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggprism)


#D:\confetti_mice\mutation_frequency\7months
#make an object
goliath443 <-read.csv("D:\\confetti_mice\\mutation_frequency\\3months\\bubble443.csv")

glimpse(goliath443)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath443$mutation <- as.factor(goliath443$mutation) 

#set order of gene names -- note that this is from least 
#frequent to most frequent, which lines the names along
#the y-axis in descending order from most to least frequent
#I know this seems counter-intuitive!!!

goliath443$mutation <- factor(goliath443$mutation, 
                              levels = c("Crebbp", "Notch2", "Nf1", "Adam29",
                              "Nlrp1a", "Plcb1", "Prex2","Met", "Nsd1", 
                                         "Trp53"))

#now create the basic bubble plot for goliath 443

bubble443_plot <- ggplot(goliath443, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
#  scale_size_area(max_size = 15, name = "Frequency") +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

  
  
# now  give it final polish

bubble443_plot +
  theme_bw() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 16),
    axis.title.y = element_blank(),
    legend.position = (c(0.9, 0.35)),
    legend.title = element_text(colour="black", size=20, face="bold"),
    legend.text = element_text(colour="black", size=20, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  ) +
  xlab("Frequency of mutation") +
  xlim(1, 12)  ###adjust x-axis limits by max number of "positions"


#D:\confetti_mice\mutation_frequency\7months
#make an object
goliath445 <-read.csv("D:\\confetti_mice\\mutation_frequency\\3months\\bubble445.csv")

glimpse(goliath445)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath445$mutation <- as.factor(goliath445$mutation) 

#set order of gene names -- note that this is from least 
#frequent to most frequent, which lines the names along
#the y-axis in descending order from most to least frequent
#I know this seems counter-intuitive!!!

goliath445$mutation <- factor(goliath445$mutation, 
                              levels = c("Kmt2d", "Prex2", "Sall1", "Apob",
                                         "Muc4", "Notch1", "Erbb4","Fat4"))

#now create the basic bubble plot for goliath 445

bubble445_plot <- ggplot(goliath445, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))



# now make give it final polish

bubble445_plot +
  theme_bw() +
  theme_prism() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 36),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.55)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ###adjust x-axis limits by max number of "positions"

#D:\confetti_mice\mutation_frequency\7months

rm(list=ls())

#make an object
goliath9159 <-read.csv("D:\\confetti_mice\\mutation_frequency\\3months\\bubble9159.csv")

glimpse(goliath9159)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9159$mutation <- as.factor(goliath9159$mutation) 

#set order of gene names -- note that this is from least 
#frequent to most frequent, which lines the names along
#the y-axis in descending order from most to least frequent
#I know this seems counter-intuitive!!!

goliath9159$mutation <- factor(goliath9159$mutation, 
                              levels = c("Prex2", "Epha2", "Smad4", "Fgfr2",
                                         "Scn11a", "Nlrp1a", "Akt1", "Notch3", 
                                         "Muc4","Mir6917"))

#now create the basic bubble plot for goliath 445

bubble9159_plot <- ggplot(goliath9159, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))



# now make give it final polish

bubble9159_plot +
  theme_bw() +
  theme_prism() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 36),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.55)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ###adjust x-axis limits by max number of "positions"

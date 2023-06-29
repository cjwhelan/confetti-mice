####create bubble plots of mutation frequencies
### for genes with 10 highest frequencies


rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggprism)



#D:\confetti_mice\mutation_frequency\7months
#make an object
goliath006 <-read.csv("D:\\confetti_mice\\mutation_frequency\\7months\\bubble_006.csv")

glimpse(goliath006)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath006$mutation <- as.factor(goliath006$mutation) 
  

goliath006$mutation <- factor(goliath006$mutation, 
                           levels = c("Arid1a", "Crebbp", "Adgrb3", "Nf1",
                                      "Pten", "Kmt2a", "Fgfr3","Dicer1", "Ptprt", 
                                      "Kmt2c"))
  

bubble006_plot <- ggplot(goliath006, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
#  scale_size_continuous(limits=c(0,1), range = c(0,20), breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
 
bubble006_plot
 
bubble006_plot   +
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
    legend.position = (c(0.8, 0.7)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2) 
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 8)


####goliath 444

#make an object
goliath444 <-read.csv("D:\\confetti_mice\\mutation_frequency\\7months\\bubble_444.csv")

glimpse(goliath444)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath444$mutation <- as.factor(goliath444$mutation) 


goliath444$mutation <- factor(goliath444$mutation, 
                              levels = c("Akt2", "Met", "Scn1a", "Apob",
                                         "Erbb4", "Nlrp1a", "Ptprt","Prex2", 
                                         "Trp53", "Arid1a"))


bubble444_plot <- ggplot(goliath444, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
#  scale_size_continuous(limits=c(0,1), range = c(1,20), breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

bubble444_plot

bubble444_plot +
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
    legend.position = (c(0.8, 0.4)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2) 
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 25)



####goliath 445

#make an object
goliath445 <-read.csv("D:\\confetti_mice\\mutation_frequency\\7months\\bubble_445.csv")

glimpse(goliath445)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath445$mutation <- as.factor(goliath445$mutation) 


goliath445$mutation <- factor(goliath445$mutation, 
                              levels = c("Scn11a", "Egfr", "Nf1", "Fbxw7",
                                         "Cdh1", "Arid1a", "Kmt2a"))


bubble445_plot <- ggplot(goliath445, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

bubble445_plot +
  theme_bw() +
  theme_prism() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.x=element_blank(), #remove x axis labels
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 36),
    axis.title.y = element_blank(),
    legend.position = (c(0.8, 0.4)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2) 
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 9)

####goliath 446

#make an object
goliath446 <-read.csv("D:\\confetti_mice\\mutation_frequency\\7months\\bubble_446.csv")

glimpse(goliath446)


####make bubble plot

###order mutations from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath446$mutation <- as.factor(goliath446$mutation) 


goliath446$mutation <- factor(goliath446$mutation, 
                              levels = c("Trp53", "Apob", "Notch1", "Pced1b",
                                         "Grin2a", "Kmt2d", "Muc3", "Nf1",
                                         "Adamts18", "Prex2"))


bubble446_plot <- ggplot(goliath446, 
                         aes(x = position, y = mutation, size = frequency))+
  geom_point(fill = "cornflowerblue", color = "black", shape = 21) +
  scale_size_area(limits = c(0, 1), max_size = 20, breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

bubble446_plot +
  theme_bw() +
  theme_prism() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 36),
    axis.title.y = element_blank(),
    legend.position = (c(0.8, 0.4)),
    legend.title = element_text(colour="black", size=36, face="bold"),
    legend.text = element_text(colour="black", size=36, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, size = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 8)




## Bubble Plot for 443
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)
library(ggprism)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath443 <-read.csv("/Users/4477473/Desktop/3months/bubble443.csv")

glimpse(goliath443)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath443$mutation <- as.factor(goliath443$mutation)

glimpse(goliath443)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath443$mutation <- factor(goliath443$mutation,
                              levels = c("Crebbp","Notch2","Nf1","Adam29",
                                         "Nlrp1a","Plcb1","Prex2","Met",
                                         "Nsd1","Trp53"))

#now create the basic bubble plot for goliath 443

bubble443_plot <- ggplot(goliath443, 
                         aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
                  breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

#to show graph

bubble443_plot

# now give it final polish 

bubble443_plot +
  theme_bw() +
  theme_prism() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 35),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
    axis.title.y = element_blank(),
    legend.position = (c(0.8, 0.3)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


## Bubble Plot for 445
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)


#/Users/4477473/Desktop/3months
#make an object
goliath445 <-read.csv("/Users/4477473/Desktop/3months/bubble445.csv")

glimpse(goliath445)


#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath445$mutation <- as.factor(goliath445$mutation)

glimpse(goliath445)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath445$mutation <- factor(goliath445$mutation,
                              levels = c("Kmt2d","Prex2","Sall1","Apob",
                                         "Muc4","Notch1","Erbb4","Fat4"))

#now create the basic bubble plot for goliath 445

bubble445_plot <- ggplot(goliath445, 
                         aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
                  breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

#to show graph
bubble445_plot


# now give it final polish 

bubble445_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 35),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.6)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ##adjust x-axis limits by max number of "positions"

## Bubble plot for 9154_1
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath9154_1 <-read.csv("/Users/4477473/Desktop/3months/bubble9154_1.csv")

glimpse(goliath9154_1)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9154_1$mutation <- as.factor(goliath9154_1$mutation)

glimpse(goliath9154_1)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9154_1$mutation <- factor(goliath9154_1$mutation,
                                 levels = c("Smo","Cdh1","Apob","Sall1",
                                            "Egfr","Notch3","Kmt2c","Epha2"))

#now create the basic bubble plot for goliath 445

bubble9154_1_plot <- ggplot(goliath9154_1, 
                            aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
                  breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))

#to show graph

bubble9154_1_plot

# now give it final polish 

bubble9154_1_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 35),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.6)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


#### bubble plot for goliath9154_2

###create bubble polts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)
#/Users/4477473/Desktop/3months
#make an object
goliath9154_2 <-read.csv("/Users/4477473/Desktop/3months/bubble9154_2.csv")

glimpse(goliath9154_2)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9154_2$mutation <- as.factor(goliath9154_2$mutation)

glimpse(goliath9154_2)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9154_2$mutation <- factor(goliath9154_2$mutation,
                                 levels = c(("Rbm10","Triobp","Flg2","Fat1",
                                             "Trp63","Apob","Sall1","Notch4",
                                             "Adgrb3","Trp53"))
                                 
#now create the basic bubble plot for goliath 445
                                 
bubble9154_2_plot <- ggplot(goliath9154_2,
                            aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+ 
  scale_size_area(limits = c(0, 1), max_size = 20, 
                  breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
  
                                  
                                 
#to show graph
                                 
bubble9154_2_plot

bubble9154_2_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 35),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.6)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)
  ) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


#### bubble plot for goliath9156
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath9156 <-read.csv("/Users/4477473/Desktop/3months/bubble9156.csv")

glimpse(goliath9156)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9156$mutation <- as.factor(goliath9156$mutation)

glimpse(goliath9156)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9156$mutation <- factor(goliath9156$mutation,
                               levels = c(("Erbb4","Nlrp1a","Scn11a","Casp8",
                                           "Kmt2a","Akt1","Kmt2d","Notch4"
                                           "Egfr","Kcnh5"))
                               
#now create the basic bubble plot for goliath 445
                               
bubble9156_plot <- ggplot(goliath9156, 
                            aes(x = position, y = mutation, size = frequency)) +
    geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
    scale_size_area(limits = c(0, 1), max_size = 20, 
                breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
                               
#to show graph
                               
bubble9156_plot
                               
#now give it final polish 
                               
bubble9156_plot +
  theme_bw() +
theme(
   axis.line = element_line(linewidth = 1, colour = "black"),
   axis.ticks = element_line(color = "black", linewidth = 1),
   axis.text.x=element_blank(), #remove x axis labels
   axis.ticks.x=element_blank(), #remove x axis ticks
   axis.title.x = element_text(color = "black", face = "bold", size = 35),
   axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
   axis.title.y = element_blank(),
   legend.position = (c(0.75, 0.6)),
   legend.title = element_text(colour="black", size=16, face="bold"),
   legend.text = element_text(colour="black", size=16, face="bold"),
   panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)) +
 xlab("Alleles ranked by frequency") +
 xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


### bubble plot for goliath 9157
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath9157 <-read.csv("/Users/4477473/Desktop/3months/bubble9157.csv")

glimpse(goliath9157)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9157$mutation <- as.factor(goliath9157$mutation)

glimpse(goliath9157)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9157$mutation <- factor(goliath9157$mutation,
                               levels = c(("Kmt2d","Fat4","Grin2a","Cdh1",
                                           "Adgrb3","Sufu","Notch1","Kmt2a",
                                           "Prex2","Notch3"))
                               
#now create the basic bubble plot for goliath 445
                               
bubble9157_plot <- ggplot(goliath9157, 
                             aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
                breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
                               
#to show graph
                               
bubble9157_plot
                               
# now give it final polish 
                               
bubble9157_plot +
  theme_bw() +
  theme(
     axis.line = element_line(linewidth = 1, colour = "black"),
     axis.ticks = element_line(color = "black", linewidth = 1),
     axis.text.x=element_blank(), #remove x axis labels
     axis.ticks.x=element_blank(), #remove x axis ticks
     axis.title.x = element_text(color = "black", face = "bold", size = 35),
     axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
     axis.title.y = element_blank(),
     legend.position = (c(0.75, 0.6)),
     legend.title = element_text(colour="black", size=16, face="bold"),
     legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)) +
 xlab("Alleles ranked by frequency") +
 xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


### bubble plot for goliath 9158

###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath9158 <-read.csv("/Users/4477473/Desktop/3months/bubble9158.csv")

glimpse(goliath9158)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9158$mutation <- as.factor(goliath9158$mutation)

glimpse(goliath9158)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9158$mutation <- factor(goliath9158$mutation,
                               levels = c(("Notch3","Ptprt","Adgrb3","Dicer1",
                                           "Scn11a","Mien1","Nsd1","Scn1a",
                                           "Gpsm3","Rbm10"))
                               
#now create the basic bubble plot for goliath 445
                               
bubble9158_plot <- ggplot(goliath9158, 
                             aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
              breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
                               
#to show graph
                               
bubble9158_plot
                               
# now give it final polish 
                               
bubble9158_plot +
  theme_bw() +
  theme(
    axis.line = element_line(linewidth = 1, colour = "black"),
    axis.ticks = element_line(color = "black", linewidth = 1),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.title.x = element_text(color = "black", face = "bold", size = 35),
    axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
    axis.title.y = element_blank(),
    legend.position = (c(0.75, 0.6)),
    legend.title = element_text(colour="black", size=16, face="bold"),
    legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)) +
 xlab("Alleles ranked by frequency") +
 xlim(1, 12)  ##adjust x-axis limits by max number of "positions"


### bubble plot for goliath 9159
###create bubble plts of mutation frequencies
### for genes with 10 highest frequencies
###for goliaths at 3 months

#clear R's brain

rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#makes truncate axes
library(ggh4x)

#/Users/4477473/Desktop/3months
#make an object
goliath9159 <-read.csv("/Users/4477473/Desktop/3months/bubble9159.csv")

glimpse(goliath9159)

#order multation from from large to small

#change character to factor
#data2$x2 <- as.factor(data2$x2)

goliath9159$mutation <- as.factor(goliath9159$mutation)

glimpse(goliath9159)

#set order of gene names -- note: from least to frequent to most frequet, 
## which lines the names along the y-axis in descending order from most
## to least frequent... it's a bit counter-intuitive

goliath9159$mutation <- factor(goliath9159$mutation,
                               levels = c(("Prex2","Epha2","Smad4","Fgfr2",
                                           "Scn11a","Nlrp1a","Akt1","Notch3",
                                           "Muc4","Mir6917"))
                               
#now create the basic bubble plot for goliath 9159
                               
bubble9159_plot <- ggplot(goliath9159, 
                            aes(x = position, y = mutation, size = frequency)) +
  geom_point(fill = "cornflowerblue", color = "black", shape = 21)+
  scale_size_area(limits = c(0, 1), max_size = 20, 
                breaks = c(0.1, 0.2, 0.4, 0.6, 0.8, 1))
                               
#to show graph
                               
bubble9159_plot
                               
# now give it final polish 
                               
bubble9159_plot +
  theme_bw() +
  theme(
     axis.line = element_line(linewidth = 1, colour = "black"),
     axis.ticks = element_line(color = "black", linewidth = 1),
     axis.text.x=element_blank(), #remove x axis labels
     axis.ticks.x=element_blank(), #remove x axis ticks
     axis.title.x = element_text(color = "black", face = "bold", size = 35),
     axis.text.y = element_text(color = "black", face = "bold.italic", size = 35),
     axis.title.y = element_blank(),
     legend.position = (c(0.75, 0.4)),
     legend.title = element_text(colour="black", size=16, face="bold"),
     legend.text = element_text(colour="black", size=16, face="bold"),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.2)) +
  xlab("Alleles ranked by frequency") +
  xlim(1, 12)  ##adjust x-axis limits by max number of "positions"

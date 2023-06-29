#create ECDF plots for confetti mice experiment
#start with looking at max volumes contingent upon UV exposure
#create the ecdf column from log2 volume data


rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library("dplyr")


#Make truncated axes
library(ggh4x)

#D:\confetti_mice
#make an object
ECDF_volume<-read.csv( "D:\\confetti_mice\\ECDF_volume.csv")


#ECDF_volume

glimpse(ECDF_volume)


#################
#################

#truncated volumes to 8192 cubic microns and greater

trunc_volumes <- ECDF_volume %>%
  filter(month > "m0", structure_volume >= 8192)

glimpse(trunc_volumes)

write.csv(trunc_volumes, file = "D:/confetti_mice/trunc_volumes.csv")

##now with volume transformed by log base 2

trunc_log2_volume <- trunc_volumes
trunc_log2_volume[, 4] <- log(trunc_volumes[4], 2)
trunc_log2_volume

#create an ecdf column from log2 volume data

bytreatment <- trunc_log2_volume %>%
  group_by(treatment)

write.csv(bytreatment, file = "D:/confetti_mice/bytreatment.csv")

fun.ecdf<-ecdf(bytreatment$structure_volume) #make a function that applies ecdf to your data

my.ecdf<-fun.ecdf(sort(bytreatment$structure_volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 

bytreatment$distribution_volume<-my.ecdf #add the cumulative probabilities to your data frame

bytreatment

write.csv(trunc_log2_volume, file = "D:/confetti_mice/trunc_log2_volume.csv")


#density_volume <- ECDF_log2_volume %>%
#  mutate(
#    distribution_volume = ecdf(ECDF_log2_volume$structure_volume))

#density_volume


# create an ecdf column to ECDF_log2_volume

#density_volume <- ECDF_log2_volume %>% 
#density_volume <- ecdf(ECDF_log2_volume$structure_volume)
#density_volume

write.csv(ECDF_log2_volume, file = "D:/confetti_mice/ECDF_log2_volume.csv")


trunc_log2_volume_plot <- ggplot(trunc_log2_volume, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

trunc_log2_volume_plot

trunc_log2_volume_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=14), 
    legend.text=element_text(face = "bold", size=14),
    legend.position = c(0.2, 0.8),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Maximum volume log[2](cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
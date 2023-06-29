#create ECDF plots for confetti mice experiment
#start with looking at max volumes contingent upon UV exposure


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


#truncated volumes to 8192 cubic microns and greater

trunc_volumes <- ECDF_volume %>%
  filter(month > "m0", structure_volume >= 8192)

#trunc_volumes

write.csv(trunc_volumes, file = "D:/confetti_mice/trunc_volumes.csv")

glimpse(trunc_volumes)

#Now ECDF of volumes by UV category

#################
#################

#now with volume transformed by log base 2

ECDF_log2_truncvolume <- trunc_volumes
ECDF_log2_truncvolume[, 4] <- log(trunc_volumes[4], 2)
ECDF_log2_truncvolume

write.csv(ECDF_log2_truncvolume, file = "D:/confetti_mice/ECDF_log2_truncvolume.csv")


ECDF_log2_truncvolume_plot <- ggplot(ECDF_log2_truncvolume, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"))

ECDF_log2_truncvolume_plot

ECDF_log2_truncvolume_plot  +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

ECDF_log2_truncvolume_plot2 <- ggplot(ECDF_log2_truncvolume, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"))
  

ECDF_log2_truncvolume_plot2


ECDF_log2_truncvolume_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.90, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############


#now get ecdf plots month by month for mice represented from m1 through m6

#filter to get data for mice: 3, 6 22, 970

mice_allmonths <- ECDF_log2_truncvolume %>% 
  filter(mouse_id %in% c("#003", "#006", "#022", "#970")) #allows filtering of multiple mice

#write to hard drive
write.csv(mice_allmonths, file = "D:/confetti_mice/mice_allmonths.csv")

#Create ECDF plots for mice_allmonths
mice_allmonths_plot <- ggplot(mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"))

mice_allmonths_plot

mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nAll mice represented in months 1-6") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

mice_allmonths_plot <- ggplot(mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 2, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"))


mice_allmonths_plot


mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nAll mice represented in months 1-6") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))


#now filter by month 1 == m1
m1_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m1")

#write to hard drive
write.csv(m1_mice_allmonths, file = "D:/confetti_mice/m1_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m1_mice_allmonths_plot <- ggplot(m1_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"))

m1_mice_allmonths_plot

m1_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 1") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m1_mice_allmonths_plot2 <- ggplot(m1_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m1_mice_allmonths_plot2


m1_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 1") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now filter by month 2 == m2
m2_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m2")

#write to hard drive
write.csv(m2_mice_allmonths, file = "D:/confetti_mice/m2_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m2_mice_allmonths_plot <- ggplot(m2_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))

m2_mice_allmonths_plot

m2_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 2") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m2_mice_allmonths_plot2 <- ggplot(m2_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m2_mice_allmonths_plot2


m2_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 2") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#now filter by month 3 == m3
m3_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m3")

#write to hard drive
write.csv(m3_mice_allmonths, file = "D:/confetti_mice/m3_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m3_mice_allmonths_plot <- ggplot(m3_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))

m3_mice_allmonths_plot

m3_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 3") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m3_mice_allmonths_plot2 <- ggplot(m3_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m3_mice_allmonths_plot2


m3_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 3") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#now filter by month 4 == m4
m4_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m4")

#write to hard drive
write.csv(m4_mice_allmonths, file = "D:/confetti_mice/m4_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m4_mice_allmonths_plot <- ggplot(m4_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))

m4_mice_allmonths_plot

m4_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 4") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m4_mice_allmonths_plot2 <- ggplot(m4_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m4_mice_allmonths_plot2


m4_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 4") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now filter by month 5 == m5
m5_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m5")

#write to hard drive
write.csv(m5_mice_allmonths, file = "D:/confetti_mice/m5_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m5_mice_allmonths_plot <- ggplot(m5_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))

m5_mice_allmonths_plot

m5_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 5") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m5_mice_allmonths_plot2 <- ggplot(m5_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m5_mice_allmonths_plot2


m5_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 5") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#now filter by month 6 == m6
m6_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m6")

#write to hard drive
write.csv(m6_mice_allmonths, file = "D:/confetti_mice/m6_mice_allmonths.csv")


#now create ecdf plot for month 1 with all mice represented from m1 through m6
#mice: #003, #006, #022, #970

m6_mice_allmonths_plot <- ggplot(m6_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))

m6_mice_allmonths_plot

m6_mice_allmonths_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 6") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#now truncate y-axis

m6_mice_allmonths_plot2 <- ggplot(m6_mice_allmonths, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "red"))


m6_mice_allmonths_plot2


m6_mice_allmonths_plot2  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure \nMonth 6") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(10, 30.0) +
  ylim(0.75, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
##############
#Now clear everything and use KS and AD tests
#to determine if UV and No UV ECDF plots differ
#for all mice represented in months 1-6
#together and stratified by month

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


#truncated volumes to 8192 cubic microns and greater

trunc_volumes <- ECDF_volume %>%
  filter(month > "m0", structure_volume >= 8192)

#trunc_volumes

glimpse(trunc_volumes)

#################
#################

#now with volume transformed by log base 2

ECDF_log2_truncvolume <- trunc_volumes
ECDF_log2_truncvolume[, 4] <- log(trunc_volumes[4], 2)
ECDF_log2_truncvolume

#filter to get data for mice: 3, 6 22, 970

mice_allmonths <- ECDF_log2_truncvolume %>% 
  filter(mouse_id %in% c("#003", "#006", "#022", "#970")) #allows filtering of multiple mice

#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970


#install.packages("dgof")

library(dgof)

a <- mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month


#now filter by month 1 == m1
m1_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m1")


a <- m1_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m1_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 100)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month


#now filter by month 2 == m2
m2_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m2")

a <- m2_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m2_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 100)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 100)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month

#now filter by month 3 == m3
m3_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m3")

a <- m3_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m3_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 100)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 100)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month

#now filter by month 4 == m4
m4_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m4")

a <- m4_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m4_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 1000)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 1000)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month

#now filter by month 5 == m5
m5_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m5")

a <- m5_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m5_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 1000)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 1000)


######################
##############
#Test for differences in ECDFs for UV and No UV
#for all mice represented in months 1-6
# mice #003, #006, #022, #970
#stratified by month

#now filter by month 6 == m6
m6_mice_allmonths <- mice_allmonths %>% 
  filter(month == "m6")


a <- m6_mice_allmonths %>%
  filter(treatment == "No UV")
a

b <- m6_mice_allmonths %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 1000)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 1000)


######################
##############
######################


#create ECDF plots Stratified by month
#filter by month
month1 <- ECDF_log2_volume %>%
  filter(month == "m1")

write.csv(month1, file = "D:/confetti_mice/month1_log2.csv")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month1_plot <- ggplot(month1, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

month1_plot

month1_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month1)") +
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
  xlab("Maximum volume (log base 2(cubic microns)") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs for LDOS pre vs post at 72h and pH<6.6


#install.packages("dgof")

library(dgof)

a <- month1 %>%
  filter(treatment == "No UV")
a

b <- month1 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

######################
#create ECDF plots Stratified by month
#filter by month
month2 <- ECDF_log2_volume %>%
  filter(month == "m2")

write.csv(month2, file = "D:/confetti_mice/month2_log.csv")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month2_plot <- ggplot(month2, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

month2_plot

month2_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month 2)") +
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
  xlab("Maximum volume (log base 2(cubic microns)") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#Test for differences in ECDFs UV and No UV by month


#install.packages("dgof")

library(dgof)

a <- month2 %>%
  filter(treatment == "No UV")
a

b <- month2 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

######################

######################
#create ECDF plots Stratified by month
#filter by month
month3 <- ECDF_log2_volume %>%
  filter(month == "m3")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month3_plot <- ggplot(month3, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(size = 1.2, pad = FALSE)

month3_plot

month3_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month 3)") +
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
  xlab("Maximum volume (log base 2(cubic microns)") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs UV and No UV by month


#install.packages("dgof")

library(dgof)

a <- month3 %>%
  filter(treatment == "No UV")
a

b <- month3 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

##############
######################
#create ECDF plots Stratified by month
#filter by month
month4 <- ECDF_log2_volume %>%
  filter(month == "m4")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month4_plot <- ggplot(month4, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(size = 1.2, pad = FALSE)

month4_plot

month4_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month 4)") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs UV and No UV by month


#install.packages("dgof")

library(dgof)

a <- month4 %>%
  filter(treatment == "No UV")
a

b <- month4 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

######################
######################
#create ECDF plots Stratified by month
#filter by month
month5 <- ECDF_log2_volume %>%
  filter(month == "m5")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month5_plot <- ggplot(month5, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(size = 1.2, pad = FALSE)

month5_plot

month5_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month 5)") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs UV and No UV by month


#install.packages("dgof")

library(dgof)

a <- month5 %>%
  filter(treatment == "No UV")
a

b <- month5 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

######################
######################
#create ECDF plots Stratified by month
#filter by month
month6 <- ECDF_log2_volume %>%
  filter(month == "m6")

#transforming volume by log base 2

#ECDF_log2_volume <- ECDF_volume
#ECDF_log2_volume[, 3] <- log(ECDF_volume[3], 2)
#ECDF_log2_volume


month6_plot <- ggplot(month6, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

month6_plot

month6_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (Month 6)") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs UV and No UV by month


#install.packages("dgof")

library(dgof)

a <- month6 %>%
  filter(treatment == "No UV")
a

b <- month6 %>%
  filter(treatment == "UV")
b


# plotting the result
# visualization
plot(ecdf(a$structure_volume),
     col = "blue")
plot(ecdf(b$structure_volume),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$structure_volume, b$structure_volume)

ks.test(jitter(a$structure_volume), jitter(b$structure_volume))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$structure_volume
u2 <- b$structure_volume
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

######################
##############
##############################
######################

#######use A-D test for up to 4 comparisons
#perform k-sample Anderson-Darling test

library(kSamples)

u1 <- ECDF_log2_volume %>%
  filter(month == "m1" & treatment == "No UV")
u1

u2 <- ECDF_log2_volume %>%
  filter(month == "m1" & treatment == "UV")

u2

u3 <- ECDF_log2_volume %>%
  filter(month == "m2" & treatment == "No UV")
u3

u4 <- ECDF_log2_volume %>%
  filter(month == "m2" & treatment == "UV")

u4


#now with pre-post pH data
u1 <- u1$structure_volume
u2 <- u2$structure_volume
u3 <- u3$structure_volume
u4 <- u4$structure_volume

ad.test(u1, u2, u3, u4, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)


##############
###filter for UV treatment####


UVlog2 <- ECDF_log2_volume %>%
  filter(treatment == "UV" & month > "m0")
#filter(month == "m1" | month = "m2" | month = "m3" | month = "m4" | month = "m5" | month = "m6") 
           

UVlog2


UVlog2_plot <- ggplot(UVlog2, aes(x = (structure_volume), color = month)) +
  labs(colour = "Month") +
  scale_colour_discrete(labels = c("Month1", "Month2", "Month3", "Month4", "Month5", "Month6")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

UVlog2_plot

UVlog2_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution \nUV Exposure and Month (cubic microns)") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
###filter for UV treatment####


no_UVlog2 <- ECDF_log2_volume %>%
  filter(treatment == "No UV" & month > "m0")
#filter(month == "m1" | month = "m2" | month = "m3" | month = "m4" | month = "m5" | month = "m6") 


no_UVlog2


no_UVlog2_plot <- ggplot(no_UVlog2, aes(x = (structure_volume), color = month)) +
  labs(colour = "Month") +
  scale_colour_discrete(labels = c("Month1", "Month2", "Month3", "Month4", "Month5", "Month6")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

no_UVlog2_plot

no_UVlog2_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution \nNo UV Exposure and Month (cubic microns)") +
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
  xlab("Maximum volume (log base 2(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Distribution") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
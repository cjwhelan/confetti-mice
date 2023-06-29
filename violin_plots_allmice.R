##############
#Now for confetti mice

rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(ggeasy)

#Make truncated axes
#library(ggh4x)

#D:\confetti_mice
#make an object
confetti_violin<-read.csv( "D:\\confetti_mice\\mice all months data\\allmice_allmonths_corrected_ecdf.csv")

glimpse(confetti_violin)

#bymonth <- ECDF_violin %>%
#  group_by(month)

#filter by month

m1 <- confetti_violin %>% 
  filter(month == "m1")

# Basic violin plot
m1_plot <- ggplot(m1, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
# geom_boxplot(width=.1, fill = "white") +
    theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
#  ggtitle("B") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
#    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 1") +
  ylab("Clade volume (log2(cubic microns)") +
  ylim(12.5, 26)
m1_plot

###############
m2 <- confetti_violin %>% 
  filter(month == "m2")

# Basic violin plot
m2_plot <- ggplot(m2, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
#   geom_boxplot(width=.1, fill = "white") +
    theme_minimal() +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(legend.position="FALSE") +
#  ggtitle("      Month 2") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 2") +
  #  ylab("Structure volume (log2(cubic microns)") +
  easy_remove_y_axis() +
  ylim(12.5, 26)
m2_plot

###############
m3 <- confetti_violin %>% 
  filter(month == "m3")

# Basic violin plot
m3_plot <- ggplot(m3, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
#   geom_boxplot(width=.1, fill = "white") +
      theme_minimal() +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(legend.position="FALSE") +
#  ggtitle("      Month 3") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 3") +
  #  ylab("Structure volume (log2(cubic microns)") +
  easy_remove_y_axis() +
  ylim(12.5, 26)
m3_plot

###############
m4 <- confetti_violin %>% 
  filter(month == "m4")

# Basic violin plot
m4_plot <- ggplot(m4, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
#  geom_boxplot(width=.1, fill = "white") +
      theme_minimal() +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(legend.position="FALSE") +
#  ggtitle("      Month 4") +
  theme(
#    plot.title = element_text(face = "bold", size = 18),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 4") +
  #  ylab("Structure volume (log2(cubic microns)") +
  easy_remove_y_axis() +
  ylim(12.5, 26)
m4_plot

###############
m5 <- confetti_violin %>% 
  filter(month == "m5")

# Basic violin plot
m5_plot <- ggplot(m5, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
#  geom_boxplot(width=.1, fill = "white") +
    theme_minimal() +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(legend.position="FALSE") +
#  ggtitle("      Month 5") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 5") +
  #  ylab("Structure volume (log2(cubic microns)") +
  easy_remove_y_axis() +
  ylim(12.5, 26)
m5_plot

###############
m6 <- confetti_violin %>% 
  filter(month == "m6")

# Basic violin plot
m6_plot <- ggplot(m6, aes(x=treatment, y=ln2volume, fill = treatment)) + 
  geom_violin(draw_quantiles = c(0.95)) + 
  stat_summary(fun.y=mean, geom="point", fill = "white", shape=23, size=4) +
  stat_summary(fun.y=median, geom="point", color = "yellow", size=4) +
#   geom_boxplot(width=.1, fill = "white") +
      theme_minimal() +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  theme(legend.position="FALSE") +
#  ggtitle("      Month 6") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20)
  ) + 
  xlab("Month 6") +
#  ylab("Structure volume (log2(cubic microns)") +
  easy_remove_y_axis() +
  ylim(12.5, 26)
m6_plot

grid.arrange(m1_plot, m2_plot, m3_plot, m4_plot, m5_plot, m6_plot, ncol = 6, widths = c(1.0, 0.75, 0.75, 0.75, 0.75, 0.75))

######
##data from Chandler
rm(list =ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library("dplyr")
library(gridExtra)
library(ggeasy)

#make an object
volume_diff<-read.csv( "D:\\confetti_mice\\ecdf_difference.csv")

glimpse(volume_diff)

dx_plot <- ggplot(data=volume_diff, aes(x=dx, y=ecdf_y, group=month, color=month)) +
  geom_point(size = 2)  +
  theme_classic() +
  scale_color_manual(values = c("blue", "brown", "forest green", "dark orange1", 
                                "red", "dark violet")) +
  ggtitle("Volume difference between UV and No UV treatments") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
#    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = c(0.7, 0.45),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
# xlab(bquote(X-Axis[subscript]))
  xlab(bquote(log[2])~cubic micron) +
  ylab("Cumulative density") +
  geom_vline(xintercept = 0, size = 1.2)  + 
  xlim(-1, 4) +
  guides(colour = guide_legend(override.aes = list(size=6)))

  
dx_plot

 #new plot -- Ken?
max_plot <- ggplot(data=volume_diff, aes(x=UV_x, y=dx, group=month, color=month)) +
  geom_point(size = 2)  +
  theme_classic() +
  scale_color_manual(values = c("blue", "brown", "forest green", "dark orange1", 
                                "red", "dark violet")) +
  ggtitle("Volume difference between UV and No UV treatments") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    legend.position = c(0.3, 0.8),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Maximum volume (UV treatment)") +
  ylab("Cumulative volume difference (UV - no UV)") +
  geom_hline(yintercept = 0, size = 1.2)  + 
  xlim(12, 26) +
  guides(colour = guide_legend(override.aes = list(size=8)))


max_plot

#filter by month

m1_dx <- volume_diff %>% 
  filter(month == "m1")

m1_dx_plot <- ggplot(data=m1_dx, aes(x=dx, y=ecdf_y, color=month)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("blue")) +
  xlim(-1, 4.0)
  

m1_dx_plot

#filter by month

m2_dx <- volume_diff %>% 
  filter(month == "m2")

m2_dx_plot <- ggplot(data=m2_dx, aes(x=dx, y=ecdf_y, color=month)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("red")) +
  xlim(-1, 4.0)

m2_dx_plot

#filter by month

m3_dx <- volume_diff %>% 
  filter(month == "m3")

m3_dx_plot <- ggplot(data=m3_dx, aes(x=dx, y=ecdf_y, color=month))
+
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("forest green")) +
  xlim(-1, 4.0)

m3_dx_plot

#filter by month

m4_dx <- volume_diff %>% 
  filter(month == "m4")

m4_dx_plot <- ggplot(data=m4_dx, aes(x=dx, y=ecdf_y, color=month)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("dark orange1")) +
  xlim(-1, 4.0)

m4_dx_plot

#filter by month

m5_dx <- volume_diff %>% 
  filter(month == "m5")

m5_dx_plot <- ggplot(data=m5_dx, aes(x=dx, y=ecdf_y, color=month)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("brown")) +
  xlim(-1, 4.0)

m5_dx_plot

#filter by month

m6_dx <- volume_diff %>% 
  filter(month == "m6")

m6_dx_plot <- ggplot(data=m6_dx, aes(x=dx, y=ecdf_y, color=month)) +
  geom_point(size = 2) +
  geom_vline(xintercept = 0, size = 1.2)  +
  theme_classic() +
  scale_color_manual(values = c("dark violet")) +
  xlim(-1, 4.0)

m6_dx_plot

####try bubble plot

rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library("dplyr")

#make object

bubble_12<-read.csv( "D:\\confetti_mice\\bubble data\\bubble_12.csv")

glimpse(bubble_12)

bubble_12$month <- as.factor(bubble_12$month) 

glimpse(bubble_12)

#ggplot(mpg, aes(x=cty, y=hwy, size = pop)) +geom_point(alpha=0.7)

bubble <- ggplot(bubble_12, aes(x = density, y = dx, size = mean_volume, fill = month)) +
  geom_point(shape=21, alpha = 0.75, color="black", stroke = 1.1) +
  scale_size_continuous(breaks = c(10000,50000,100000,250000,500000,1000000),
                  name = "Mean Volume \n(cubic microns)", range = c(8,20)) +
  theme_classic() +
scale_fill_manual(values = c("blue", "brown", "forest green", "dark orange1", 
                              "red", "dark violet")) +
  ggtitle("Volume difference between UV and No UV treatments by month") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Cumulative density") +
  ylab("Cumulative log base 2 volume difference (UV - No UV)") +
  ylim(-1, 2.5) +
  xlim(0,1) +
  scale_x_continuous(breaks = c(0.12, 0.24, 0.36, 0.48, 0.60, 0.72, 0.84, 0.96)) +
  theme(legend.box.just = "center") +
  guides(fill = guide_legend(override.aes = list(size=8))) +
  theme(legend.title = element_text(colour="black", size=12, face="bold")) +
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))

bubble

####now from density = 0.88 to density = 0.98


bubble_88_98<-read.csv( "D:\\confetti_mice\\bubble data\\bubble_88_98_2.csv")

glimpse(bubble_88_98)

bubble_88_98$month <- as.factor(bubble_88_98$month) 

glimpse(bubble_88_98)

#ggplot(mpg, aes(x=cty, y=hwy, size = pop)) +geom_point(alpha=0.7)

bubble_88_98_plot <- ggplot(bubble_88_98, aes(x = density, y = dx, size = mean_volume, fill = month)) +
  geom_point(shape=21, alpha = 0.75, color="black", stroke = 1.1) +
  scale_size_continuous(breaks = c(100000,500000,1000000,2500000,5000000,10000000),
                        name = "Mean Volume \n(cubic microns)", range = c(8,20)) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "brown", "forest green", "dark orange1", 
                               "red", "dark violet")) +
#  ggtitle("Volume difference between UV \nand No UV treatments by month") +
  theme(
#    plot.title = element_text(face = "bold", size = 30),
    plot.margin = margin(0.5, 1.5, 0.5, 1.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 28),
    axis.title.x = element_text(color = "black", face = "bold", size = 28),
    axis.text.y = element_text(color = "black", face = "bold", size = 28),
    axis.title.y = element_text(face = "bold", size = 28)
  ) + 
  xlab("Cumulative density") +
  ylab("Cumulative log2 volume difference \n(UV - No UV)") +
  ylim(-1, 2.5) +
  xlim(0,1) +
  scale_x_continuous(breaks = c(0.88, 0.90, 0.92, 0.94, 0.96, 0.98)) +
  theme(legend.box.just = "center") +
  guides(fill = guide_legend(override.aes = list(size=8))) +
  theme(legend.title = element_text(colour="black", size=28, face="bold")) +
  theme(legend.text = element_text(colour="black", size = 28, face = "bold"))

bubble_88_98_plot

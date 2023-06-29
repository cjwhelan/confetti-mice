#create scatter plots of ln(FICM) versus size (log base 2 of structure volume)


rm(list=ls())

library(dplyr)
library(gcookbook)
library(ggplot2)
library(ggthemes)

#make an object
first_inc_mom <- read.csv("D:\\confetti_mice\\first_inc_mom.csv")

glimpse(first_inc_mom)

#filter by month and treatment

####################
#####  month 1 and no uv


m1_no_uv <- first_inc_mom %>% 
  filter(month == "m1", treatment == "no_uv")


#create scatter plot and fitted line
m1_no_uv_plot <- ggplot(m1_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 1, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m1_no_uv_plot

####################
#####  month 1 and uv


m1_uv <- first_inc_mom %>% 
  filter(month == "m1", treatment == "uv")


#create scatter plot and fitted line
m1_uv_plot <- ggplot(m1_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 1, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m1_uv_plot


####################
#####  month 2 and no uv


m2_no_uv <- first_inc_mom %>% 
  filter(month == "m2", treatment == "no_uv")


#create scatter plot and fitted line
m2_no_uv_plot <- ggplot(m2_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 2, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m2_no_uv_plot

####################
#####  month 2 and uv


m2_uv <- first_inc_mom %>% 
  filter(month == "m2", treatment == "uv")


#create scatter plot and fitted line
m2_uv_plot <- ggplot(m2_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 2, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m2_uv_plot

####################
#####  month 3 and no uv


m3_no_uv <- first_inc_mom %>% 
  filter(month == "m3", treatment == "no_uv")


#create scatter plot and fitted line
m3_no_uv_plot <- ggplot(m3_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 3, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m3_no_uv_plot

####################
#####  month 3 and uv


m3_uv <- first_inc_mom %>% 
  filter(month == "m3", treatment == "uv")


#create scatter plot and fitted line
m3_uv_plot <- ggplot(m3_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 3, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m3_uv_plot

####################
#####  month 4 and no uv


m4_no_uv <- first_inc_mom %>% 
  filter(month == "m4", treatment == "no_uv")


#create scatter plot and fitted line
m4_no_uv_plot <- ggplot(m4_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 4, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m4_no_uv_plot

####################
#####  month 4 and uv


m4_uv <- first_inc_mom %>% 
  filter(month == "m4", treatment == "uv")


#create scatter plot and fitted line
m4_uv_plot <- ggplot(m4_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 4, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m4_uv_plot

####################
#####  month 5 and no uv


m5_no_uv <- first_inc_mom %>% 
  filter(month == "m5", treatment == "no_uv")


#create scatter plot and fitted line
m5_no_uv_plot <- ggplot(m5_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 5, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m5_no_uv_plot

####################
#####  month 5 and uv


m5_uv <- first_inc_mom %>% 
  filter(month == "m5", treatment == "uv")


#create scatter plot and fitted line
m5_uv_plot <- ggplot(m5_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 5, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m5_uv_plot


####################
#####  month 6 and no uv


m6_no_uv <- first_inc_mom %>% 
  filter(month == "m6", treatment == "no_uv")


#create scatter plot and fitted line
m6_no_uv_plot <- ggplot(m6_no_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 6, no UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m6_no_uv_plot

####################
#####  month 6 and uv


m6_uv <- first_inc_mom %>% 
  filter(month == "m6", treatment == "uv")


#create scatter plot and fitted line
m6_uv_plot <- ggplot(m6_uv, aes(x = volume, y = log(FICM))) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("Month 6, UV") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Log base 2 structure volume") +
  ylab("Ln (FICM)")

m6_uv_plot



##############From another graphing project##################
#create scatter plot all patients
weight_change_plot <- ggplot(weight_change, aes(x = day, y = weight_kg, color = MRN, rm.NA=TRUE)) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_bw()
#  theme_classic() +
#  geom_rangeframe() +  
#  theme_tufte() +
ggtitle("Lung cancer patients -- weight change") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Day of Observation") +
  ylab("Weight (Kg)")

weight_change_plot

######################
#create scatter plot of first incomplete moment versus size
#filter by month
CLP_0001 <- weight_change %>%
  filter(MRN == "CLP-0001")

#create scatter plot and fitted line
CLP_0001CLP_0001 <- ggplot(CLP_0001, aes(x = day, y = weight_kg, rm.NA=TRUE)) +
  geom_point() +
  stat_smooth(method = lm) +
  theme_classic() +
  ggtitle("MRN -- CLP_0001") +
  theme(
    axis.line = element_line(size = 0.75, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(face = "bold", size = 12),
    axis.title.x = element_text(face = "bold", size = 14),
    axis.text.y = element_text(face = "bold", size = 12),
    axis.title.y = element_text(face = "bold", size = 14),
  ) +
  xlab("Day of Observation") +
  ylab("Weight (Kg)")

CLP_0001

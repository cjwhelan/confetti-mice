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


#ECDF_volume

glimpse(ECDF_volume)

#Now ECDF of volumes by UV category

#transform volume to natural log of volume

ECDF_log_volume <- ECDF_volume
ECDF_log_volume[, 4] <- log(ECDF_volume[4])
ECDF_log_volume

write.csv(ECDF_log_volume, file = "D:/confetti_mice/ECDF_log_volume.csv")

ECDF_log_volume_plot <- ggplot(ECDF_volume, aes(x = log(structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(size = 1.2, pad = FALSE)

ECDF_log_volume_plot

ECDF_log_volume_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=14), 
    legend.text=element_text(face = "bold", size=14),
    legend.position = c(0.8, 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Maximum volume (ln(cubic microns))") +
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs for UV vs No UV


#install.packages("dgof")

library(dgof)

a <- ECDF_log_volume %>%
  filter(treatment == "No UV")
a

b <- ECDF_log_volume %>%
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

#now with natural logged UV and No UV data
z1 <- a$structure_volume
z2 <- b$structure_volume
#u3 <- c$pH

ad.test(z1, z2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(z1), jitter(z2), method = "exact", dist = FALSE, Nsim = 10)


#################
#################

#now with volume transformed by log base 2

ECDF_log2_volume <- ECDF_volume
ECDF_log2_volume[, 4] <- log(ECDF_volume[4], 2)
ECDF_log2_volume

write.csv(ECDF_log2_volume, file = "D:/confetti_mice/ECDF_log2_volume.csv")


ECDF_log2_volume_plot <- ggplot(ECDF_log2_volume, aes(x = (structure_volume), color = treatment)) +
  labs(colour = "Treatment") +
  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.2, pad = FALSE)

ECDF_log2_volume_plot

ECDF_log2_volume_plot  +
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
  xlim(6, 30.0) +
  ylab("Cumulative Density") + 
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
##############
#Test for differences in ECDFs for UV and No UV


#install.packages("dgof")

library(dgof)

a <- ECDF_log2_volume %>%
  filter(treatment == "No UV")
a

b <- ECDF_log2_volume %>%
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
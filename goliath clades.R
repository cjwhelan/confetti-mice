##############
# Now for confetti mice
# Find only goliath clades by treatment and by month
# Create bar plot showing goliath clades by treatment and by month



rm(list = ls())

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

#log2(500000) = 18.93157
#log2(1000000) = 19.93157
#log2(2000000) = 20.93157


#filter by month
#start with month 1

m1 <- confetti_violin %>% 
  filter(month == "m1", ln2volume >= 20.932)

m1 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 2

m2 <- confetti_violin %>% 
  filter(month == "m2", ln2volume >= 20.932)

m2 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m3 <- confetti_violin %>% 
  filter(month == "m3", ln2volume >= 20.932)

m3 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 4

m4 <- confetti_violin %>% 
  filter(month == "m4", ln2volume >= 20.932)

m4 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m5 <- confetti_violin %>% 
  filter(month == "m5", ln2volume >= 20.932)

m5 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 6

m6 <- confetti_violin %>% 
  filter(month == "m6", ln2volume >= 20.932)

m6 %>%
  group_by(treatment) %>%
  summarise(n = n())
################
################

#filter by month
#start with month 1

m1 <- confetti_violin %>% 
  filter(month == "m1", ln2volume >= 21)

m1 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 2

m2 <- confetti_violin %>% 
  filter(month == "m2", ln2volume >= 21)

m2 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m3 <- confetti_violin %>% 
  filter(month == "m3", ln2volume >= 21)

m3 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 4

m4 <- confetti_violin %>% 
  filter(month == "m4", ln2volume >= 21)

m4 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m5 <- confetti_violin %>% 
  filter(month == "m5", ln2volume >= 21)

m5 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 6

m6 <- confetti_violin %>% 
  filter(month == "m6", ln2volume >= 21)

m6 %>%
  group_by(treatment) %>%
  summarise(n = n())

###############
###############
#Find number of clades with log base 2 cubic microns >= 22
#filter by month
#start with month 1

hotspots2<-read.csv( "D:\\confetti_mice\\hotspots\\hotspots_x_month.csv")

m1 <- hotspots2 %>% 
  filter(month == "m1", ln2volume >= 22)

m1 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 2

m2 <- hotspots2 %>% 
  filter(month == "m2", ln2volume >= 22)

m2 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m3 <- hotspots2 %>% 
  filter(month == "m3", ln2volume >= 22)

m3 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 4

m4 <- hotspots2 %>% 
  filter(month == "m4", ln2volume >= 22)

m4 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 1

m5 <- hotspots2 %>% 
  filter(month == "m5", ln2volume >= 22)

m5 %>%
  group_by(treatment) %>%
  summarise(n = n())

#filter by month
#start with month 6

m6 <- hotspots2 %>% 
  filter(month == "m6", ln2volume >= 22)

m6 %>%
  group_by(treatment) %>%
  summarise(n = n())



####create bar plots conditional on treatment X month

rm(list = ls())


#make an object
goliaths<-read.csv( "D:\\confetti_mice\\goliaths\\goliaths_22.csv")

glimpse(goliaths)

####barplot

#### from Cookbook for R
# ggplot(data=dat, aes(x=time, y=total_bill)) +
#    geom_bar(aes(fill=time), stat="identity")

goliaths_plot<-ggplot(data = goliaths, aes(x=month, y = count, fill = treatment)) +
      geom_col(width=.55, position=position_dodge((.65)) ) +
  labs(fill = "Treatment", face = "bold") +
  scale_fill_manual(values=c("blue", "red")) +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 32),
    axis.title.x = element_text(color = "black", face = "bold", size = 32),
    axis.text.y = element_text(color = "black", face = "bold", size = 32),
    axis.title.y = element_blank(),
    legend.title=element_text(size=26, face = "bold"),
    legend.text=element_text(size=24, face = "bold"),    
  ) + 
  xlab("Month") +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6)) +
  ylim(0,50) +
  ylab("Number clades > 22 (log base 2 cubic microns)")
  

goliaths_plot

######################################
####out of curiosity, try plotting as lines
####does not look so good

goliaths2 <- goliaths
goliaths2$month <- factor(goliaths2$month)
ggplot(data=goliaths2, aes(x=month, y=count, group=treatment, colour=treatment)) +
  geom_line() +
  geom_point()


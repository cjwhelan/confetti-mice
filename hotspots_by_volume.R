##############
# Now for confetti mice
# Find only goliath clades by treatment and by month
# Create bar plot showing goliath clades by treatment and by month



rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library(dplyr)

#D:\confetti_mice
#make an object
hotspots<-read.csv( "D:\\confetti_mice\\hotspots\\hotspots_by_volume.csv")

glimpse(hotspots)

#####eliminate month 0 and clades < base log 2 (13)

###ln base 2 13 - 13.5

hotspots13 <- hotspots %>% 
  filter(ln2volume >= 13 & ln2volume < 13.5 )


####group and summarize by treatment and hotspots
hotspots13sum <- hotspots13 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive

write.csv(hotspots13sum, file = "D:/confetti_mice/hotspots/hotspots13sum.csv")

###ln base 2 13.5 - 13.99
hotspots13_5 <- hotspots %>% 
  filter(ln2volume >= 13.5 & ln2volume < 14 )

hotspots13_5sum <- hotspots13_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive

write.csv(hotspots13_5sum, file = "D:/confetti_mice/hotspots/hotspots13_5sum.csv")


###ln base 2 14 - 14.49

hotspots14 <- hotspots %>% 
  filter(ln2volume >= 14 & ln2volume < 14.5 )

#### 14 to 14.49
hotspots14sum <- hotspots14 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive

write.csv(hotspots14sum, file = "D:/confetti_mice/hotspots/hotspots14sum.csv")

###ln base 2 14.5 - 14.99

hotspots14_5 <- hotspots %>% 
  filter(ln2volume >= 14.5 & ln2volume < 15)

hotspots14_5sum <- hotspots14_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive

write.csv(hotspots14_5sum, file = "D:/confetti_mice/hotspots/hotspots14_5sum.csv")

###ln base 2 15 - 15.49

hotspots15 <- hotspots %>% 
  filter(ln2volume >= 15 & ln2volume < 15.5)

hotspots15sum <- hotspots15 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots15sum, file = "D:/confetti_mice/hotspots/hotspots15sum.csv")

###ln base 2 15.5 - 15.99
hotspots15_5 <- hotspots %>% 
  filter(ln2volume >= 15.5 & ln2volume < 16)

hotspots15_5sum <- hotspots15_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive

write.csv(hotspots15_5sum, file = "D:/confetti_mice/hotspots/hotspots15_5sum.csv")

###ln base 2 16 - 16.49
hotspots16 <- hotspots %>% 
  filter(ln2volume >= 16 & ln2volume < 16.5)

hotspots16sum <- hotspots16 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots16sum, file = "D:/confetti_mice/hotspots/hotspots16sum.csv")

###ln base 2 16 - 16.49
hotspots16_5 <- hotspots %>% 
  filter(ln2volume >= 16.6 & ln2volume < 17)

hotspots16_5sum <- hotspots16_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots16_5sum, file = "D:/confetti_mice/hotspots/hotspots16_5sum.csv")

###ln base 2 17 - 17.59
hotspots17 <- hotspots %>% 
  filter(ln2volume >= 17 & ln2volume < 17.5)

hotspots17sum <- hotspots17 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots17sum, file = "D:/confetti_mice/hotspots/hotspots17sum.csv")

###ln base 2 17.5 - 17.99
hotspots17_5 <- hotspots %>% 
  filter(ln2volume >= 17.5 & ln2volume < 18)

hotspots17_5sum <- hotspots17_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots17_5sum, file = "D:/confetti_mice/hotspots/hotspots17_5sum.csv")


###ln base 2 18 - 18.49
hotspots18 <- hotspots %>% 
  filter(ln2volume >= 18 & ln2volume < 18.5)

hotspots18sum <- hotspots18 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots18sum, file = "D:/confetti_mice/hotspots/hotspots18sum.csv")

###ln base 2 18.5 - 18.99
hotspots18_5 <- hotspots %>% 
  filter(ln2volume >= 18.5 & ln2volume < 19)

hotspots18_5sum <- hotspots18 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots18_5sum, file = "D:/confetti_mice/hotspots/hotspots18_5sum.csv")

###ln base 2 19 - 19.49
hotspots19 <- hotspots %>% 
  filter(ln2volume >= 19 & ln2volume < 19.5)

hotspots19sum <- hotspots19 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots19sum, file = "D:/confetti_mice/hotspots/hotspots19sum.csv")

###ln base 2 19.5 - 19.99
hotspots19_5 <- hotspots %>% 
  filter(ln2volume >= 19.5 & ln2volume < 20)

hotspots19_5sum <- hotspots19 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots19_5sum, file = "D:/confetti_mice/hotspots/hotspots19_5sum.csv")

###ln base 2 20 - 20.49
hotspots20 <- hotspots %>% 
  filter(ln2volume >= 20 & ln2volume < 20.5)

hotspots20sum <- hotspots20 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots20sum, file = "D:/confetti_mice/hotspots/hotspots20sum.csv")

###ln base 2 20.5 - 20.99
hotspots20_5 <- hotspots %>% 
  filter(ln2volume >= 20.5 & ln2volume < 21)

hotspots20_5sum <- hotspots20_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots20_5sum, file = "D:/confetti_mice/hotspots/hotspots20_5sum.csv")

###ln base 2 21 - 21.49
hotspots21 <- hotspots %>% 
  filter(ln2volume >= 21 & ln2volume < 21.5)

hotspots21sum <- hotspots21 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots21sum, file = "D:/confetti_mice/hotspots/hotspots21sum.csv")

###ln base 2 21.5 - 21.99
hotspots21_5 <- hotspots %>% 
  filter(ln2volume >= 21.5 & ln2volume < 22)

hotspots21_5sum <- hotspots21_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots21_5sum, file = "D:/confetti_mice/hotspots/hotspots21_5sum.csv")

###ln base 2 22 - 22.49
hotspots22 <- hotspots %>% 
  filter(ln2volume >= 22 & ln2volume < 22.5)

hotspots22sum <- hotspots22 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots22sum, file = "D:/confetti_mice/hotspots/hotspots22sum.csv")

###ln base 2 22.5 - 22.99
hotspots22_5 <- hotspots %>% 
  filter(ln2volume >= 22.5 & ln2volume < 23)

hotspots22_5sum <- hotspots22_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots22_5sum, file = "D:/confetti_mice/hotspots/hotspots22_5sum.csv")

###ln base 2 23 - 23.49
hotspots23 <- hotspots %>% 
  filter(ln2volume >= 23 & ln2volume < 23.5)

hotspots23sum <- hotspots23 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots23sum, file = "D:/confetti_mice/hotspots/hotspots23sum.csv")

###ln base 2 23.5 - 23.99
hotspots23_5 <- hotspots %>% 
  filter(ln2volume >= 23.5 & ln2volume < 24)

hotspots23_5sum <- hotspots23_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots23_5sum, file = "D:/confetti_mice/hotspots/hotspots23_5sum.csv")

###ln base 2 24 - 24.59
hotspots24 <- hotspots %>% 
  filter(ln2volume >= 24 & ln2volume < 24.5)

hotspots24sum <- hotspots24 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots24sum, file = "D:/confetti_mice/hotspots/hotspots24sum.csv")

###ln base 2 24.5 - 24.99
hotspots24_5 <- hotspots %>% 
  filter(ln2volume >= 24.5 & ln2volume < 25)

hotspots24_5sum <- hotspots24_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots24_5sum, file = "D:/confetti_mice/hotspots/hotspots24_5sum.csv")

###ln base 2 25 - 25.49
hotspots25 <- hotspots %>% 
  filter(ln2volume >= 25 & ln2volume < 25.5)

hotspots25sum <- hotspots25 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots25sum, file = "D:/confetti_mice/hotspots/hotspots25sum.csv")

###ln base 2 25.5 - 25.99
hotspots25_5 <- hotspots %>% 
  filter(ln2volume >= 25.5 & ln2volume < 26)

hotspots25_5sum <- hotspots25_5 %>%
  group_by(treatment, hotspots) %>%
  summarise(n = n())

#write to hard drive
write.csv(hotspots25_5sum, file = "D:/confetti_mice/hotspots/hotspots25_5sum.csv")


#####create dot plot with ggplot2
#make an object
hotspots<-read.csv( "D:\\confetti_mice\\hotspots\\hotspots_x_volume.csv")

UVhotspots <- hotspots %>%
  filter(treatment == "UV")

ggplot(UVhotspots, aes(x = ln2volume, y = hotspots)) +
  geom_point(size = 5, color = "red") +
  theme_classic() +
  theme(
    plot.margin = margin(0.5, 0.75, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20),
    legend.title=element_text(size=26, face = "bold"),
    legend.text=element_text(size=24, face = "bold"),    
  ) + 
  xlab("log base 2(cubic microns)") +
  scale_x_continuous(breaks=c(12, 14, 16, 18, 20,  22, 24, 26)) +
  ylab("Number Micro-clumps")


No_UVhotspots <- hotspots %>%
  filter(treatment == "No UV")

ggplot(No_UVhotspots, aes(x = ln2volume, y = hotspots)) +
  geom_point(size = 5, color = "blue") +
  theme_classic() +
  theme(
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.x = element_text(color = "black", face = "bold", size = 20),
    axis.text.y = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(face = "bold", size = 20),
    legend.title=element_text(size=26, face = "bold"),
    legend.text=element_text(size=24, face = "bold"),    
  ) + 
  xlab("log base 2(cubic microns)") +
  xlim(12,26) +
  scale_x_continuous(breaks=c(12, 14, 16, 18, 20, 22, 24, 26)) +
  ylab("Number Micro-clumps") +
  ylim(0,25)


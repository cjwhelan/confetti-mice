####histogram of mutation frequencies


rm(list=ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(hrbrthemes)
library(ggExtra)
library(ggthemes)

#Make truncated axes
library(ggh4x)


# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/1_OneNum.csv", header=TRUE)

# plot
p <- data %>%
  filter( price<300 ) %>%
  ggplot( aes(x=price)) +
  geom_histogram(binwidth=3, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 3") +
  theme_ipsum() +
  theme(
    plot.title = element_text(size=15)
  )

p


#D:\confetti_mice\hotspots
#make an object:
mutations_histogram<-read.csv( "D:\\confetti_mice\\mutation_frequency\\443_Goliath_UV.csv") 

#check:
glimpse(mutations_histogram)

histogram1 <- ggplot(mutations_histogram, aes(x = frequency)) +
  geom_rangeframe(sides = "bl") +
  geom_histogram(binwidth = 0.002, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
    theme_tufte() +
  guides(x = "axis_truncated", y = "axis_truncated")  + 
  theme(
      axis.line = element_line(linewidth = 1))+
  scale_y_continuous(limits = c(0, 8)) + 
  scale_x_continuous(limits = c(0.02, 0.19))

histogram1

histogram <- ggplot(mutations_histogram, aes(x = frequency)) + 
  geom_histogram(bins=40, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_classic() 

histogram

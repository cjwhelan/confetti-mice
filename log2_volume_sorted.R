#create ECDF plots for confetti mice experiment
#working with UV and No UV separately, sorted by log base 2 volume
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
UV_mice_allmonths<-read.csv( "D:\\confetti_mice\\UV_mice_allmonths.csv")


#ECDF_volume

glimpse(UV_mice_allmonths)

#add column of ECDF function

fun.ecdf<-ecdf(UV_mice_allmonths$structure_volume) #make a function that applies ecdf to your data

my.ecdf<-fun.ecdf(sort(UV_mice_allmonths$structure_volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 

UV_mice_allmonths$distribution_volume<-my.ecdf #add the cumulative probabilities to your data frame

UV_mice_allmonths

write.csv(UV_mice_allmonths, file = "D:/confetti_mice/UV_mice_allmonths_ecdf.csv")

#############################
#Now with No UV


rm(list = ls())

No_UV_mice_allmonths<-read.csv( "D:\\confetti_mice\\No_UV_mice_allmonths.csv")


#ECDF_volume

glimpse(No_UV_mice_allmonths)

#add column of ECDF function

fun.ecdf<-ecdf(No_UV_mice_allmonths$structure_volume) #make a function that applies ecdf to your data

my.ecdf<-fun.ecdf(sort(No_UV_mice_allmonths$structure_volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 

No_UV_mice_allmonths$distribution_volume<-my.ecdf #add the cumulative probabilities to your data frame

No_UV_mice_allmonths

write.csv(No_UV_mice_allmonths, file = "D:/confetti_mice/No_UV_mice_allmonths_ecdf.csv")

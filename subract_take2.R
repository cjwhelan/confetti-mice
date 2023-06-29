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
#library(ggh4x)

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

#fun.ecdf<-ecdf(bytreatment$structure_volume) #make a function that applies ecdf to your data

#my.ecdf<-fun.ecdf(sort(bytreatment$structure_volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 

#first for structures arising with no UV treatment
fun.ecdf<-ecdf(a$structure_volume)

fun.ecdf

no_uv_ecdf <- fun.ecdf((sort(a$structure_volume)))

no_uv_ecdf

#now for structures arising with UV treatment

fun.ecdf<-ecdf(b$structure_volume)

uv_ecdf <- fun.ecdf(sort(b$structure_volume))

uv_ecdf


ecdf_subtract <- uv_ecdf - no_uv_ecdf

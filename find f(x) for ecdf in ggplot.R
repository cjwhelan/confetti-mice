#find ecdf for each month stratified by treatment

rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library("dplyr")

#D:\confetti_mice\mice all months data

#First month 1 UV and no UV
#make an object
m1_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m1_UV_allmonths.csv")

glimpse(m1_UV_allmonths)


# plot the result
ggplot(m1_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 

#Now calculate ecdf(volume) and add column to df and write to hard drive
fun.ecdf_UV <- ecdf(m1_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m1_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m1_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m1_UV_allmonths, file = "D:/confetti_mice/mice all months data/m1_UV_allmonths_ecdf.csv")

########################################
#m1 and no UV
#make an object

rm(list = ls())

m1_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m1_noUV_allmonths.csv")

glimpse(m1_noUV_allmonths)


# plot the result
ggplot(m1_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 

#Now calculate ecdf(volume) and add column to df and write to hard drive
fun.ecdf_UV <- ecdf(m1_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m1_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m1_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m1_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m1_noUV_allmonths_ecdf.csv")


#Now month 2 UV and no UV
#make an object
rm(list = ls())

m2_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m2_UV_allmonths.csv")

glimpse(m2_UV_allmonths)


# plot the result
ggplot(m2_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m2_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m2_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m2_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m2_UV_allmonths, file = "D:/confetti_mice//mice all months data//m2_UV_allmonths_ecdf.csv")

########################################
#m2 and no UV
#make an object

rm(list = ls())

m2_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m2_noUV_allmonths.csv")

glimpse(m2_noUV_allmonths)


# plot the result
ggplot(m2_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m2_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m2_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m2_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m2_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m2_noUV_allmonths_ecdf.csv")

########################################
#Now month 3 UV and no UV
#make an object
rm(list = ls())

m3_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data//m3_UV_allmonths.csv")

glimpse(m3_UV_allmonths)


# plot the result
ggplot(m3_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m3_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m3_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m3_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m3_UV_allmonths, file = "D:/confetti_mice/mice all months data/m3_UV_allmonths_ecdf.csv")

#####################################


#m3 and no UV
#make an object

rm(list = ls())

m3_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m3_noUV_allmonths.csv")

glimpse(m3_noUV_allmonths)


# plot the result
ggplot(m3_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m3_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m3_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m3_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m3_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m3_noUV_allmonths_ecdf.csv")

########################################
#Now month 4 UV and no UV
#make an object
rm(list = ls())

m4_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m4_UV_allmonths.csv")

glimpse(m4_UV_allmonths)


# plot the result
ggplot(m4_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m4_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m4_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m4_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m4_UV_allmonths, file = "D:/confetti_mice/mice all months data/m4_UV_allmonths_ecdf.csv")




#m4 and no UV
#make an object

rm(list = ls())

m4_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m4_noUV_allmonths.csv")

glimpse(m4_noUV_allmonths)


# plot the result
ggplot(m4_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m4_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m4_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m4_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m4_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m4_noUV_allmonths_ecdf.csv")

########################################
#Now month 5 UV and no UV
#make an object
rm(list = ls())

m5_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m5_UV_allmonths.csv")

glimpse(m5_UV_allmonths)


# plot the result
ggplot(m5_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m5_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m5_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m5_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m5_UV_allmonths, file = "D:/confetti_mice/mice all months data/m5_UV_allmonths_ecdf.csv")

##########################################
##########################################

#m5 and no UV
#make an object

rm(list = ls())

m5_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m5_noUV_allmonths.csv")

glimpse(m5_noUV_allmonths)


# plot the result
ggplot(m5_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m5_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m5_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m5_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m5_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m5_noUV_allmonths_ecdf.csv")


########################################
#Now month 6 UV and no UV
#make an object
rm(list = ls())

m6_UV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m6_UV_allmonths.csv")

glimpse(m6_UV_allmonths)


# plot the result
ggplot(m6_UV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m6_UV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m6_UV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m6_UV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m6_UV_allmonths, file = "D:/confetti_mice/mice all months data/m6_UV_allmonths_ecdf.csv")


#m6 and no UV
#make an object

rm(list = ls())

m6_noUV_allmonths<-read.csv( "D:\\confetti_mice\\mice all months data\\m6_noUV_allmonths.csv")

glimpse(m6_noUV_allmonths)


# plot the result
ggplot(m6_noUV_allmonths, aes(x = ln2volume)) + 
  stat_ecdf() 



fun.ecdf_UV <- ecdf(m6_noUV_allmonths$ln2volume)

my.ecdf <- fun.ecdf_UV(sort(m6_noUV_allmonths$ln2volume)) # apply that function to a sorted list of your data to give the cumulative probabilities 
m6_noUV_allmonths$density <- my.ecdf #add the cumulative probabilities to your data frame

write.csv(m6_noUV_allmonths, file = "D:/confetti_mice/mice all months data/m6_noUV_allmonths_ecdf.csv")



#the script below is still working as a step function and does
#not "interpolate" between steps

# create ecdf function
#e = ecdf(x$col1)

# plot the result
#ggplot(x, aes(col1)) + 
#  stat_ecdf() +
#  geom_label(aes(x = -1, y = e(-1)), 
#             label = e(-1))
ggplot(m1_UV_allmonths, aes(x = volume)) + 
  stat_ecdf() +
  geom_label(aes(x = 13.6912754699, y = fun.ecdf_UV(13.6912754699)), 
             label = fun.ecdf_UV(13.6912754699))

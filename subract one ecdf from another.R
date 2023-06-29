#subtract one ecdf curve from another

rm(list = ls())

c <- runif(10000)
k <- rnorm(15000,0.5,0.5)
plot(ecdf(k))
lines(ecdf(c), col='red')

decdf <- function(x, baseline, treatment)  ecdf(baseline)(x) - ecdf(treatment)(x)

curve(decdf(x,c,k), from=min(c,k), to=max(c,k))

#add two ecdf curves

aecdf <- function(x, baseline, treatment)  ecdf(baseline)(x) + ecdf(treatment)(x)

curve(aecdf(x,a,b), from=min(a,b), to=max(a,b))

##############
#Now for confetti mice

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

#library(dgof)

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

#vec1 <- data$x1                         # $-Operator
#vec1

n <- a$structure_volume    ###no uv

n

u <- b$structure_volume    ###uv

u

plot(ecdf(n))
lines(ecdf(u), col='red')



decdf <- function(x, baseline, treatment)  ecdf(baseline)(x) - ecdf(treatment)(x)

curve(decdf(x,n,u), from=min(n,u), to=max(n,u))




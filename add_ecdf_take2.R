##############
#another try at adding/subtracting ecdf's
#first clear everything


rm(list = ls())

#library(dplyr)
library(ggplot2)
library(tidyr)
library("dplyr")


#Make truncated axes
library(ggh4x)

#D:\confetti_mice
#make an object
mouse3_UV<-read.csv( "D:\\confetti_mice\\ecdf_subtraction\\mouse3_m1_UV.csv")

mouse3_noUV<-read.csv( "D:\\confetti_mice\\ecdf_subtraction\\mouse3_m1_noUV.csv")

y1 <- mouse3_UV$density

x1 <- mouse3_UV$volume

f1 <- stepfun(x = x1, y = y1)

y1 <- c(0, 1, 2, 0)
x1 <- c(1, 2, 3)
f1 <- stepfun(x = x1, y = y1)
par(mfrow = c(2, 2))
plot(f1)

y2 <- c(0, 1, 0)
x2 <- c(1.5, 2.5)
f2 <- stepfun(x = x2, y = y2)
plot(f2)

fs <- function(x, f1, f2) {
  return(f1(x) + f2(x))
}

f_subtract <- function(x, f1, f2) {
  return(f1(x) - f2(x))
}

fm <- function(x, f1, f2) {
  return(f1(x) * f2(x))
}

x <- seq(0, 4, length.out = 100)
plot(x, fs(x, f1, f2), type = "s", main = "Sum f1+f2")
plot(x, f_subtract(x, f1, f2), type = "s", main = "Sum f1-f2")
plot(x, fm(x, f1, f2), type = "s", main = "Multiplication f1*f2")

par(mfrow = c(1, 1))

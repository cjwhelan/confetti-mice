
#install.packages("remotes")
#remotes::install_github("robjohnnoble/demonanalysis")

#calculate first incomplete moment

rm(list = ls())

library(demonanalysis)
library(tidyr)
library(dplyr)


#df_test <- data.frame(size = 1:30, count = exp(-(1:30)))

#first_inc_moment(df_test$size, df_test$count, 2)                      


#make an object
first_inc_mom <- read.csv("D:\\confetti_mice\\first_inc_mom.csv")

glimpse(first_inc_mom)

#filter by month and treatment

####################
#####  month 1 and no uv

m1_no_uv <- first_inc_mom %>% 
  filter(month == "m1", treatment == "no_uv")

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 0)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 1)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 2)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 3)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 4)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 5)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 6)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 7)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 8)

first_inc_moment(m1_no_uv$volume, m1_no_uv$proportion, 9)

####month 1 & uv

#filter by month and treatment

m1_uv <- first_inc_mom %>% 
  filter(month == "m1", treatment == "uv")

first_inc_moment(m1_uv$volume, m1_uv$proportion, 0)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 1)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 2)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 3)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 4)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 5)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 6)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 7)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 8)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 9)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 10)

first_inc_moment(m1_uv$volume, m1_uv$proportion, 11)

#filter by month and treatment

######month 2 & no uv

m2_no_uv <- first_inc_mom %>% 
  filter(month == "m2", treatment == "no_uv")

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 0)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 1)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 2)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 3)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 4)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 5)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 6)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 7)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 8)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 9)

first_inc_moment(m2_no_uv$volume, m2_no_uv$proportion, 10)

####month 2 & uv

#filter by month and treatment

m2_uv <- first_inc_mom %>% 
  filter(month == "m2", treatment == "uv")

first_inc_moment(m2_uv$volume, m2_uv$proportion, 0)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 1)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 2)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 3)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 4)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 5)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 6)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 7)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 8)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 9)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 10)

first_inc_moment(m2_uv$volume, m2_uv$proportion, 11)

######month 3 & no uv

m3_no_uv <- first_inc_mom %>% 
  filter(month == "m3", treatment == "no_uv")

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 0)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 1)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 2)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 3)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 4)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 5)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 6)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 7)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 8)

first_inc_moment(m3_no_uv$volume, m3_no_uv$proportion, 9)


####month 3 & uv

#filter by month and treatment

m3_uv <- first_inc_mom %>% 
  filter(month == "m3", treatment == "uv")

first_inc_moment(m3_uv$volume, m3_uv$proportion, 0)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 1)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 2)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 3)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 4)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 5)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 6)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 7)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 8)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 9)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 10)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 11)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 12)

first_inc_moment(m3_uv$volume, m3_uv$proportion, 13)



#month 4 & no uv

m4_no_uv <- first_inc_mom %>% 
  filter(month == "m4", treatment == "no_uv")

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 0)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 1)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 2)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 3)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 4)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 5)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 6)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 7)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 8)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 9)

first_inc_moment(m4_no_uv$volume, m4_no_uv$proportion, 10)


####month 4 & uv

#filter by month and treatment

m4_uv <- first_inc_mom %>% 
  filter(month == "m4", treatment == "uv")

first_inc_moment(m4_uv$volume, m4_uv$proportion, 0)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 1)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 2)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 3)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 4)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 5)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 6)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 7)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 8)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 9)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 10)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 11)

first_inc_moment(m4_uv$volume, m4_uv$proportion, 12)

#month 5 & no uv

m5_no_uv <- first_inc_mom %>% 
  filter(month == "m5", treatment == "no_uv")

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 0)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 1)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 2)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 3)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 4)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 5)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 6)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 7)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 8)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 9)

first_inc_moment(m5_no_uv$volume, m5_no_uv$proportion, 10)


####month 5 & uv

#filter by month and treatment

m5_uv <- first_inc_mom %>% 
  filter(month == "m5", treatment == "uv")

first_inc_moment(m5_uv$volume, m5_uv$proportion, 0)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 1)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 2)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 3)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 4)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 5)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 6)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 7)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 8)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 9)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 10)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 11)

first_inc_moment(m5_uv$volume, m5_uv$proportion, 12)


##################################
#####month 6 & no uv

m6_no_uv <- first_inc_mom %>% 
  filter(month == "m6", treatment == "no_uv")

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 0)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 1)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 2)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 3)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 4)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 5)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 6)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 7)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 8)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 9)

first_inc_moment(m6_no_uv$volume, m6_no_uv$proportion, 10)


####month 5 & uv

#filter by month and treatment

m6_uv <- first_inc_mom %>% 
  filter(month == "m6", treatment == "uv")

first_inc_moment(m6_uv$volume, m6_uv$proportion, 0)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 1)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 2)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 3)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 4)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 5)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 6)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 7)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 8)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 9)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 10)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 11)

first_inc_moment(m6_uv$volume, m6_uv$proportion, 12)



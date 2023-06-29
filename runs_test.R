####running runs randomness tests
# package DescTools will run on dichotomous variable (e.g., +/-)
# package randtests runs on real numbers


rm(list=ls())


library(DescTools)

# x will be coerced to a dichotomous variable
x <- c("S","S", "T", "S", "T","T","T", "S", "T")
RunsTest(x)

x1 <- c('H','T','T','H','H','H','T','T','T','T','T','T','T','H','H',
        'H','T','H','T','H','H','H','T','H','H','H','T','H','T','H')

x1

RunsTest(x1)

x <- c(13, 3, 14, 14, 1, 14, 3, 8, 14, 17, 9, 14, 13, 2, 16, 1, 3, 12, 13, 14)
RunsTest(x)
# this will be treated as
RunsTest(x > median(x))

plot( (x < median(x)) - 0.5, type="s", ylim=c(-1,1) )
abline(h=0)


x <- c(31,23,36,43,51,44,12,26,43,75,2,3,15,18,78,24,13,27,86,61,13,7,6,8)
RunsTest(x, exact=TRUE)       # exact probability

RunsTest(x, exact=FALSE)      # normal approximation

# if y is not NULL, the Wald-Wolfowitz-Test will be performed
A <- c(35,44,39,50,48,29,60,75,49,66)
B <- c(17,23,13,24,33,21,18,16,32)

RunsTest(A, B, exact=TRUE)
RunsTest(A, B, exact=FALSE)


library(randtests)

x2 <- c(1.0,2.0,2.0,1.0,1.0,1.0,2.0,2.0,2.0,2.0,
        2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,2.0,1.0,1.0,1.0,2.0,
        1.0,1.0,1.0,2.0,1.0,2.0,1.0)

x2

RunsTest(x2)

set.seed(0.2)

jitter(x2)

median(jitter(x2))

runs.test(jitter(x2))



earthden <- c(5.36, 5.29, 5.58, 5.65, 5.57, 5.53, 5.62, 5.29, 5.44, 5.34, 5.79,
              5.10, 5.27, 5.39, 5.42, 5.47, 5.63, 5.34, 5.46, 5.30, 5.75, 5.68, 5.85)

earthden

runs.test(earthden)

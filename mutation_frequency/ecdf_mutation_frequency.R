#####create ecdf graphs of mutation frequencies for goliath clades

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

#Make truncated axes
library(ggh4x)

#D:\confetti_mice
#D:\confetti_mice\mutation_frequency
#make an object
goliath006_UV <-read.csv( "D:\\confetti_mice\\mutation_frequency\\006_Goliath_UV.csv")

glimpse(goliath006_UV)


goliath006_UV_plot <- ggplot(goliath006_UV, aes(x = (frequency))) +
#  labs(colour = "Treatment") +
#  scale_colour_discrete(labels = c("No UV", "UV")) +
#  theme(legend.title=element_text(size=14), 
#        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  xlim(0,1)
#  scale_color_manual(values = c("blue", "firebrick2"))

goliath006_UV_plot

#make an object
goliath444_UV <-read.csv( "D:\\confetti_mice\\mutation_frequency\\goliath444_UV.csv")

glimpse(goliath444_UV)


goliath444_UV_plot <- ggplot(goliath444_UV, aes(x = (frequency))) +
  #  labs(colour = "Treatment") +
  #  scale_colour_discrete(labels = c("No UV", "UV")) +
  #  theme(legend.title=element_text(size=14), 
  #        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  xlim(0,1)
#  scale_color_manual(values = c("blue", "firebrick2"))

goliath444_UV_plot

#####################
###put multiple goliaths on single plot

#make an object
ecdf_mutation_frequency <-read.csv( "D:\\confetti_mice\\mutation_frequency\\ecdf_mutation_frequency.csv")

glimpse(ecdf_mutation_frequency)

####here are some colors
"#69b3a2"  "firebrick2" "#B87333" 'darkorange' 'cadetblue1' '#BDB76B'
'red' 'black' 'blue' 'purple' 'cornflowerblue' 'darkorchid1' '#006400'
#######


ecdf_mutation_frequency_plot <- ggplot(ecdf_mutation_frequency, 
                                       aes(x = frequency, color = goliath)) +
    labs(colour = "Goliath") +
    theme(legend.title=element_text(size=36), 
          legend.text=element_text(size=36)) +
  stat_ecdf(geom = "line", linewidth = 2.55, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2", "#B87333", 'purple'),
                     breaks=c("g006", "g444", "g445", "g446"),
                     labels=c("006", "444", "445", "446"))

ecdf_mutation_frequency_plot

ecdf_mutation_frequency_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
#  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=36), 
    legend.text=element_text(face = "bold", size=36),
    legend.position = c(0.8, 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(face = "bold", size = 36)
  ) + 
  xlab("Mutation frequency") +
  xlim(0, 1) +
    ylab("Cumulative density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

#######
###An alternative frequency plot

#make an object
ecdf_mutation_frequency <-read.csv( "D:\\confetti_mice\\mutation_frequency\\ecdf_mutation_frequency.csv")

glimpse(ecdf_mutation_frequency)


ecdf_mutation_frequency_plot <- ggplot(ecdf_mutation_frequency, 
                               aes(x = frequency, after_stat(density), color = goliath)) +
  labs(colour = "Goliath") +
  #  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  geom_freqpoly(binwidth = 0.02, linewidth = 1.25) +
  #  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2", "#B87333", 'purple'),
                     breaks=c("g006", "g444", "g445", "g446"),
                     labels=c("006", "444", "445", "446"))

ecdf_mutation_frequency_plot

ecdf_mutation_frequency_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  #  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=14), 
    legend.text=element_text(face = "bold", size=14),
    #    legend.position = c(0.8, 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Mutation frequency") +
  xlim(0, 1) +
  ylim(0, 20) +
  ylab("Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

##############
#Test for differences in ECDFs among the different goliaths
# goliaths #006, #444, #445, #446


#install.packages("dgof")

library(dgof)

a <- ecdf_mutation_frequency %>%
  filter(goliath == "g444")
a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g445")
b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

#######
a <- ecdf_mutation_frequency %>%
  filter(goliath == "g444")
#a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g446")
#b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)


####
a <- ecdf_mutation_frequency %>%
  filter(goliath == "g444")
#a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g006")
#b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

#######
a <- ecdf_mutation_frequency %>%
  filter(goliath == "g445")
#a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g446")
#b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)


####
a <- ecdf_mutation_frequency %>%
  filter(goliath == "g445")
#a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g006")
#b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

####
a <- ecdf_mutation_frequency %>%
  filter(goliath == "g446")
#a

b <- ecdf_mutation_frequency %>%
  filter(goliath == "g006")
#b


# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

###### 3 months #######
#make an object
ecdf_mutation_frequency3 <-read.csv( "D:\\confetti_mice\\mutation_frequency\\ecdf_mutation_frequency_3m.csv")

glimpse(ecdf_mutation_frequency3)

#### here are some colors
"#69b3a2"  "firebrick2" "#B87333" 'darkorange' 'cadetblue1' '#BDB76B'
'red' 'black' 'blue' 'purple' 'cornflowerblue' 'darkorchid1' '#006400'
#######


ecdf_mutation_frequency_plot <- ggplot(ecdf_mutation_frequency3, 
                                       aes(x = frequency, color = goliath)) +
  labs(colour = "Goliath") +
  #  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=36), 
        legend.text=element_text(size=36)) +
  stat_ecdf(geom = "line", linewidth = 2.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2", "#B87333", 'purple', 'darkorange',
                                      '#69b3a2', 'black', 'cornflowerblue'),
                                      breaks=c("g443", "g445", "g9154_1",
                                               "g9154_2", "g9156", "g9157", "g9158", "g9159"),
                     labels=c("443", "445", "9154_1", "9154_2", "9156",
                              "9157", "9158", "9159"))

ecdf_mutation_frequency_plot

ecdf_mutation_frequency_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  #  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 36),
    legend.title=element_text(face = "bold", size=36), 
    legend.text=element_text(face = "bold", size=36),
    legend.position = c(0.8, 0.45),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(face = "bold", size = 36)
  ) + 
  xlab("Mutation frequency") +
  xlim(0, 1) +
  ylab("Cumulative Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

#####
### plot density with alternative graph type

#make an object
ecdf_mutation_frequency3 <-read.csv( "D:\\confetti_mice\\mutation_frequency\\ecdf_mutation_frequency_3m.csv")

glimpse(ecdf_mutation_frequency3)

ecdf_mutation_frequency_plot <- ggplot(ecdf_mutation_frequency3, 
                      aes(x = frequency, after_stat(density), color = goliath)) +
  labs(colour = "Goliath") +
  #  scale_colour_discrete(labels = c("No UV", "UV")) +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  geom_freqpoly(binwidth = 0.02, linewidth = 1.25) +
  #  stat_ecdf(geom = "point", size = 1.5, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2", "#B87333", 'purple', 'darkorange',
                                '#69b3a2', 'black', 'cornflowerblue'),
                                breaks=c("g443", "g445", "g9154_1",
                                "g9154_2", "g9156", "g9157", "g9158", "g9159"),
                     labels=c("443", "445", "9154_1", "9154_2", "9156",
                              "9157", "9158", "9159"))

ecdf_mutation_frequency_plot

ecdf_mutation_frequency_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  #  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=14), 
    legend.text=element_text(face = "bold", size=14),
    #    legend.position = c(0.8, 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Mutation frequency") +
  xlim(0, 1) +
  ylim(0, 20) +
  ylab("Density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))


#Test for differences in ECDFs among the different goliaths at 3 months
# goliaths #443, #445, #9154_1, #9154_2, #9156, #9157, #9158, #9159 


#install.packages("dgof")

library(dgof)

a <- ecdf_mutation_frequency3 %>%
  filter(goliath == "g9158")
#a

b <- ecdf_mutation_frequency3 %>%
  filter(goliath == "g9159")
#b

# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 10)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 10)

#####################
###compare 3 months with 7 months

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)

#Make truncated axes
library(ggh4x)


#make an object
ecdf_mutation_frequency <-read.csv( "D:\\confetti_mice\\mutation_frequency\\ecdf_mutation_frequency_3m7m.csv")

glimpse(ecdf_mutation_frequency)

####here are some colors
"#69b3a2"  "firebrick2" "#B87333" 'darkorange' 'cadetblue1' '#BDB76B'
'red' 'black' 'blue' 'purple' 'cornflowerblue' 'darkorchid1' '#006400'
#######


ecdf_mutation_frequency_plot <- ggplot(ecdf_mutation_frequency, 
                                       aes(x = frequency, color = month)) +
  labs(colour = "Month") +
  theme(legend.title=element_text(size=14), 
        legend.text=element_text(size=14)) +
  stat_ecdf(geom = "line", linewidth = 1.25, pad = FALSE) +
  scale_color_manual(values = c("blue", "firebrick2"),
                     breaks=c("m3", "m7"),
                     labels=c("3 months", "7 months"))

ecdf_mutation_frequency_plot

ecdf_mutation_frequency_plot  +
  theme_minimal() +
  guides(x = "axis_truncated", y = "axis_truncated")  +
  #  ggtitle("Cumulative Volume Distribution by UV Exposure (cubic microns)") +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title=element_text(face = "bold", size=14), 
    legend.text=element_text(face = "bold", size=14),
    legend.position = c(0.8, 0.2),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 14),
    axis.title.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.y = element_text(face = "bold", size = 16)
  ) + 
  xlab("Mutation frequency") +
  xlim(0, 1) +
  ylab("Cumulative density") +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

####Test for differences in ECDFs between 3 month and 7 month goliaths
####

#install.packages("dgof")

library(dgof)

a <- ecdf_mutation_frequency %>%
  filter(month == "m3")
#a

b <- ecdf_mutation_frequency %>%
  filter(month == "m7")
#b

# plotting the result
# visualization
plot(ecdf(a$frequency),
     col = "blue")
plot(ecdf(b$frequency),
     add = TRUE,
     lty = "dashed",
     col = "red")

ks.test(a$frequency, b$frequency)

ks.test(jitter(a$frequency), jitter(b$frequency))

#perform k-sample Anderson-Darling test

library(kSamples)

#now with pre-post pH data
u1 <- a$frequency
u2 <- b$frequency
#u3 <- c$pH

ad.test(u1, u2, method = "exact", dist = FALSE, Nsim = 1000)

ad.test(jitter(u1), jitter(u2), method = "exact", dist = FALSE, Nsim = 1000)

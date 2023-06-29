
# point plot of skew and kurtosis for confetti mice project
#and 95% confidence limits
# by No UV vs UV treatment stratified from month  1-6


rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)
library(gcookbook)


#make an object:
skew_kurtosis<-read.csv( "D:\\confetti_mice\\skew_kurtosis\\skew_kurtosis.csv")

#check:
glimpse(skew_kurtosis)

####make month a factor
skew_kurtosis$month <- as.factor(skew_kurtosis$month) 

glimpse(skew_kurtosis)

skew_plot <- ggplot(skew_kurtosis, aes(x = month, y = skewness, color = treatment)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = skewness - skew_se,
                     ymax = skewness + skew_se), 
                size = 1.2, width = 0.5) +
  labs(color = "Treatment", face = "bold") +
  theme_classic()+
  scale_color_manual(values = c("blue", "firebrick2")) +
#  ggtitle("Skewness") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
#    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(linewidth = 1.25, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36),
    legend.title=element_text(size=36, face = "bold"),
    legend.text=element_text(size=36, face = "bold")
  ) + 
  xlab("Month") +
  ylab("Clade Skewness") +
  geom_hline(aes(yintercept=0.0), color="black", linetype="dashed", size = 1.25)
  


skew_plot


kurtosis_plot <- ggplot(skew_kurtosis, aes(x = month, y = kurtosis, color = treatment)) +
  geom_point(size = 6) +
  geom_errorbar(aes(ymin = kurtosis - kurtosis_se,
                     ymax = kurtosis + kurtosis_se), 
                size = 1.2, width = 0.5) +
  labs(color = "Treatment", face = "bold") +
  theme_classic()+
  scale_color_manual(values = c("blue", "firebrick2")) +
#  ggtitle("Kurtosis") +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
#    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(linewidth = 1.25, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36),
    legend.title=element_text(size=36, face = "bold"),
    legend.text=element_text(size=36, face = "bold")
  ) + 
  xlab("Month") +
  ylab("Clade Kurtosis") +
  geom_hline(aes(yintercept=0.0), color="black", linetype="dashed", size = 1.25) 


kurtosis_plot


theme(legend.title = element_text(color = "blue", size = 10),
      legend.text = element_text(color = "red"))

#filter by month

m1 <- skew_kurtosis %>% 
  filter(month == "m1")


m1_skew_plot <- ggplot(m1, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m1_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 1") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)

#filter month 2

m2 <- skew_kurtosis %>% 
  filter(month == "m2")

m2_skew_plot <- ggplot(m2, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m2_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 2") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)

#filter month 3

m3 <- skew_kurtosis %>% 
  filter(month == "m3")

m3_skew_plot <- ggplot(m3, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m3_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 3") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)


#filter month 4

m4 <- skew_kurtosis %>% 
  filter(month == "m4")

m4_skew_plot <- ggplot(m4, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m4_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 4") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)

#filter month 5

m5 <- skew_kurtosis %>% 
  filter(month == "m5")

m5_skew_plot <- ggplot(m5, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m5_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 5") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)

#filter month 6

m6 <- skew_kurtosis %>% 
  filter(month == "m6")

m6_skew_plot <- ggplot(m6, aes(x = treatment, y = skewness)) +
  geom_point(size = 4) +
  geom_errorbar((aes(ymin = skewness - skew_se, 
                     ymax = skewness + skew_se))) 
#  scale_y_continuous(breaks = c(0, 0.1, 0.162, 0.2, 0.3))
m6_skew_plot + 
  theme_minimal() +
  theme(legend.position="FALSE") +
  scale_fill_manual(values = c("blue", "firebrick2")) +
  ggtitle("Month 6") +
  theme(
    axis.text.x = element_text(face = "bold", size = 12)) +
  theme(
    axis.text.y = element_text(face = "bold", size = 12)) +  
  theme(axis.title.x = element_text(face = "bold", size = 12)) +
  ylab("Skewness") +
  theme(axis.title.y = element_text(face = "bold", size = 14)
  ) +
  ylim(0, 1.5)



grid.arrange(m1_skew_plot, m2_skew_plot, m3_skew_plot, m4_skew_plot, 
             m5_skew_plot, m6_skew_plot,  ncol=6)

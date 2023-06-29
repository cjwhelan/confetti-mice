####try bubble plot

rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)


#make object
#### for density = 0.89 to density = 0.99


bubble_89_99<-read.csv( "D:\\confetti_mice\\bubble data\\bubble_89_99.csv")

glimpse(bubble_89_99)

####make month a factor
bubble_89_99$month <- as.factor(bubble_89_99$month) 

glimpse(bubble_89_99)

#ggplot(mpg, aes(x=cty, y=hwy, size = pop)) +geom_point(alpha=0.7)

bubble_89_99_plot <- ggplot(bubble_89_99, aes(x = density, y = dx, size = mean_volume, fill = month)) +
  geom_point(shape=21, alpha = 0.75, color="black", stroke = 1.1) +
  scale_size_area(breaks = c(100000,500000,1000000,2500000,5000000,10000000),
                        name = "Mean Volume\n(cubic microns)", max_size = 20) +
  theme_bw() +
  scale_fill_manual(values = c("blue", "brown", "forest green", "dark orange1", 
                                     "red", "dark violet")) +
                                       #  ggtitle("Volume difference between UV \nand No UV treatments by month") +
  theme(
    #    plot.title = element_text(face = "bold", size = 30),
    plot.margin = margin(0.1, 1.5, 0.5, 1.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36)
  ) + 
  xlab("Cumulative density") +
  ylab(expression(bold(atop(log["2"]~"volume difference", ("UV - No UV"))))) +
  ylim(-1, 3.0) +
  scale_x_continuous(breaks = c(0.89, 0.91, 0.93, 0.95, 0.97, 0.99)) +
  theme(legend.box.just = "center") +
  guides(fill = guide_legend(override.aes = list(size=8))) +
  theme(legend.title = element_text(colour="black", size=36, face="bold")) +
  theme(legend.text = element_text(colour="black", size = 36, face = "bold"))

bubble_89_99_plot

#scale_y_continuous(sec.axis = sec_axis(trans = ~ ./5, 
#name = expression(bold(atop(H[2]*O,(mu*mol~m^bold("-2")~s^bold("-1"))))),
#breaks=c(-3,-1.5,0,1.5,3)),breaks=seq(-10,20,10))
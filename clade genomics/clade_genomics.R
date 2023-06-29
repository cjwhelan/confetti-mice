#create graph of mutations versus clade volume for UV and No UV treatments
# point plot of skew and kurtosis for confetti mice project
#and 95% confidence limits
# by No UV vs UV treatment stratified from month  1-6


rm(list = ls())

library(dplyr)
library(ggplot2)
library(tidyr)




#make an object:
clade_genomics<-read.csv( "D:\\confetti_mice\\clade_genomics.csv")

#check:
glimpse(clade_genomics)

#make a factor
clade_genomics <- 
  clade_genomics %>% 
  mutate(
    mouse_id = as.factor(mouse_id),
    month = as.factor(month),
    treatment = as.factor(treatment),
    month_treatment = as.factor(month_treatment)
  )


glimpse(clade_genomics)

#  shape: 16 = solid square; 17 = solid triangle (pointing up)
color <-  c("blue", "firebrick2")

genomics_plot <- ggplot(clade_genomics) +
  geom_point(size = 6, aes(x = (log2(volume)-13), 
                           y = sqrt_mutation,
                           color = treatment, 
                           shape = month)) +
  scale_color_manual(
                    name = "Treatment",  
                    values = color
                    ) +
  scale_shape_manual(
                    name = "Month",
                    values = c(15, 17)) + 
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)
         ) +
# ggtitle("Skewness") +
  theme_classic() +
  theme(
#    plot.title = element_text(face = "bold", size = 16),
#    plot.margin = margin(0.5, 0.75, 0.5, 0.75, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36),
    legend.title=element_text(size=1326, face = "bold"),
    legend.text=element_text(size=36, face = "bold")
  ) +
  xlim(0,15) +
#  xlab("Log2(clade volume)") +
  ylab("Altering somatic mutations") +
  geom_hline(aes(yintercept = 3.0), color = "black", linetype="dashed", size = 1.25)
  
genomics_plot


genomics_clade_plot <- ggplot(clade_genomics) +
  geom_point(size = 6, aes(x = (log2(volume)-13), 
                           y = sqrt_mutation,
                           color = treatment, 
                           shape = month)) +
  scale_color_manual(
    name = "Treatment",  
    values = color
  ) +
  scale_shape_manual(
    name = "Month",
    values = c(15, 17)) + 
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)
  ) +
    theme_classic() +
  theme(
    plot.margin = margin(0.1, 0.75, 0.5, 0.75, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 36),
    axis.text.y = element_text(color = "black", face = "bold", size = 36),
    axis.title.x = element_text(color = "black", face = "bold", size = 36),
    axis.title.y = element_text(color = "black", face = "bold", size = 36),
    legend.title=element_text(size=36, face = "bold"),
    legend.text=element_text(size=36, face = "bold")
  ) +
  xlim(0,15) +
  xlab(expression(bold("Clade volume"~(log["2"](μ*m^{"3"}))))) +
  ylab(expression((bold("0.05% altered mutations"))^{"1/2"})) +
  geom_hline(aes(yintercept = 3.0), color = "black", linetype="dashed", linewidth = 1.25)

genomics_clade_plot

#μ  μ Square root

#attempt to modify the axes with geom_rangeframe() and themeJaspRaw()
library(jaspGraphs)

genomics_plot2 <- ggplot(clade_genomics) +
  geom_point(size = 4, aes(x = (log2(volume)-13), 
                           y = sqrt_mutation,
                           color = treatment, 
                           shape = month)) +
  scale_color_manual(
    name = "Treatment",  
    values = color
  ) +
  scale_shape_manual(
    name = "Month",
    values = c(15, 17)) + 
  guides(color = guide_legend(order = 1),
         shape = guide_legend(order = 2)
  ) +
  # ggtitle("Skewness") +
  theme_classic() +
  theme(
    #    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.75, 0.5, 0.75, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(color = "black", face = "bold", size = 18),
    legend.title=element_text(size=16, face = "bold"),
    legend.text=element_text(size=16, face = "bold")
  ) +
  xlim(0,15) +
  xlab("Log2(structure volume)") +
  ylab("Square root(0.05% altered mutations)")

genomic2_plot2

genomics_plot2 +
  geom_rangeframe(size = 2) +
  geom_segment(aes(x = 0, y = 3, xend = 15, yend = 3), 
               linetype = "dashed", size = 1.25) +
  themeJaspRaw()

genomics_plot2 +
  geom_rangeframe(mapping = NULL, data = NULL, stat = "identity",
                  position = "identity", na.rm = FALSE,
                  show.legend = TRUE, inherit.aes = TRUE) +
  geom_segment(aes(x = 0, y = 3, xend = 15, yend = 3), 
               linetype = "dashed", size = 1.25) +
  themeJaspRaw()


library(ggthemes)
genomics_plot2 +
  geom_rangeframe() + 
  theme_tufte()

#code to combine 2 legends into 1

#scale_colour_manual(name = "Treatment & State",
#                    labels = c("Control, Non-F", "Control, Flwr", "Exclosure, Non-F", "Exclosure, Flwr"),
#                    values = c("blue", "red", "blue", "red")) +   
#scale_shape_manual(name = "Treatment & State",
#                    labels = c("Control, Non-F", "Control, Flwr", "Exclosure, Non-F", "Exclosure, Flwr"),
#                    values = c(19, 19, 17, 17))


#  geom_rangeframe() +
#  themeJaspRaw()

#####Code below is not doing what I was trying to do....###

#set order to 3NoUV, 3UV, 7NoUV, 7UV:
clade_genomics$month_treatment <- factor(clade_genomics$month_treatment, 
                                  levels = c("NoUV3", "UV3", "NoUV7", "UV7")) 

genomics_plot3 <- ggplot(clade_genomics, aes(x = (log2(volume)-13), 
                                             y = sqrt_mutation), 
                                             color = month_treatment, 
                                             shape = month_treatment) +
  geom_point(size = 4) +
  theme_classic() +
  labs(color = "Treatment", face = "bold") +
#  scale_color_manual(values = c("blue", "firebrick2", "blue", "firebrick2"),
#                     breaks=c("NoUV3", "UV3", "NoUV7", "UV7"),
#                     labels=c("3 months No UV", "3 months UV", 
#                              "7 months No UV", "7 months UV")) +
  scale_shape_manual(
                     values = c(15, 16, 15, 16),
                     breaks = waiver(),
                     labels = waiver()
                     ) + 
#  guides(color = guide_legend(order = 1),
#         shape = guide_legend(order = 2)) +
  # # ggtitle("Skewness") +
  theme(
    #    plot.title = element_text(face = "bold", size = 16),
    #    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    axis.line = element_line(size = 1, colour = "black"),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text.x = element_text(color = "black", face = "bold", size = 16),
    axis.text.y = element_text(color = "black", face = "bold", size = 16),
    axis.title.x = element_text(color = "black", face = "bold", size = 18),
    axis.title.y = element_text(color = "black", face = "bold", size = 18),
    legend.title=element_text(size=16, face = "bold"),
    legend.text=element_text(size=16, face = "bold")
  ) +
  xlim(0,15) +
  xlab("Log2(structure volume)") +
  ylab("Square root(0.05% altered mutations)") +
  geom_hline(aes(yintercept=3.0), color="black", linetype="dashed", size = 1.25)


genomics_plot3 


#  geom_rangeframe() +
#  themeJaspRaw()



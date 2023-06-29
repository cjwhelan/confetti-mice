###try ggbreak

library("ggprism")
library("ggbreak")
library("ggplot2")
library("aplot")
data1 <- read.table(file = "D:/confetti_mice/data1.txt",
                    header = TRUE, sep = "\t", dec = ".")
data1$Species.name <- factor(data1$Species.name,
                             levels = rev(unique(data1$Species.name)))
load("D:/confetti_mice/sigdata.rda")
p1 <- ggplot(
  data = data1,
  mapping = aes(
    x = Species.name,
    weight = Mean,
    fill = group
  )
) +
  geom_bar(position = 'dodge') +
  labs(y = 'Relative abundance(%)', x = NULL) +
  coord_flip() +
  theme_prism()
p1

p2 <- ggplot(
  data = data1,
  mapping = aes(
    x = Species.name,
    weight = log10(Mean+1),
    fill = group
  )
) +
  geom_bar(position = 'dodge') +
  labs(y = 'log10(Relative abundance+1)', x = NULL) +
  coord_flip() +
  theme_prism()
p3 <- p1 +
  scale_y_break(c(32, 84), scales = 0.5, ticklabels=c(84, 85, 86)) +
  scale_y_break(c(3.5, 10), scales = 0.5, ticklabels = c(15, 25))
p4 <- p3 +
  geom_text(
    data = sigdata,
    mapping = aes(
      x = Species,
      y = Mean,
      label = sig
    ),
    vjust=-0.1
  )
plot_list(p1, p2, p3, p4, byrow=T, tag_levels = 'A', tag_size = rel(2))

p1
p2
p3
p4
 
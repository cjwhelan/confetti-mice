

require(ggthemes) 
qplot(mpg, wt, data=mtcars) + geom_rangeframe() + theme_tufte()

qplot(mpg, wt, data=mtcars) + 
  geom_rangeframe(data=data.frame(x=c(10, 35), y=c(0, 6)), aes(x, y)) + 
  theme_tufte() +
  scale_x_continuous(limits = c(10, 35)) +
  scale_y_continuous(limits = c(0, 6)) 

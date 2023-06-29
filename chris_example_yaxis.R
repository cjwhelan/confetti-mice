set.seed(555)

x <- sort(rnorm(1e5))
# make two curves sort of shaped like yours
my_a <- plogis(-1 + 2.5 * x)
my_b <- plogis(1 + 2.25 * x)

# I'm going to subsample them so there are different points between the two
# but also it seems as if they all start at the same point.

a_plot <- c(100, sort(sample(101:length(x), 2000)))
b_plot <- c(100,sort(sample(101:length(x), 2000)))

a_df <- data.frame(
  x = x[a_plot],
  y = my_a[a_plot]
)
b_df <- data.frame(
  x = x[b_plot],
  y = my_b[b_plot]
)

plot(a_df$y ~ a_df$x, pch = 19, col = "blue",
     xlab = "Maximum volume", ylab = "Cumulative Density",
     ylim = c(0,1), xlim = c(-3.75, 3.75))
points(b_df$y ~ b_df$x, pch = 19, col = "red")

# Now here is where we get started. First, interpolate each of these lines.
a_interp <- approxfun(
  x = a_df$y,
  y = a_df$x
)

b_interp <- approxfun(
  x = b_df$y,
  y = b_df$x
)
# Use your starting point and then go out to some maximum number
#  your choice at what to choose here.
common_y_axis <- seq(0,1, length.out = 4000)

# get the points
a_line <- a_interp(common_y_axis)
b_line <- b_interp(common_y_axis)
# substract the two, there will be NA's in here if one of the lines
#  has a longer 'tail'
diff_line <- a_line - b_line

# plot out the difference
plot(common_y_axis ~ diff_line, pch = 19, col = "blue",
     xlab = "Maximum volume", ylab = "Difference",
     ylim = c(0,1))


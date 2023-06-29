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

UV <- read.csv("F:\\confetti_mice\\UV_mice_allmonths_ecdf_sorted.csv")
no_UV <- read.csv("F:\\confetti_mice\\No_UV_mice_allmonths_ecdf_sorted.csv")

UV <- split(UV, factor(UV$month))
no_UV <- split(no_UV, factor(no_UV$month))

results <- vector("list", length = 6)
names(results) <- names(UV)


UV_interp <- approxfun(
  x = UV$m1$distribution_volume,
  y = UV$m1$structure_volume,
  ties = "ordered"
)

no_UV_interp <- approxfun(
  x = no_UV$m1$distribution_volume,
  y = no_UV$m1$structure_volume,
  ties = "ordered"
)


# Use your starting point and then go out to some maximum number
#  your choice at what to choose here.
common_y_axis <- seq(0,1, length.out = 100)

# get the points
UV_line <- UV_interp(common_y_axis)
no_UV_line <- no_UV_interp(common_y_axis)
# substract the two, there will be NA's in here if one of the lines
#  has a longer 'tail'
diff_line <- UV_line - no_UV_line

results$m1 <- data.frame(
  month = "m1", dx = diff_line, y = common_y_axis
)

# plot out the difference
plot(common_y_axis ~ diff_line, pch = 19, col = "blue",
     xlab = "Maximum volume", ylab = "Difference",
     ylim = c(0,1))

ex <- do.call("rbind", results)

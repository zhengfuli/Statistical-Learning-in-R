library(ggplot2)

p = ggplot() + geom_line(data = data.frame(X1 = c(-1, 1), X2 = c(1.5, 0.5)), aes(X1, X2)) +
geom_text(x = c(-0.5, 0.5), y = c(0.75, 1.25), aes(label = label), data.frame(label = c("-2 + X1 + 2X2 < 0", "-2 + X1 + 2X2 > 0"))) +
scale_x_continuous(breaks=seq(-1, 1, 1))


f = function(x)
{
  y1 = 2 - sqrt(4 - (1 + x)^2)
  y2 = 2 + sqrt(4 - (1 + x)^2)
  d = data.frame(x, y1, y2)
  return(d)
}

x = seq(-3, 1, 0.005)

p = ggplot() + geom_line(data = f(x), aes(x, y1)) +
geom_line(data = f(x), aes(x, y2)) + coord_cartesian(xlim = c(-4, 5), ylim = c(-1, 8)) +
xlab("X1") + ylab("X2") +
geom_text(x = c(-1, -1), y = c(2, 4.5), aes(label = label), data.frame(label = c("(1 + X1)^2 + (2-X2)^2 < 4", "(1 + X1)^2 + (2-X2)^2 > 4")))

f1 = function(x, y)
{
  c = ""
  if((1 + x)^2 + (2 - y)^2 < 4) c = "#D55E00"
  else c = "#56B4E9"
  return(c)
}
p = p + geom_point(data = data.frame(x = c(0, -1, 2, 3), y = c(0, 1, 2, 8)), aes(x, y), col = c(f1(0, 0), f1(-1, 1), f1(2, 2), f1(3, 8)), size = 3) + scale_x_continuous(breaks=seq(-4, 5, 1))
# plot(p)

observations = data.frame(X1 = c(3,2,4,1,2,4,4,2), X2 = c(4,2,4,4,1,3,1,3))
col = c(rep("#D55E00", 4), rep("#56B4E9", 4))
p = ggplot() + geom_point(data = observations, aes(X1, X2), col = col, size = 3) +
    coord_cartesian(xlim = c(0, 5), ylim = c(0, 5))

p = p + geom_line(data = data.frame(X1 = c(0.5, 5), X2 = c(0, 4.5)), aes(X1, X2)) +
    geom_line(data = data.frame(X1 = c(0, 5), X2 = c(0, 5)), aes(X1, X2), linetype="dashed") +
    geom_line(data = data.frame(X1 = c(1, 5), X2 = c(0, 4)), aes(X1, X2), linetype="dashed")


# f = function(x){y = 1 + 3 * x; return(y)}
# x1 = -10:10
# x2 = f(x1)
#
# x_range = range(x1)
# y_range = range(x2)
# x_grid = seq(from = x_range[1], to = x_range[2], length = 60)
# y_grid = seq(from = y_range[1], to = y_range[2], length = 60)
# grid_points = expand.grid(X1 = x_grid, X2 = y_grid)
#
# grid_points_predited = ifelse(grid_points$X2 > f(grid_points$X1), 1, 2)
#
# color_array = c("red", "blue")[as.numeric(grid_points_predited)]
# plot(grid_points, col = color_array, pch = 20, cex = 0.25)
# lines(x1, x2)
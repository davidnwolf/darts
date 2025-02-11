library(rgl)
draw_dartboard <- function(z_plane = 0){
  
points <- c(6, 13, 4, 18, 1, 20, 5, 12, 9, 14,
            11, 8, 16, 7, 19, 3, 17, 2, 15, 10)
positions <- c(seq(0, 39 * pi / 20, by = pi / 10))
positions
df <- data.frame(positions = positions, points = points)
df$angle_start <- df$positions - pi / 20
df$angle_end <- df$positions + pi / 20
df
wedge_colors <- rep(c('black', 'white'), 10)
red_bullseye <- 6.35
green_bullseye <- 16
triple <- 103
double <- 166

plot3d(0, 0, 0, type = "n", 
       xlim = c(-200, 200), ylim = c(-200, 200), zlim = c(0, 60), 
       xlab = "X", ylab = "Y", zlab = "Z")
theta <- seq(0, 2*pi, length.out = 200)
x_double_ub <- (double + 4) * cos(theta)
y_double_ub <- (double + 4) * sin(theta)
x_double_lb <- (double - 4) * cos(theta)
y_double_lb <- (double - 4) * sin(theta)
x_triple_ub <- (triple + 4) * cos(theta)
y_triple_ub <- (triple + 4) * sin(theta)
x_triple_lb <- (triple - 4) * cos(theta)
y_triple_lb <- (triple - 4) * sin(theta)
x_double_bull <- red_bullseye * cos(theta)
y_double_bull <- red_bullseye * sin(theta)
x_single_bull <- green_bullseye * cos(theta)
y_single_bull <- green_bullseye * sin(theta)

angles <- seq(pi / 20, 41 * pi / 20, length.out = 21)
for (i in 1:20) {
  arc_angle <- seq(angles[i], angles[i+1], length.out = 20)
  x_outer <- (double - 4) * cos(arc_angle)
  y_outer <- (double - 4) * sin(arc_angle)
  x_wedge <- c(0, x_outer)
  y_wedge <- c(0, y_outer)
  z_wedge <- rep(0,length(x_wedge))
  polygon3d(x_wedge, y_wedge, rep(0,3), col = wedge_colors[i], border=NA)
}

lines3d(x_double_ub, y_double_ub, 0,     col="blue",      lwd=2)
lines3d(x_double_lb, y_double_lb, 0,     col="blue",      lwd=2)
lines3d(x_triple_ub, y_triple_ub, 0,     col='purple',    lwd=2)
lines3d(x_triple_lb, y_triple_lb, 0,     col='purple',    lwd=2)
lines3d(x_double_bull, y_double_bull, 0, col='red',   lwd=2)
lines3d(x_single_bull, y_single_bull, 0, col='green', lwd=2)

x_coord <- c(0,0,0)
y_coord <- c(0,0,0)
radii <- c(green_bullseye, red_bullseye)
colors <- c("green","red")
td_colors <- rep(c("red", "green"), length.out = 20)
polygon3d(x_coord,y_coord,z=rep(0,3), col=colors, border=NA)
for (i in 1:20) {
  arc_angle <- seq(angles[i], angles[i+1], length.out = 20)
  x_outer <- (triple + 4) * cos(arc_angle)
  y_outer <- (triple + 4) * sin(arc_angle)
  x_inner <- (triple - 4) * cos(arc_angle)
  y_inner <- (triple - 4) * sin(arc_angle)
  x_triples <- c(x_inner, rev(x_outer))
  y_triples <- c(y_inner, rev(y_outer))
  polygon3d(x_triples, y_triples, rep(0,3), col=td_colors[i], border=NA)
}

for (i in 1:20) {
  arc_angle <- seq(angles[i], angles[i+1], length.out = 20)
  x_outer <- (double + 4) * cos(arc_angle)
  y_outer <- (double + 4) * sin(arc_angle)
  x_inner <- (double - 4) * cos(arc_angle)
  y_inner <- (double - 4) * sin(arc_angle)
  x_doubles <- c(x_inner, rev(x_outer))
  y_doubles <- c(y_inner, rev(y_outer))
  polygon3d(x_doubles, y_doubles, col=td_colors[i], border=NA)
  x_label <- (double + 50) * cos(positions[i])
  y_label <- (double + 50) * sin(positions[i])
  text3d(x_label, y_label, 0, text = points[i], cex = 0.8, col="black")
}

for(i in 1:20){
  x_start <- green_bullseye * cos(positions[i] + pi / 20)
  y_start <- green_bullseye * sin(positions[i] + pi / 20)
  x_end <- (double + 4) * cos(positions[i] + pi / 20)
  y_end <- (double + 4) * sin(positions[i] + pi / 20)
  segments3d(x = c(x_start,x_end),
             y = c(y_start,y_end),
             z = c(0,0), col = "black", lwd = 2)
}

return(NULL)
}

p_f <- function(x, y){
  if (x == 0) {
    angle <- ifelse(y > 0, pi/2, -pi/2)  # Assign angle based on y
  } else {
    angle <- atan2(y,x)
  }
  radius <- sqrt(x^2 + y^2)
  for(i in 1:20){
    if(radius == 0){
      return(50)
    }
    if(angle > df$angle_start[i] && angle < df$angle_end[i]){
      if(radius < triple + 4 && radius > triple - 4){
        return(points[i] * 3)
      }
      if(radius < double + 4 && radius > double - 4){
        return(points[i] * 2)
      }
      if(radius < green_bullseye && radius > red_bullseye){
        return(25)
      }
      if(radius < red_bullseye){
        return(50)
      }
      if(radius > double + 4) {
        return(0)
      }
      else {
        return(points[i])
      }
    }
  }
  return(NA)
}
circle_uniform <- function(x,y,skill){
  n_points <- 10000
  thetas <- runif(n_points, 0, 2*pi)
  radii <- runif(n_points,0,skill)
  x_points <- x + radii * cos(thetas)
  y_points <- y + radii * sin(thetas)
  random_uniform_points <- data.frame(x = x_points, y = y_points)
  return(random_uniform_points)
}
circle_normal <- function(x,y,skill){
  n_points <- 10000
  thetas <- runif(n_points, 0, 2*pi)
  radii <- rnorm(n_points,0,skill)
  x_points <- x + radii * cos(thetas)
  y_points <- y + radii * sin(thetas)
  random_points <- data.frame(x = x_points, y = y_points)
  return(random_points)
}

objective_function_uniform <- function(x,y,skill){
  random_points <- circle_uniform(x,y,skill)
  point_values <- mapply(p_f, random_points$x, random_points$y)
  return(mean(point_values, na.rm = TRUE))
}
objective_function_normal <- function(x,y,skill){
  random_points <- circle_normal(x,y,skill)
  point_values <- mapply(p_f, random_points$x, random_points$y)
  return(mean(point_values, na.rm = TRUE))
}

radii <- runif(1000,0,double+4)
thetas <- runif(1000,0,2*pi)
x_points <- radii * cos(thetas)
y_points <- radii * sin(thetas)
table <- data.frame(x = x_points, y = y_points)
skill <- 20
table$expected_value_normal <- mapply(objective_function_normal, table$x, table$y, MoreArgs = list(skill = skill))
table$expected_value_uniform <- mapply(objective_function_normal, table$x, table$y, MoreArgs = list(skill = skill))                                  

install.packages("rgl")
library(rgl)
draw_dartboard(z_plane = 0)
for(i in 1:nrow(table)){
  points3d(table$x[i], table$y[i], table$expected_value_uniform[i], col = "green", size = 5)
}


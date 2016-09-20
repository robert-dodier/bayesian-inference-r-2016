L1 <- function (x, y) {exp(-((x - 2)^2 + 2*(y - 1)^2))}
L2 <- function (x, y) {exp(-((x - 1)^2 + 0.5*(y - 2)^2))}
L <- function (x, y) {L1(x, y) + L2(x, y)}
x <- seq (-1, 4, length=100)
y <- seq (-1, 4, length=100)
z  <- outer (x, y, Vectorize (L))
svg ("proportional_target.svg")
contour (x=x, y=y, z)
dev.off ()

Q <- function (x,y) {rnorm (2, mean=c(x, y), sd=0.1)}

mcmc.sequence <- function (n, xy0) {
  x <- vector (length=n)
  y <- vector (length=n)
  x[1] <- xy0[1]
  y[1] <- xy0[2]
  for (i in 2:n) {
    xy1 <- Q (xy0[1], xy0[2])
    r <- L (xy1[1], xy1[2]) / L (xy0[1], xy0[2])
    if (r > 1 || r > runif(1)) {xy0 <- xy1}
    x[i] <- xy0[1]
    y[i] <- xy0[2]
  }
  list (x=x, y=y)
}

xy0 <- Q(-0.9, -0.9)

xy1 <- mcmc.sequence (1000, xy0)
svg ("target+initial-sequence.svg")
contour (x=x, y=y, z)
lines (xy1, col="blue")
dev.off ()

xy2 <- mcmc.sequence (10000, c(xy1$x[1000], xy1$y[1000]))
svg ("target+initial+subsequent-sequence.svg")
contour (x=x, y=y, z)
lines (xy1, col="blue")
lines (xy2, col="red")
dev.off ()


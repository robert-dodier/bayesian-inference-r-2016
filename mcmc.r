pe <- function (e, sigma.e) {exp(-0.5*(e/sigma.e)^2)/sigma.e/sqrt(2*pi)}
pyx <- function (y, x, a.0, a.1, sigma.e) {pe(y - (a.0 + a.1*x), sigma.e)}
pa.0 <- function (a.0) {exp(-0.5*a.0^2)/sqrt(2*pi)}
pa.1 <- function (a.1) {a.1*exp(-a.1)}

data.y <- c(0.35, 0.9, 1.1)
data.x <- c(0.1, 0.5, 0.6)

L <- function (a.0, a.1, sigma.e) {pyx (data.y[1], data.x[1], a.0, a.1, sigma.e) * 
                                   pyx (data.y[2], data.x[2], a.0, a.1, sigma.e) * 
                                   pyx (data.y[3], data.x[3], a.0, a.1, sigma.e)}

L2 <- function (a.0, a.1) {L (a.0, a.1, 0.25)}

a.0 <- seq (-1, 1, length=100)
a.1 <- seq (0, 2, length=100)
z  <- outer (a.0, a.1, Vectorize (L2))
svg ("proportional_target.svg")
contour (x=a.0, y=a.1, z)
dev.off ()

Q <- function (a.0, a.1) {rnorm (2, mean=c(a.0, a.1), sd=0.05)}

mcmc.sequence <- function (n, p0) {
  a.0 <- vector (length=n)
  a.1 <- vector (length=n)
  a.0[1] <- p0[1]
  a.1[1] <- p0[2]
  for (i in 2:n) {
    p1 <- Q (p0[1], p0[2])
    r <- L2 (p1[1], p1[2]) / L2 (p0[1], p0[2])
    if (r > 1 || r > runif(1)) {p0 <- p1}
    a.0[i] <- p0[1]
    a.1[i] <- p0[2]
  }
  list (x=a.0, y=a.1)
}

p0 <- Q(0, 0)

a.initial <- mcmc.sequence (1000, p0)
svg ("target+initial-sequence.svg")
contour (x=a.0, y=a.1, z)
lines (a.initial, col="blue")
dev.off ()

a.additional <- mcmc.sequence (10000, c(a.initial$x[1000], a.initial$y[1000]))
svg ("target+initial+subsequent-sequence.svg")
contour (x=a.0, y=a.1, z)
lines (a.initial, col="blue")
lines (a.additional, col="red")
dev.off ()


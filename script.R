rm(list = ls())
setwd("~/github/dpmgp")
library(ggplot2)
# generate synthetic dataset
set.seed(1)

coeff <- matrix(runif(12, min = -1, max = 1), nrow = 4, ncol = 3)
coeff[1, ] <- runif(3, min = -500, max = 500)

dat <- data.frame(x = NULL, y = NULL, z = NULL)
for (n in 1:100) {
  x <- runif(rpois(1, lambda = 10), min = 0, max = 10)
  z <- sample(1:3, size = 1)
  y <- cbind(1, x, x^2, x^3) %*% coeff[, z] + rnorm(length(x))
  dat <- rbind(dat, cbind(x, y, z))
}
colnames(dat) <- c("x", "y", "z")
dat$z <- factor(dat$z)
ggplot(data = dat, aes(x = x, y = y, color = z)) + geom_point()

###### implementation of DPM-GP ########
K <- 20 # set the maximum number of clusters
sigma <- 1
alpha <- 1
max_iter <- 100
theta <- c(1, 0.005) # hyper-parameters for co-variance function
N <- nrow(dat)
# theta[1] is bandwidth and theta[2] is length_scale
# randomly initialize Z
Z <- matrix(runif(nrow(dat) * K), nrow = nrow(dat), ncol = K) 
Z <- Z / rowSums(Z)
# initialize covariance matrix for each cluster
K <- as.matrix(dist(dat$x))
K <- K * (-theta[2] / 2)
K <- exp(K)
K <- K * theta[1]
mu <- matrix(NA, nrow = N, ncol = K)
for (iter in 1:max_iter) {
  for (k in 1:K) {
    R <- diag(Z[, k]) / (sigma^2)
    C <- solve(K + R)
    mu[, k] <- (C %*% R) %*% matrix(dat$y, nrow = N, ncol = 1)
  }
  
}
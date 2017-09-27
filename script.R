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
Z <- matrix(NA, nrow = nrow(dat), ncol = K)
# randomly initialize Z


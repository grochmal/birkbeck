library(ISLR)
library(boot)
A <- ISLR::Auto
f <- function(x,y) {
  fit <- glm(mpg ~ poly(horsepower, x), data=A)
  err <- cv.glm(A, fitas.numeric(, K=y)
  return(err$delta[1])
}

m <- seq(1,6)
mses <- sapply(m, function(x) f(x,length(A$mpg)))

k10 <- sapply(seq(1,10), function(x) sapply(m, function(x) f(x, 10)))

plot(m, mses)
#k10s <- as.list(as.data.frame(rbind(k10), seq(1,10)))
#lapply(k10s, function(x) lines(m, x[1:6], col=str(as.numeric(x[7]))))
sapply(seq(1,10), function(x) lines(m, k10[,x], col=x))

x <- c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
y <- c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9)
x <- x - mean(x)
y <- y - mean(y)
cov(x,y)
cov(x,x)
cov(y,y)
#comatrix <- matrix(c(

DF.xy <- data.frame( x=c(2.5, 0.5, 2.2, 1.9, 3.1, 2.3, 2, 1, 1.5, 1.1)
                   , y=c(2.4, 0.7, 2.9, 2.2, 3.0, 2.7, 1.6, 1.1, 1.6, 0.9))
sapply(DF.xy, var)

DF <- data.frame( Maths=c(80, 90, 95)
                , Science=c(85, 85, 80)
                , English=c(60, 70, 40)
                , Music=c(55, 45, 50))
sapply(DF,var)

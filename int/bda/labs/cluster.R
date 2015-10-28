set.seed(2)
x <- matrix(rnorm(50*2), ncol=2)
x[1:25,1] <- x[1:25,1] + 3
x[1:25,2] <- x[1:25,2] - 4
plot(x)

km <- kmeans(x, 2, nstart=20)
km$cluster
plot( x, col=(km$cluster+1)
    , main="K-Mean Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

set.seed(4)
km <- kmeans(x, 3, nstart=1000000)
km
plot( x, col=(km$cluster+1)
    , main="K-Mean Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

hac.complete <- hclust(dist(x), method="complete")
hac.average=hclust(dist(x),method="average")
hac.single=hclust(dist(x),method="single")
par(mfrow=c(1,3))
plot(hac.complete,main="Complete Linkage",xlab="",ylab="",cex=0.9)
rect.hclust(hac.complete, 3)
plot(hac.average,main="Average Linkage",xlab="",ylab="",cex=0.9)
rect.hclust(hac.average, 3)
plot(hac.single,main="Single Linkage",xlab="",ylab="",cex=0.9)
rect.hclust(hac.single, 4)

cutree(hac.complete, 2)
cutree(hac.average,  2)
cutree(hac.single,   2)

# correlation
set.seed(4)
nx <- matrix(rnorm(4*3), ncol=3)
nd <- as.dist(1 - cor(t(nx)))

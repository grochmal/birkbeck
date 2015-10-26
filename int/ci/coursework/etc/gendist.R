# From all 6 frames generates a 7th frame where the number of normal and
# abnormal cases is roughly the same.  Following the idea that "a neural
# network is as good as the data used to train it" the network trained on
# this seventh frame shall perform better on the test set.

frames    <- c(1,2,3,4,5,6)
fmat      <- paste(frames, "trn.norm.ssv", sep="")
path      <- "../third_party/"
files     <- paste(path, fmat, sep="")
data      <- Reduce(function(x, y) { rbind(x, read.table(y)) }, files, NULL)
smpls     <- list()
smpls$pos <- data[data[,17] == 1,]
smpls$neg <- data[data[,17] == 0,]
addnrows  <- function(x, y, n) { rbind(x, y[sample(1:nrow(y), n),]) }
output    <- Reduce(function(x, y) addnrows(x, y, 150), smpls, NULL)
write.table(output[sample(1:nrow(output), nrow(output)),],
	    file=paste(path, "good_trn.norm", sep=""),
	    col.names=FALSE, row.names=FALSE)


# The R version for the analysis.m file (it runs faster)

filens <- c("trn.ssv", "tst.ssv")
frames <- c(1,2,3,4,5,6)
fmat   <- sapply(filens, function(x) { paste(frames, x, sep="") })
nms    <- c(fmat[,1], fmat[,2])
path   <- "../third_party/"
flnm   <- function(x) { paste(path, x, sep="") }
files  <- lapply(nms, function(x) { sapply(x, flnm) })
names(files) <- nms
data   <- lapply(nms, function(x) read.table(files[[x]]))
names(data)  <- nms
trnsz  <- sapply(fmat[,1], function(x) { dim(data[[x]]) })
tstsz  <- sapply(fmat[,2], function(x) { dim(data[[x]]) })
app    <- function(f) {
    Reduce(function(x, y) { rbind(x, apply(y, 2, f)) }, data, NULL)
}
mmm    <- lapply(c(mean, min, max), app)
names(mmm)   <- c('mean', 'min', 'max')
cat("Sizes:\n")
print(trnsz)
print(tstsz)
cat("\nStats:\n")
print(mmm)


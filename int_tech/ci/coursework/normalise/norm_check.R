# Script checking whether the normalisation procedure maintains the same
# distribution in the normalised data as in the original data.

filens  <- c("trn", "tst")
frames  <- c(1,2,3,4,5,6)
fmat    <- sapply(filens, function(x) { paste(frames, x, sep="") })
path    <- "../third_party/"
files   <- paste(path, fmat, sep="")
coldist <- function(x) { abs(mean(x) - median(x)) / max(x) }
getfile <- function(file, ext) { read.table(paste(file, ext, sep=""))[1:16] }
chkfile <- function(file) {
    orgnorm <- c(".ssv", ".norm.ssv")
    dst <- sapply(orgnorm, function(x) { apply(getfile(file, x), 2, coldist) })
    rownames(dst) <- NULL
    cat("\n", paste("File:", substring(file, nchar(path)+1)), "\n")
    print(dst)
    dst[,1]
}
results <- sapply(files, chkfile)
colnames(results) <- NULL
cat("\nAny distribution more than 5% different from a normal distribution?\n")
print(results > 0.05)


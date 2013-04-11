# Normalisation procedure

filens  <- c("trn", "tst")
frames  <- c(1,2,3,4,5,6)
fmat    <- sapply(filens, function(x) { paste(frames, x, sep="") })
path    <- "../third_party/"
files   <- paste(path, fmat, sep="")
normcol <- function(x) { x / max(x) }
normall <- function(file) {
    old_file = paste(file, ".ssv", sep="")
    new_file = paste(file, ".norm", sep="")
    write.table(apply(read.table(old_file), 2, normcol),
		file=new_file, col.names=FALSE, row.names=FALSE)
    new_file
}
sapply(files, normall)


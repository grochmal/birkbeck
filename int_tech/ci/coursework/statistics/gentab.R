# Prints the tables used in the coursework based on the data files present in
# the ../results directory.  Tables are divided by training and generalisation
# (test), and compared between the number of hidden nodes.  Also, a comparison
# of the frames is done.

hidden  <- seq(from=5, to=55, by=5)
pfilenm <- function(frm, hid) { sprintf('%s_%02i', frm, hid) }
by_frame <- function(fname, fnums) {
    frms <- paste(fname, fnums, sep="")
    fnms <- lapply(frms, function(x) sapply(hidden, function(y) pfilenm(x, y)))
    names(fnms) <- paste("f", fnums, sep="")
    fnms
}
by_hidden <- function(fname, fnums) {
    frms <- paste(fname, fnums, sep="")
    fnms <- lapply(hidden, function(x) sapply(frms, function(y) pfilenm(y, x)))
    names(fnms) <- paste("h", hidden, sep="")
    fnms
}
path   <- "../results/"
fullnm <- function(x) { paste(path, x, ".dat", sep="") }
read_vec <- function(x) {
    Reduce(function(y, z) { rbind(y, read.table(z)) }, x, NULL)
}
read_all <- function(fnms) {
    files <- lapply(fnms, fullnm)
    lapply(files, read_vec)
}
frames6 <- c(1,2,3,4,5,6)
frames7 <- c(1,2,3,4,5,6,7)
get_files <- function(fname) {
    out     <- list()
    out$byh <- read_all(by_hidden(fname, frames6))
    out$byf <- read_all(by_frame(fname, frames7))
    out
}
bdtable <- function(lst, cnms, rnms) {
    out <- t(sapply(lst, function(x) { apply(x, 2, median) }))
    colnames(out) <- cnms
    rownames(out) <- rnms
    out
}
build_tables <- function(dataset) {
    tbs  <- list()
    ctrn <- c("TP", "FN", "TN", "FP", "acc", "epochs", "error")
    cgen <- c("TP", "FN", "TN", "FP", "acc", "error")
    rhid <- paste("hdn_", hidden, sep="")
    rfrm <- paste("frame_", frames7, sep="")
    tbs$t1_trnbyh <- bdtable(dataset$trn$byh, ctrn, rhid)
    tbs$t2_genbyh <- bdtable(dataset$tst$byh, cgen, rhid)
    tbs$t3_trnbyf <- bdtable(dataset$trn$byf, ctrn, rfrm)
    tbs$t4_genbyf <- bdtable(dataset$tst$byf, cgen, rfrm)
    tbs
}
filens <- c("trn", "tst")
data   <- lapply(filens, get_files)
names(data) <- filens
tables <- build_tables(data)
print_tab <- function(x) {
    write.table(tables[[x]], file=paste(x,'.out',sep=""), quote=FALSE)
}
ignore <- sapply(names(tables), print_tab)
tables


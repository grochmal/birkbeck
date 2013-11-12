#!/usr/bin/env Rscript

IMGSZ = 256
script = "vecgraph.R"
usage <- function(status=0) {
  cat( "Usage:"
     , paste("    ", script
            , " <input.dat> <output.png> [color] [marker]", sep="")
     , ""
     , "color must be a valid plot col value,"
     , "e.g. 'blue', 'black', 'green', 'red', ..."
     , "The default color is 'blue'"
     , ""
     , "marker must be a valid plot pch value in R,"
     , "e.g. it can be numeric in the range 0-25 or a character"
     , "['x', '+', '-', 'O', ...]"
     , "The default marker is 3"
     , sep="\n")
  quit(status=status)
}

args   <- commandArgs(trailingOnly=TRUE)
input  <- ifelse(is.na(args[1])           , usage(1) , args[1])
output <- ifelse(is.na(args[2])           , usage(1) , args[2])
color  <- ifelse(is.na(args[3])           ,   "blue" , args[3])
point  <- ifelse(is.na(args[4])           ,        3 , args[4])
point  <- ifelse(is.na(as.integer(point)) ,    point , as.integer(point))

cat(script, ":", "readng", input, "\n", sep=" ")
data <- read.table(input)
cat(script, ":", "writting", output, "\n", sep=" ")
png(output, width=IMGSZ*2, height=IMGSZ*2)
cat(script, ":", "plotting\n", sep=" ")
plot(data[,1], data[,2], pch=point, col=color)
dev.off()


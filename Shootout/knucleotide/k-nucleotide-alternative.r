#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

input <- as.character(commandArgs(trailingOnly = TRUE)[1])
if(is.na(input)) input <- "fasta_out_5000000.txt"
lines <- readLines(input)

seq_startpoints <- which(substr(lines, 1L, 1L) == ">")
myseq_start <- seq_startpoints[substr(lines[seq_startpoints], 1L, 6L) == ">THREE"]
myseq_end <- min(seq_startpoints[seq_startpoints > myseq_start], length(lines))

sequence <- unlist(strsplit(toupper(lines[(myseq_start + 1L):myseq_end]), ""))
rm(lines)

getFreq <- function(sequence, keylength) {
    if(keylength == 1L) {
        tab <- table(sequence)
    } else {
        if(leaveout <- length(sequence) %% keylength)
            sequence <- sequence[seq_len(length(sequence) - leaveout)]

        x <- matrix(sequence, ncol = keylength, byrow = TRUE)
        tab <- table(apply(matrix(sequence, ncol = keylength, byrow = TRUE), 1L, paste, collapse = ""))
    }

    tab * 100L / sum(tab)
}


print(getFreq(sequence, keylength = 1L))
print(getFreq(sequence, keylength = 2L))
print(getFreq(sequence, keylength = 2L))
print(getFreq(sequence, keylength = 3L)["GGT"])
print(getFreq(sequence, keylength = 4L)["GGTA"])
print(getFreq(sequence, keylength = 6L)["GGTATT"])
print(getFreq(sequence, keylength = 12L)["GGTATTTTAATT"])
print(getFreq(sequence, keylength = 18L)["GGTATTTTAATTTATAGT"])

#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

n <- as.integer(commandArgs(trailingOnly = TRUE)[1])

printOut <- function(x, line.length = 60L) {
    x <- unlist(strsplit(x, ""))
    if(tmp <- length(x) %% line.length)
        x <- c(x, rep("", line.length - tmp))

    cat(paste(apply(matrix(x, ncol = line.length, byrow = TRUE), 1, paste, collapse = ""), collapse = "\n"), "\n")
}


repeatFasta <- function(s, n) {
	nc <- nchar(s)
	printOut(c(rep(s, n %/% nc), substr(s, 1, n %% nc)))
}


randomFasta <- function(genelist, n) {
    printOut(sample(names(genelist), size = n, prob = as.double(genelist), replace = TRUE))
}

iub <- c(0.27, 0.12, 0.12, 0.27, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02)
names(iub) <- c("a", "c", "g", "t", "B", "D", "H", "K", "M", "N", "R", "S", "V", "W", "Y")

homosapiens <- c(0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008)
names(homosapiens) <- c("a", "c", "g", "t")

alu <- paste("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
             "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA",
             "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT",
             "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA",
             "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
             "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
             "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA", sep = "")

cat(">ONE Homo Sapiens alu\n")
repeatFasta(alu, n * 2L)

cat(">TWO IUB ambiguity codes\n")
randomFasta(iub, n * 3L)

cat(">THREE Homo sapiens frequency\n")
randomFasta(homosapiens, n * 5L)

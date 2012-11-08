#!/usr/bin/env Rscript

# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# Version with internal random number generator
# ------------------------------------------------------------------

LINE_LENGTH <- 60L

n <- as.integer(commandArgs(trailingOnly = TRUE)[1])
  


iub <- (c(0.27, 0.12, 0.12, 0.27, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02))
names(iub) <- c("a", "c", "g", "t", "B", "D", "H", "K", "M", "N", "R", "S", "V", "W", "Y")


homosapiens <- (c(0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008))
names(homosapiens) <- c("a", "c", "g", "t")


alu <- paste("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
             "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA", 
             "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT", 
             "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA", 
             "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
             "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
             "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA", sep = "")


repeatFasta <- function(s, n, ll = LINE_LENGTH) {
    slen <- nchar(s)
    ll <- LINE_LENGTH
    pos <- 0L
    writtenInLine <- 0L

    while(n > 0L) {
        # wieviele zeichen schreiben?
        nwrite <- min(slen - pos, n, ll - writtenInLine)
        cat(substr(s, pos + 1L, pos + nwrite))

        writtenInLine <- writtenInLine + nwrite
        n <- n - nwrite
        pos <- pos + nwrite

        if(pos >= slen) pos <- 0L # goto string firstpos
        if(writtenInLine >= ll) { # newline
            cat("\n")
            writtenInLine <- 0L
        }
    }
    if(writtenInLine) cat("\n") # newline at the end
}


randomFasta <- function(genelist, n, ll = LINE_LENGTH) {
    replicate(n %/% ll, cat(paste(sample(names(genelist), size = ll, replace = TRUE, prob = genelist), collapse = ""), "\n"))
    if(left <- n %% ll) 
        cat(paste(sample(names(genelist), size = left, replace = TRUE, prob = genelist), collapse = ""), "\n")
}


cat(">ONE Homo Sapiens alu\n")
repeatFasta(alu, n * 2L)

cat(">TWO IUB ambiguity codes\n")
randomFasta(iub, n * 3L)

cat(">THREE Homo sapiens frequency\n")
randomFasta(homosapiens, n * 5L)

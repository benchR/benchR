#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

LINE_LENGTH <- 60L
RAN_IM <- 139968L
RAN_IA <- 3877L
RAN_IC <- 29573L
RAN_STATE <- 42L

n <- as.integer(commandArgs(trailingOnly = TRUE)[1])
   
iub <- cumsum(c(0.27, 0.12, 0.12, 0.27, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02, 0.02))
names(iub) <- c("a", "c", "g", "t", "B", "D", "H", "K", "M", "N", "R", "S", "V", "W", "Y")


homosapiens <- cumsum(c(0.3029549426680, 0.1979883004921, 0.1975473066391, 0.3015094502008))
names(homosapiens) <- c("a", "c", "g", "t")

alu <- paste("GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG",
             "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA", 
             "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT", 
             "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA", 
             "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG",
             "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC",
             "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA", sep = "")


randomgen <- function(max) {
    RAN_STATE <<- (RAN_STATE * RAN_IA + RAN_IC) %% RAN_IM
    max * RAN_STATE / RAN_IM
}


repeatFasta <- function(s, n) {
    slen <- nchar(s)
    ll <- LINE_LENGTH
    pos <- 0L
    writtenInLine <- 0L

    while(n > 0L) {
        nwrite <- min(slen - pos, n, ll - writtenInLine)
        cat(substr(s, pos + 1L, pos + nwrite))

        n <- n - nwrite
        writtenInLine <- writtenInLine + nwrite
        pos <- pos + nwrite
        if(pos >= slen) pos <- 0L
        if(writtenInLine >= ll) {
            cat("\n")
            writtenInLine <- 0L
        }
    }
    if(writtenInLine)
        cat("\n")
}


randomFasta <- function(genelist, n) {
    ll <- LINE_LENGTH
    for(i in seq(n %/% ll)) {
        rand <- replicate(ll, randomgen(1L))
        cat(paste(sapply(rand, function(x) names(which.max(x <= genelist))), collapse = ""), "\n")
    }
    
    left <- n %% ll
    if(left) {
        rand <- replicate(ll, randomgen(1L))
        cat(paste(sapply(rand, function(x) names(which.max(x <= genelist))), collapse = ""), "\n")
    }
}


cat(">ONE Homo Sapiens alu\n")
repeatFasta(alu, n * 2L)

cat(">TWO IUB ambiguity codes\n")
randomFasta(iub, n * 3L)

cat(">THREE Homo sapiens frequency\n")
randomFasta(homosapiens, n * 5L)

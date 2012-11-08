#!/usr/bin/env Rscript

# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

getSeq <- function(con, id = ">THREE") {
    nc <- nchar(id)
    sequence <- character(0L)

    while(length(line <- readLines(con, n = 1L)) && substr(line, 0L, nc) != id) {}
    while(length(line <- readLines(con, n = 1L)) && substr(line, 0L, 1L) != ">") sequence <- c(sequence, line)

    toupper(paste(sequence, collapse = ""))
}


getFreq1 <- function(sequence, keylength) {
    sequence <- strsplit(sequence, "")[[1L]]
    if(keylength == 1L) {
        tab <- table(sequence)
    } else {
        if(leaveout <- length(sequence) %% keylength)
            sequence <- sequence[seq_len(length(sequence) - leaveout)]
        
        x <- matrix(sequence, ncol = keylength, byrow = TRUE)
        tab <- table(apply(x, 1, paste, collapse = ""))
    }

    tab / sum(tab) * 100
}


### Closer to the original shootout implementation, better memory usage although slower
getFreq2 <- function(sequence, keylength) {
    n <- nchar(sequence)
    res <- list()

    for(i in seq(from = 1L, to = n - (n %% keylength), by = keylength)) {
        ss <- substr(sequence, i, i + keylength - 1L)
        if(is.null(res[[ss]])) res[[ss]] <- 1L else res[[ss]] <- res[[ss]] + 1L
    }

    res <- sort(unlist(res), decreasing = TRUE)
    res / sum(res) * 100
}

#con <- file("stdin", raw = FALSE) # -> stdin mit readLines verhaelt sich seltsam. Muss ich nochmal nachschauen, woran das liegen kann
input <- as.character(commandArgs(trailingOnly = TRUE)[1])
con <- file(input, "r")
sequence <- getSeq(con)
close(con)

getFreq <- getFreq2 # welche der beiden Implementierungen ?

print(getFreq(sequence, keylength = 1L))
print(getFreq(sequence, keylength = 2L))
print(getFreq(sequence, keylength = 3L)["GGT"])
print(getFreq(sequence, keylength = 4L)["GGTA"])
print(getFreq(sequence, keylength = 6L)["GGTATT"])
print(getFreq(sequence, keylength = 12L)["GGTATTTTAATT"])
print(getFreq(sequence, keylength = 18L)["GGTATTTTAATTTATAGT"])


#!/usr/bin/env Rscript

# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------

n <- as.integer(commandArgs(trailingOnly = TRUE)[1])

u <- rep(1.0, n)
j <- seq(from = 0L, to = n - 1L)
i <- rep(seq_len(n), each = n)

M <- 1.0 / matrix((i + j) * (i + j - 1L) / 2L + i, nrow = n, ncol = n, byrow = TRUE)
Mt <- t(M)

for(i in seq_len(10L)) {
    v <- u %*% Mt %*% M
    u <- v %*% Mt %*% M
}

cat(sprintf("%0.9f\n", sqrt(sum(u %*% t(v)) / sum(v %*% t(v)))))
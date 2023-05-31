rm(list = ls())
source('functions.R')

l <- c(1, 1.5, 2)
u <- l + 0.75
intervals <- data.frame(left = l, right = u)
plotIntervalsTex('../tex/figex.tex', intervals, labels = thetas(nrow(intervals)),
                 blank.x = FALSE)

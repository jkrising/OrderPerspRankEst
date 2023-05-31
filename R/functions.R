source('imports.R')

thetas <- function(n)
{
  sprintf('$\\theta_%d$', 1:n)
}

plotIntervalsTex <- function(filename, intervals, pts = NULL, labels = NULL,
                             blank.x = FALSE, asc = TRUE, height = 2,
                             width = 4)
{
  p <- plotIntervals(intervals, pts, labels, blank.x, asc)
  tikz(filename, width = width, height = height, timestamp = FALSE)
  print(p)
  dev.off()
}

catColumns <- function(left, right, open)
{
  ints <- paste(left, right, sep = ', ')
  if (open)
  {
    paste('(', ints, ')', sep = '')
  }
  else
  {
    paste('[', ints, ']', sep = '')
  }
}

make.intervals <- function(x, alpha)
{
  ci <- function(x, alpha)
  {
    t.test(x, conf.level = 1 - alpha)$conf.int[1:2]
  }
  
  intervals <- apply(x, 2, ci, alpha = alpha) |> t() |> as.data.frame()
  names(intervals) <- c('left', 'right')
  intervals
}

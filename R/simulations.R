rm(list = ls())
source('functions.R')

set.seed(1684621292)

mu <- matrix(NA, nrow = 6, ncol = 5)
mu[1, ] <- c(10.0, 10.2, 10.4, 10.6, 10.8)
mu[2, ] <- c(10.0, 10.2, 10.4, 10.6, 10.8)
mu[3, ] <- c(10.0, 10.2, 10.7, 11.2, 11.4)
mu[4, ] <- c(10.0, 10.5, 10.7, 11.0, 11.2)
mu[5, ] <- c(9.8, 10.5, 10.7, 10.9, 11.6)
mu[6, ] <- c(10.1, 10.2, 10.3, 10.4, 10.5)

sigma <- matrix(NA, nrow = 6, ncol = 5)
sigma[1, ] <- c(0.07, 0.07, 0.07, 0.07, 0.07)^2
sigma[2, ] <- c(0.05, 0.05, 0.2, 0.2, 0.2)^2
sigma[3, ] <- c(0.15, 0.15, 0.25, 0.15, 0.15)^2
sigma[4, ] <- c(0.1, 0.3, 0.3, 0.1, 0.5)^2
sigma[5, ] <- c(0.5, 0.1, 0.1, 0.1, 0.5)^2
sigma[6, ] <- c(0.5, 0.5, 0.5, 0.5, 0.5)^2

n <- 30
N <- 1000
p <- ncol(mu)

alphas <- c(0.1, 0.1 / p, 0.05, 0.05 / p)
filenames <- paste('../tex/tab', c('unad90.tex', 'bonf90.tex', 'unad95.tex',
                          'bonf95.tex'), sep = '')

for (i in 1:length(alphas))
{
  alpha <- alphas[i]
  filename <- filenames[i]

  print(sprintf('Generating %s', filename))

  stats <- matrix(NA, nrow = nrow(mu), ncol = 5)
  stats[, 1] <- 1:nrow(mu)
  for (j in 1:nrow(mu))
  {
    perf <- matrix(NA, nrow = N, ncol = 4)
    for (k in 1:N)
    {
      x <- rmvnorm(n, mean = mu[j, ], sigma = diag(sigma[j, ]))
      intervals <- make.intervals(x, alpha)

      perf[k, 1] <- isCompatible(1:p, intervals)

      ris <- rankIntervals(intervals)
      perf[k, 2] <- rankUncertainty(ris, rankIntervals = TRUE)

      sRank <- apply(x, 2, mean) |> rank(ties.method = 'min')
      sioRank <- sampleIntervalOrderRanks(ris, TRUE)

      d1 <- rankingDistance(1:p, sioRank)
      d2 <- rankingDistance(1:p, sRank)
      perf[k, 3] <- d1 <= d2
      perf[k, 4] <- d1 < d2
    }

    stats[j, 2] <- mean(perf[, 1])
    stats[j, 3] <- median(perf[, 2])
    stats[j, 4] <- mean(perf[, 3])
    stats[j, 5] <- mean(perf[, 4])

    stats <- data.frame(stats) |> xtable(align = 'l|r|rrrr|',
                                         digits = c(0, 0, 2, 2, 2, 2))
    names(stats) <- c('Case', 'Cov. Prob.', 'Med. Uncert.',
                      'Prob. Match', 'Prob. Win')
    print(stats, file = filename, include.rownames = FALSE,
          floating = FALSE, comment = FALSE, sanitize.text.function = identity)
  }
}

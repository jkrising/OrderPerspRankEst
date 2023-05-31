rm(list = ls())
source('functions.R')

data <- antiRetroviral
data <- data[complete.cases(data), ]
data <- data[with(data, order(Country.Code)), ]
intervals <- data.frame(left = data[, 'YR2021.lb'], right = data[, 'YR2021.ub'])

plotIntervalsTex('../tex/figdata.tex', intervals, pts = data[, 'YR2021.pe'],
                 labels = data[, 'Country.Code'], blank.x = FALSE, asc = FALSE,
                 height = 8, width = 6.5)


left <- data[, 'YR2021.lb']
right <- data[, 'YR2021.ub']
intervals <- data.frame(left = left, right = right)
ris <- rankIntervals(intervals)

tableData <- data.frame(code = data[, 'Country.Code'],
                        name = data[, 'Country.Name'],
                        estimate = data[, 'YR2021.pe'],
                        intEst = catColumns(left, right, TRUE),
                        rankInt = catColumns(ris[, 'left'], ris[, 'right'], FALSE),
                        sRank = rank(data[, 'YR2021.pe'], ties.method = 'min'),
                        sioRank = sampleIntervalOrderRanks(ris, TRUE))

tableData <- xtable(tableData, align = 'l|ll|rrrrr|')
names(tableData) <- c('Code', 'Country', 'Pt. Est.', 'Int. Est.', 'Rank Int.',
                      'S. Rank', 'SIO Rank')
print(tableData, file = '../tex/tabdata.tex', include.rownames = FALSE,
      floating = FALSE, comment = FALSE)

u <- rankUncertainty(ris, rankIntervals = TRUE)

idx <- data[, 'Country.Name'] != 'Comoros'
df <- intervals[idx, ]
row.names(df) <- 1:nrow(df)
u1 <- rankUncertainty(df)

v <- rankingDistance(tableData[, 'S. Rank'], tableData[, 'SIO Rank'], TRUE)

commands <- c('dataUncertainty', 'dataSubsetUncertainty', 'dataRankDistance')
values <- c(u, u1, v)
lines <- rep(NA, 3)

fileConn<-file('../tex/dataCommands.tex')
for (i in 1:3)
{
  lines[i] <- paste('\\newcommand{\\', commands[i], '}{\\ensuremath{',
                    round(values[i], 3), '}}', sep = '')
}
writeLines(lines, fileConn)
close(fileConn)

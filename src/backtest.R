# load dependencies
require(quantmod)
require(PerformanceAnalytics)

# read and fix dataset
# TODO: read from command line
data <- read.csv('../data/EURUSD60.csv', header=F, colClasses=c('character', 'character', 'numeric', 'numeric', 'numeric', 'numeric', 'numeric'))

# available data sets often have no header
names(data) <- c('date','time','open','high','low','close','volume')

# create extensible data series
#data$time <- sprintf('%s:00', data$time)
#data$timestamp <- chron(as.character(data$date), as.character(data$time), c('y.m.d','h:m:s'))
data$timestamp <- sprintf('%s %s:00', gsub("\\.", "-", data$date), data$time)
data$timestamp <- as.POSIXlt(data$timestamp, tz='GMT')
series <- xts(data[,3:7], order.by=data$timestamp)

# compute indicator
dvi <- DVI(series$close)

# compute signal vector
signal <- Lag(ifelse(dvi$dvi < 0.5, 1, -1))

# compute return gains
returns <- ROC(series$close) * signal

png('chart.png')
charts.PerformanceSummary(returns)
dev.off()


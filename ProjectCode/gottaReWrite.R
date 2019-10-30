movies10 <- downloadStockPairDFRobust('NFLX', 'RGC', start = 2006, nyears = 10)
cor(movies10$stock1,movies10$stock2)

energyandshipping <- downloadStockPairDFRobust('CHK', 'LAKE', start = 2006, nyears = 10)
cor(energyandshipping$stock1,energyandshipping$stock2)

oilandgames10 <- downloadStockPairDF('GRVY', 'XOM', start = 2006, nyears = 10)
cor(anotherone$stock1,anotherone$stock2)

plotRatio <- function(stockDF, k=1) {
  m <- mean(stockDF$ratio); s <- sd(stockDF$ratio)
  plot(x=1:length(stockDF$ratio), y=stockDF$ratio,
       type='l', col='red',
       xlab='', ylab=paste('Ratio (k=',k,')', sep = ''))
  abline(h=c(m-k*s,m,m+k*s), lty=c(2,1,2))
}

findPositions <- function(ratio, m, s, k = 1) {
  len <- length(ratio)
  toReturn <- list(); lind <- 1
  finger <- 1
  openquestionmark <- FALSE
  
  while (finger < len) {

    if (!openquestionmark) { #means we have to find the first place to open a position
      highones <- which(ratio[finger:len]>(m+s*k))
      lowones <- which(ratio[finger:len]<(m-s*k))
      if (length(highones)>0 | length(lowones)>0) {
       start <- min(highones[1], lowones[1], na.rm = TRUE) + finger - 1
       highorlow <- ifelse(identical(start, highones[1]+finger-1),
                           1, -1)
       finger <- start
       openquestionmark <- TRUE
      }
      else finger <- len
    }
    else { #we need to find where to close the position
      close <- ifelse(highorlow == 1,
                      which(ratio[finger:len]<m)[1],
                      which(ratio[finger:len]>m)[1])
      if (is.na(close)) { #means there is no good close; must close on last day
        close <- len
      }
      else close <- close+finger-1
      
      finger <- close
      toReturn[[lind]] <- c(start, close, highorlow)
      lind <- lind + 1
      openquestionmark <- FALSE
    }
       
  }#end of while loop
  return(toReturn)
}
sodas <- downloadStockPairDFRobust('PEP','KO')
plotRatio1(sodas)
pos <- findPositions1(sodas$ratio,
                      mean(sodas$ratio),
                      sd(sodas$ratio))
addPositions <- function(ratio, positions) {
  for (p in positions) {
    points(x=p[1],y=ratio[p[1]],col='green',pch=19)
    points(x=p[2],y=ratio[p[2]],col='red',pch=19)
  }
}
addPositions(sodas$ratio,positions = pos)
positionProfit <- function(stocksDF, positions, net = TRUE) {
  if (length(positions) == 0) return(0)
  cashcash <- numeric(length(positions))
  i <- 1
  for (p in positions) {
    shares1 <- 1/stocksDF$stock1[p[1]]
    shares2 <- 1/stocksDF$stock2[p[1]]
    profit1 <- p[3]*-1 * shares1 * stocksDF$stock1[p[2]]
    profit2 <- p[3] * shares2 * stocksDF$stock2[p[2]]
    fees <- 0.003 * (1+1+abs(profit1)+abs(profit2))
    cashcash[i] <- profit1+profit2-fees
    i <- i+1
  }
  if (net) return(sum(cashcash))
  return(cashcash)
}
plotRatio(ath10)
posath <- findPositions1(ath10$ratio,
                         mean(ath10$ratio),
                         sd(ath10$ratio))
addPositions(ath10$ratio, posath)
positionProfit(ath10, posath, net = FALSE)

findOptimalK <- function(stocksDF, plot = FALSE) {
  ratio <- stocksDF$ratio; m <- mean(stocksDF$ratio); s <- sd(stocksDF$ratio)
  kmax <- max(abs(ratio - m))/s
  kvalues <- seq(0, kmax, length = 100)
  kprof <- sapply(kvalues, function(x) positionProfit(stocksDF = stocksDF,
                                                      findPositions(ratio, m, s, k=x)))
  ind <- which(kprof == max(kprof))[1]
  bestk <- kvalues[ind]
  
  if (plot) {
    plot(x=kvalues, y=kprof, type='p',
         xlab='k value', ylab='Profit')
    points(x=bestk, y=kprof[ind], pch=19, col='red')
  }
  
  return(bestk)
}

evaluatePairsTrading <- function(stocksDF, trainingFrac = 0.5, plot = FALSE) {
  cutoff <- ceiling(nrow(stocksDF)*trainingFrac)
  train <- stocksDF[1:cutoff,]
  test <- stocksDF[(cutoff+1):nrow(stocksDF),]
  k <- findOptimalK(train)
  m <- mean(train$ratio); s <- sd(train$ratio)
  pos <- findPositions(test$ratio, m, s, k=k)
  
  if (plot) {
    plot(x=1:nrow(stocksDF), y=stocksDF$ratio,
         type = 'l', xlab = '', ylab = paste('Ratio (k=',k,')',sep = ''))
    abline(h = c(m-s*k, m, m+s*k), lty=c(2,1,2))
    for (p in pos) {
      points(x=p[1]+cutoff,y=ratio[p[1]+cutoff],col='green',pch=19)
      points(x=p[2]+cutoff,y=ratio[p[2]+cutoff],col='red',pch=19)
    }
  }
  
  return(positionProfit(test, pos))
}

simulateStockPair <- function(n=1000, sigma1=1, sigma2=1, rho=1, psi=0, b1=0, b2=0, plot=FALSE) {
  stock1 <- numeric(n); stock2 <- numeric(n)
  x1 <- 2; x2 <- 2
  for (t in 1:n) {
    stock1[t] <- 40 + b1*t + x1; stock2[t] <- 35 + b2*t + x2
    x1 <- rho*x1 + (1-rho)*psi*x2 + rnorm(1,0,sigma1)
    x2 <- rho*x2 + (1-rho)*psi*x1 + rnorm(1,0,sigma2)
  }
  r <- stock1/stock2
  df <- data.frame(stock1 = stock1, stock2 = stock2, ratio = r)
  if (plot) plotStocks(df)
  return(df)
}

simulateDistribution <- function(nrep = 100, returnCorrelation = FALSE, ...) {
  dist <- numeric(nrep)
  if (returnCorrelation) {
    for (i in 1:nrep) {
      df <- simulateStockPair(...)
      dist[i] <- cor(df$stock1, df$stock2)
    }
    return(dist)
  }
  for (i in 1:nrep) {
    dist[i] <- evaluatePairsTrading(simulateStockPair(...))
  }
  return(dist)
}
system.time(dist <- simulateDistribution(rho = 0.9, psi = 0.5))
filename <- "~/Downloads/profile"
Rprof(filename)
dist <- simulateDistribution()
Rprof(NULL)
summaryRprof(filename)
summaryRprof(filename)$by.self







heatmaprhopsi <- function(steps = 5, returnCorrelation=FALSE) {
  rho <- seq(0,1,length.out = steps); psi <- seq(0,1,length.out = steps)
  grid <- expand.grid(rho=rho, psi=psi)
  fill <- numeric(steps*steps)
  if (returnCorrelation) { #return mean correlation for given rho/psi
    for (i in 1:(steps*steps)) {
      fill[i] <- mean(simulateDistribution(returnCorrelation = TRUE,
                                           rho = grid[i,1],
                                           psi = grid[i,2]))
    }
  }
  else { #return median profit for given rho/psi
    for (i in 1:(steps*steps)) {
      fill[i] <- median(simulateDistribution(rho = grid[i,1],
                                             psi = grid[i,2]))
    }
  }
  grid$f <- fill
  
  library(ggplot2)
  p <- ggplot(grid, aes(x=rho, y=psi, fill=f))
  p <- p + geom_tile()
  p <- p + scale_fill_gradient(low='white', high='steelblue')
  return(p)
}

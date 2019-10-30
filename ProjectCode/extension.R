#Extension

library(dplyr)
downloadPriceDF <- function(stock, start = 2010, nyears = 1) {
  require(BatchGetSymbols)
  startdate <- paste(start, '01', '01', sep = '-')
  enddate <- paste(start+nyears, '01', '01', sep = '-')
  out <- BatchGetSymbols(tickers = stock, first.date = startdate, last.date = enddate, do.cache = FALSE)
  cat('\n')
  if(out$df.control$download.status != 'OK') stop(paste0('something went wrong downloading ', stock, 'prices'))
  stockDF <- data.frame(date = as.character(out$df.tickers$ref.date),
                        price = out$df.tickers$price.adjusted)
  return(stockDF)
} #uses Yahoo finance
downloadStockPairDF <- function(stock1, stock2, start = 2010, nyears = 1) {
  stock1df <- downloadPriceDF(stock1, start = start, nyears = nyears)
  stock2df <- downloadPriceDF(stock2, start = start, nyears = nyears)
  if (!identical(stock1df$date, stock2df$date)) {
    stop('Dates are wack')
    #rid <- which(stock1df$date != stock2df$date)
    #stock1df <- stock1df[-rid, ]
    #stock2df <- stock2df[-rid, ]
  }
  df <- data.frame(stock1 = stock1df$price,
                   stock2 = stock2df$price,
                   ratio = stock1df$price/stock2df$price)
  return(df)
}

plotRatioExtension <- function(stockDF, k=1) {
  n <- nrow(stockDF)
  m <- numeric(n); s <- numeric(n)
  for (i in 1:n) {
    m[i] <- mean(stockDF$ratio[1:i])
    s[i] <- sd(stockDF$ratio[1:i])
  }
  plot(x=1:n, y=stockDF$ratio,
       type='l',
       xlab='', ylab=paste('Ratio (k=',k,')', sep = ''), main='Ratio with Moving Mean/SD')
  lines(x=1:n, y=m, col='slateblue4')
  lines(x=1:n, y=m+s*k, col='slateblue1', lty=2)
  lines(x=1:n, y=m-s*k, col='slateblue1', lty=2)
}
findPositionsExtension <- function(ratio, k=1) {
  len <- length(ratio)
  toReturn <- list(); lind <- 1
  finger <- 2
  openquestionmark <- FALSE
  m <- numeric(len); s <- numeric(len)
  for (i in 1:len) {
    m[i] <- mean(ratio[1:i])
    s[i] <- sd(ratio[1:i])
  }
  
  while (finger < len) {
    
    if (!openquestionmark) { #means we have to find the first place to open a position
      if (ratio[finger] > m[finger]+s[finger]*k) {
        start <- finger
        finger <- finger + 1
        highorlow <- 1
        openquestionmark <- TRUE
      }
      else if (ratio[finger] < m[finger]-s[finger]*k) {
        start <- finger
        finger <- finger + 1
        highorlow <- -1
        openquestionmark <- TRUE
      }
      else {
        finger <- finger + 1
      }
    }
    
    else { #find a place to close position
      if (highorlow == 1) {
        while (openquestionmark) {
          if (ratio[finger]<m[finger]) {
            toReturn[[lind]] <- c(start, finger, 1)
            lind <- lind + 1
            openquestionmark <- FALSE
          }
          else if (finger == len) {
            toReturn[[lind]] <- c(start, finger, 1)
            openquestionmark <- FALSE
          }
          else {
            finger <- finger + 1
          }
        }
      }
      else {
        while(openquestionmark) {
          if (ratio[finger]>m[finger]) {
            toReturn[[lind]] <- c(start, finger, 1)
            lind <- lind + 1
            openquestionmark <- FALSE
          }
          else if (finger == len) {
            toReturn[[lind]] <- c(start, finger, -1)
            openquestionmark <- FALSE
          }
          else {
            finger <- finger + 1
          }
        }
      }
    }
  }
  return(toReturn)
}
addPositions <- function(ratio, positions) {
  for (p in positions) {
    points(x=p[1],y=ratio[p[1]],col='green',pch=19)
    points(x=p[2],y=ratio[p[2]],col='red',pch=19)
  }
}
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

athleisure10 <- downloadStockPairDF('NKE', 'UAA', start = 2006, nyears = 10)
pos <- findPositionsExtension(athleisure10$ratio, k=1)
plotRatioExtension(athleisure10)
addPositions(athleisure10$ratio, pos)
positionProfit(athleisure10,pos)

findOptimalKExtension <- function(stocksDF, plot=FALSE) {
  ratio <- stocksDF$ratio
  len <- length(ratio)
  m <- numeric(len); s <- numeric(len)
  for (i in 1:len) {
    m[i] <- mean(ratio[1:i])
    s[i] <- sd(ratio[1:i])
  }
  kmax <- max(abs(ratio[750:len]-m[750:len])/s[750:len])
  kvalues <- seq(0, kmax, length.out = 100)
  kprof <- sapply(kvalues, function(x) positionProfit(stocksDF = stocksDF,
                                                      findPositionsExtension2(ratio, m, s, k=x)))
  ind <- which(kprof == max(kprof))[1]
  bestk <- kvalues[ind]
  
  if (plot) {
    plot(x=kvalues, y=kprof, type='p',
         xlab='k value', ylab='Profit')
    points(x=bestk, y=kprof[ind], pch=19, col='red')
  }
  
  toReturn <- c(bestk, kprof[ind])
  return(toReturn)
}

evaluatePairsTradingExtension <- function(stocksDF, trainingFrac = 0.5, plot = FALSE) {
  cutoff <- ceiling((nrow(stocksDF)-750)/2)
  k <- findOptimalKExtension(stocksDF[1:(cutoff+750),])[1]
  
  ratio <- stocksDF$ratio; len <- length(ratio)
  m <- numeric(len); s <- numeric(len)
  for (i in 1:len) {
    m[i] <- mean(athleisure10$ratio[1:i])
    s[i] <- sd(athleisure10$ratio[1:i])
  }
  
  pos <- findPositionsExtension2(ratio[(cutoff+751):len],
                                 m[(cutoff+751):len],
                                 s[(cutoff+751):len],
                                 k = k)
  
  if (plot) {
    plotRatioExtension2(stocksDF, k=k)
    for (p in pos) {
      points(x=p[1]+cutoff+750,y=stocksDF$ratio[p[1]+cutoff+750],col='green',pch=19)
      points(x=p[2]+cutoff+750,y=stocksDF$ratio[p[2]+cutoff+750],col='red',pch=19)
    }
    abline(v=cutoff+750, lty=2)
  }  
  
  test <- stocksDF[(cutoff+751):len,]
  return(positionProfit(test, pos))
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
    abline(v = cutoff)
    for (p in pos) {
      points(x=p[1]+cutoff,y=stocksDF$ratio[p[1]+cutoff],col='green',pch=19)
      points(x=p[2]+cutoff,y=stocksDF$ratio[p[2]+cutoff],col='red',pch=19)
    }
  }
  
  return(positionProfit(test, pos))
}








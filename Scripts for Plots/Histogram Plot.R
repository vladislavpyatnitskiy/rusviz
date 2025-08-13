lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

hist.plt.rus <- function(x, s=NULL, e=NULL, data=T, lg=T){
 
  if (data){ p <- NULL # 4 scenarios: no dates, start, end & both dates
  
    getData <- function(A, s, e) { 
      if (is.null(s) && is.null(e))
        return(get_candles(A, from = "2007-07-20", interval = 'daily')) 
      if (is.null(e)) return(get_candles(A, from = s, interval = 'daily')) 
      if (is.null(s)) return(get_candles(A, till = e, interval = 'daily')) 
      return(get_candles(A, from = s, till = e, interval = 'daily')) 
    }
    for (A in x){ D <- as.data.frame(getData(A, s, e)[,c(3,8)])
    
      D <- D[!duplicated(D),] # Remove duplicates
      
      p <- cbind(p, xts(D[, 1], order.by = as.Date(D[, 2]))) }
      
    p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
    
    colnames(p) <- x # Put the tickers in column names
    
    x <- p } # Give column names 
    
  if (lg | data) x <- diff(log(as.timeSeries(x)))[-1,] # log returns 
  
  for (n in 1:ncol(x)){ s <- x[,n] # For each column
  
    h <- hist(
      s,
      main=sprintf("%s Histogram & Normal Distribution",colnames(s)),
      ylab = "Likelihood",
      xlab = "Returns",
      xlim = c(min(s), max(s)),
      col = "darkgreen",
      border = "white",
      breaks = 100,
      las=1,
      freq=F
      )
    
    grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
    abline(v = 0, col = "lightblue", lwd = 2) # Add vertical line at 0
    abline(h = 0)
    
    curve(dnorm(x, mean = mean(s), sd = sd(s)), col = "red", lwd = 3, add = T)
    
    box() } # Define borders
}
hist.plt.rus(c("LKOH","ROSN","TATN","SIBN","RNFT"), s="2025-01-01")

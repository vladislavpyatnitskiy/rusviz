lapply(c("moexer", "timeSeries", "xts"), require, character.only = T) # Libs 

box.plt.rus <- function(x, s=NULL, e=NULL, data=T, lg=F, main=NULL, xlab=NULL){
  
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
  
  if (is.null(main)) main = "Fluctuations of Securities"
  
  boxplot.matrix(
    x,
    main = main,
    title = F,
    las = 1,
    col = "steelblue",
    xlab = xlab
    )
  
  axis(side = 4, las = 2) # Second y-axis
  
  par(mar = c(5, 4, 4, 4)) # Define borders of the plot to fit right y-axis
  
  grid(nx = NULL, ny = NULL, col = "grey", lty = "dotted", lwd = 1)
  
  abline(h = 0, col = "black", lty = 3) # break even horizontal line
}
box.plt.rus(c("LKOH", "ROSN", "TATN", "SIBN", "RNFT"), s="2023-01-01") # Test

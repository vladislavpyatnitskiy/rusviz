lapply(c("moexer", "xts", "timeSeries"), require, character.only = T) # Libs

rus.cum.plot <- function(x, s = NULL, e = NULL, main = NULL){
 
  p <- NULL # 4 scenarios: no dates, only start or end dates, both dates
  
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
  
  colnames(p) <- x # Assign column names
  
  p <- as.timeSeries(p) # Display time series 
  
  p <- apply(diff(log(p))[-1,], 2, function(col) exp(cumsum(col)) - 1) 
  
  par(mar = c(8, 2.5, 4, 2.5)) # Define borders of the plot
  
  plot(
    p[,1],
    ylim = c(min(p), max(p)),
    lty = 1,
    type = "l",
    lwd = 2,
    las = 1,
    xlab = "Trading Days",
    ylab = "Returns (%)",
    main = main
    )
  
  axis(side = 4, las = 2) # Right Y-Axis Values
  
  grid(nx = 1, ny = NULL, lty = 3, col = "grey") # Horizontal lines
  
  abline(h = 0) # Add black horizontal line at break even point
  
  for (n in 2:(ncol(p))){ lines(p[,n], col = n, lwd = 2) } # Plot indices
  
  legend(
    x = "bottom",
    inset = c(0, -0.27),
    legend = x,
    col = seq(ncol(p)),
    lwd = 2,
    cex = .85,
    bty = "n",
    xpd = T,
    horiz = T
    )
  
  on.exit(par(par(no.readonly = T))) # Show legend with names
}
rus.cum.plot(c("LKOH", "ROSN", "SIBN", "TATN", "RNFT"),
             main = "Russian Oil Stocks Dynamics")

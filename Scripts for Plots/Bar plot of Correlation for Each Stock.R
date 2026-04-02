lapply(c("xts", "timeSeries", "moexer"), require, character.only = T) # lib

bar.plt.cor.rus <- function(x, s=NULL, e=NULL, main=NULL, method="spearman"){
  
  p <- NULL
  
  redom = list(
    c("AGRO", "RAGR"), c("CIAN", "CNRU"), c("HHRU", "HEAD"), c("FIVE", "X5"),
    c("FIXP", "FIXR"), c("YNDX", "YDEX"))
  
  from = "2007-01-01"
  
  R <- NULL
  
  for (n in 1:length(x)){
    
    if (any(sapply(redom, function(redom_item) x[n] %in% redom_item))){
      
      f <- which(sapply(redom, function(redom_item) x[n] %in% redom_item))
      
      for (k in 1:length(redom[[f]])){
        
        a = as.data.frame(
          get_candles(redom[[f]][k], from=from, interval='daily')[,c(3,8)]
        )
        
        if (k == 2){ 
          
          message(
            sprintf(
              "%s is downloaded; %s from %s", x[n], which(x == x[n]), length(x)
            )
          )
        }
        
        a <- a[!duplicated(a),] # Remove duplicates
        
        a <- xts(a[, 1], order.by = as.Date(a[, 2]))
        
        if (x[n] == "AGRO") a <- a / 7.01
        
        colnames(a) <- redom[[f]][2]
        
        if (is.null(R)) R <- data.frame(a) else R <- rbind.data.frame(R, a)
      }
    } else {
      
      a = as.data.frame(get_candles(x[n],from=from,interval='daily')[,c(3,8)])
      
      message(
        sprintf(
          "%s is downloaded; %s from %s", 
          x[n], which(x == x[n]), length(x)
        )
      )
      
      a <- a[!duplicated(a),] # Remove duplicates
      
      a <- xts(a[, 1], order.by = as.Date(a[, 2]))
      
      colnames(a) <- x[n]
      
      R <- data.frame(a) 
    }
    
    R <- as.timeSeries(R) # Make it time series
    
    if (x[n] == "BELU"){ j <- which(rownames(R) == "2024-08-15")
    
    R[c(1:j),] <- R[c(1:j),]/8 } # Adjustments for Novabev stock
    
    p <- cbind(p, R) 
    R <- NULL  # Reset R for next iteration
  }
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Get rid of NA
  
  colnames(p) <- x # Put the tickers in data set
  
  p <- as.timeSeries(p) # Make it time series
  
  cor_matrix <- cor(as.matrix(diff(log(as.timeSeries(p)))[-1,]), method=method)
  
  M <- NULL
  
  for (n in 1:ncol(cor_matrix)){ l <- sort(cor_matrix[n,])
    
    l <- l[l < 1]
    
    C = c(
      "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
      "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
      "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
      "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
      "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
      "#895c8b","#bd5975"
    ) # Add colour range
    
    par(mar = rep(5, 4)) # Define borders of the plot
    
    title = ifelse(
      is.null(main) == T, sprintf("Stock Correlations for %s", x[n]), main
    )
    
    # Create bar plot
    B <- barplot(
      l,
      horiz = F,
      col = C,
      main = title,
      ylab = "Median Correlation Level",
      ylim = c(0, ceiling(max(l))),
      las = 2,
      xpd = F
    )
    # Y axis
    p.seq <- seq(0, .95, .05)
    axis(side = 2, at = p.seq, las = 1, labels = p.seq)
    axis(side = 4, at = seq(0, 1, .05), las = 1, labels = seq(0, 1, .05))
    
    for (a in p.seq){ abline(h = a, col ="grey", lty = 3) } # Horizontal lines
    abline(v = B, col = "grey", lty = 3) # Vertical lines
    
    c <- c("black", "red", "orange", "gold", "greenyellow","green","limegreen")
    v <- c(0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2)
    
    for (a in 1:length(v)){ abline(h = v[a], col = c[a], lwd = 2) } # Lines
    
    box() # Add box
    
    m <- NULL # Write advices about securities according to correlations
    
    j <- list(
      c(.5, 1, "Sell these Assets:"),
      c(.45, .5, "Sell one of these Assets:"),
      c(.4, .45, "Consider to sell one of these Assets:"),
      c(.35, .4, "Check this Combination:"),
      c(.3, .35, "OK to keep Combination:"),
      c(.25, .3, "Good Combination:"),
      c(.2, .25, "Great Combination:"),
      c(-1, .2, "Best Combination:")
    )
    
    for (k in 1:length(j)){ # Messages indicating correlation levels for stocks
      
      o = j[[k]][1]
      t = j[[k]][2]
      
      if (!identical(names(which(l > o & l < t)), character(0))){
        
        m = c(m, paste(j[[k]][3], toString(names(which(l > o & l < t))))) } }
    
  if (is.null(M)) M <- list(m) else M[[n]] <- m } # Display
  
  names(M) <- x
  
  M
}
bar.plt.cor.rus(
  c(
    "RNFT", "WTCMP", "BISVP", "RENI", "DIOD", "MAGN","MRKV", "LSRG", "CNRU", 
    "RAGR", "HIMCP", "AQUA"
  )
)

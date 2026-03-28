lapply(c("xts", "timeSeries", "moexer"), require, character.only = T) # lib

bar.plt.cor <- function(x, main=NULL, method="spearman"){
  
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
      
      a = as.data.frame(get_candles(x[n], from=from, interval='daily')[,c(3,8)])
      
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
  
  p <- p[apply(p, 1, function(x) all(!is.na(x))),] # Eliminate NAs
  
  colnames(p) <- x # Column names
  
  # Calculate correlation matrix
  cor_matrix <- cor(as.matrix(diff(log(as.timeSeries(p)))[-1,]), method=method)
  
  # Extract unique pairs and their correlations
  cor_pairs <- which(upper.tri(cor_matrix, diag = TRUE), arr.ind = TRUE)
  
  # Put them into one data frame
  unique_pairs <- data.frame(Variable1 = rownames(cor_matrix)[cor_pairs[, 1]],
                             Variable2 = rownames(cor_matrix)[cor_pairs[, 2]],
                             Correlation = cor_matrix[cor_pairs]
  )
  # Filter out pairs with correlation equal to 1
  f <- unique_pairs[unique_pairs$Correlation != 1, ]
  
  rownames(f) <- seq(nrow(f)) # Row numbers
  
  colnames(f) <- c("Security 1", "Security 2", "Correlation")
  
  cor.names <- unique(c(f[,1], f[,2])) # Show only unqiue tickers
  
  l <- NULL # Calculate median and mean correlation for each security
  
  for (n in 1:length(cor.names)){ k <- cor.names[n] # Select ticker
  
  v <- f[f$`Security 1` == k | f$`Security 2` == k, ] # ticker's correlations
  
  l <- rbind.data.frame(l, cbind(median(v[,3]), mean(v[,3]))) } # Join
  
  rownames(l) <- cor.names
  colnames(l) <- c("Median", "Average") # Column names
  
  l <- l[order(l$Median), ] # Sort in an ascending way
  
  C = c(
    "#466791","#60bf37","#953ada","#4fbe6c","#ce49d3","#a7b43d","#5a51dc",
    "#d49f36","#552095","#507f2d","#db37aa","#84b67c","#a06fda","#df462a",
    "#5b83db","#c76c2d","#4f49a3","#82702d","#dd6bbb","#334c22","#d83979",
    "#55baad","#dc4555","#62aad3","#8c3025","#417d61","#862977","#bba672",
    "#403367","#da8a6d","#a79cd4","#71482c","#c689d0","#6b2940","#d593a7",
    "#895c8b","#bd5975"
  ) # Add colour range
  
  par(mar = rep(5, 4)) # Define borders of the plot
  
  # Create bar plot
  B <- barplot(
    l[,1],
    names.arg = rownames(l),
    horiz = F,
    col = C,
    main = ifelse(is.null(main) == T, "Stock Correlations", main),
    ylab = "Median Correlation Level",
    ylim = c(0, ceiling(max(l[,1]))),
    las = 2
  )
  # Y axis
  p.seq <- seq(0, .95, .05)
  axis(side = 2, at = p.seq, las = 1, labels = p.seq)
  axis(side = 4, at = seq(0, 1, .05), las = 1, labels = seq(0, 1, .05))
  
  for (n in p.seq){ abline(h = n, col ="grey", lty = 3) } # Horizontal lines
  abline(v = B, col = "grey", lty = 3) # Vertical lines
  
  c <- c("black", "red", "orange", "gold", "greenyellow", "green", "limegreen")
  v <- c(0.5, 0.45, 0.4, 0.35, 0.3, 0.25, 0.2)
  
  for (n in 1:length(v)){ abline(h = v[n], col = c[n], lwd = 2) } # Lines
  
  box() # Add box
  
  h <- l[,1] # Column for median values
  
  names(h) <- rownames(l) # Assign tickers to vector
  
  m <- NULL # Write advices about securities according to correlations
  
  j <- list(
    c(.5, 1, "Sell these Assets:"),
    c(.45, .5, "Sell one of these Assets:"),
    c(.4, .45, "Consider to sell one of these Assets:"),
    c(.35, .4, "Check these Assets:"),
    c(.3, .35, "OK to keep Assets:"),
    c(.25, .3, "Good Assets:"),
    c(.2, .25, "Great Assets:"),
    c(-1, .2, "Best Assets:")
  ) 
  
  for (n in 1:length(j)){ # Messages indicating correlation levels for stocks
    
    o = j[[n]][1]
    t = j[[n]][2]
    
    if (!identical(names(which(h > o & h < t)), character(0))){
      
      m = c(m, paste(j[[n]][3], toString(names(which(h > o & h < t))))) } }
  
  m # Display
}
bar.plt.cor(c("RNFT", "WTCMP", "BISVP", "RENI", "DIOD", "MAGN","MRKV", "LSRG"))

profilePlot <- function(x, x.var, pred.data=x$trainingData, plot=T, ...) 
{
  xname <- ifelse(is.character(x.var), x.var,
                  ifelse(is.name(x.var), deparse(x.var), eval(x.var)))
  
  main = paste0("Partial Dependence on \"", xname, "\"")
  
  n.pt = min(length(unique(pred.data[, xname])), 50)
  xlab = xname
  ylab = names(pred.data)[ncol(pred.data)]
  
  xv <- pred.data[, xname]
  n <- nrow(pred.data)
  
  if (is.factor(xv) && !is.ordered(xv)) {
    x.pt <- levels(xv)
    y.pt <- numeric(length(x.pt))
    for (i in seq(along = x.pt)) {
      x.data <- pred.data
      x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
      y.pt[i] <- mean(predict(x, x.data), na.rm = TRUE)
    }
    
    if(plot) {
      library(fields)
      
      windows(width=16, height=9)
      m <- mean(y.pt)
      palette <- topo.colors(length(xv))
      colors <- palette[sapply(x.pt, function(x) sum(xv==x, na.rm=T))]
      barplot(y.pt - m, width = rep(1, length(y.pt)), col = colors,
              xlab = paste0("Mean difference on \"", ylab, "\""),
              ylab = xlab, main = main, names.arg = x.pt, horiz=TRUE, ...)
      #     plot <- data.frame(val=y.pt - m, col=colors)
      #     row.names(plot) <- x.pt
      #     ggplot(plot, aes(x=val)) + geom_bar()
      image.plot(legend.only=TRUE, col=palette, zlim=c(0, length(xv)))
      abline(v = 0)
    }
  }
  else {
    if (is.ordered(xv)) 
      xv <- as.numeric(xv)
    x.pt <- seq(min(xv), max(xv), length = n.pt)
    if(is.integer(xv))
      x.pt <- as.integer(x.pt)
    y.pt <- numeric(length(x.pt))
    y.pt.sup <- y.pt
    y.pt.inf <- y.pt
    for (i in seq(along = x.pt)) {
      x.data <- pred.data
      x.data[, xname] <- rep(x.pt[i], n)
      predicted <- predict(x, x.data)
      y.pt[i] <- mean(predicted, na.rm = TRUE)
      y.pt.sup[i] <- y.pt[i] + (1.96*sd(predicted, na.rm=TRUE)/sqrt(nrow(x.data)))
      y.pt.inf[i] <- y.pt[i] - (1.96*sd(predicted, na.rm=TRUE)/sqrt(nrow(x.data)))
    }
    
    if(plot) {
      windows(width=16, height=9)
      par(mfrow=c(1,2), pty="s")
      myFit <- loess(y.pt~x.pt)
      plot(pred.data[, xname], pred.data[, ncol(pred.data)], pch=19,
           col=rgb(0,0,0,0.2), xlab=xlab, ylab=ylab, main=main)
      xl <- seq(min(x.pt),max(x.pt), (max(x.pt) - min(x.pt))/1000)
      lines(x.pt, y.pt, lwd=4, col="red")
      #     lines(x.pt, y.pt.sup, lwd=2, col="grey")
      #     lines(x.pt, y.pt.inf, lwd=2, col="grey")
      #lines(xl, predict(myFit,xl), col='red', lwd=4)
      plot(x.pt, y.pt, type = "l", xlab = xlab, ylab = ylab,
           ylim=c(min(y.pt.inf), max(y.pt.sup)),
           main = "Profile zoom", col="green", lwd=5)
      rug(quantile(xv, seq(0.1, 0.9, by = 0.1)), side = 1)
      #     lines(x.pt, y.pt.sup, lwd=2, col="grey")
      #     lines(x.pt, y.pt.inf, lwd=2, col="grey")
      #plot(x.pt, y.pt)
    }
  }
  invisible(list(x = x.pt, y = y.pt))
}
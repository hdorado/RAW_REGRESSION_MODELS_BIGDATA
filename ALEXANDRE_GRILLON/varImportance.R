varImportance <- function(model, pred.data=model$trainingData, ..., scale=T) 
{
  library(caret)
  inputs <- pred.data[,-ncol(pred.data)]
  response <- pred.data[,ncol(pred.data)]
  n <- nrow(pred.data)
  
  errors <- rep(0, ncol(inputs))
  errors <- as.data.frame(errors)
  row.names(errors) <- names(inputs)
  
  for(input in names(inputs)) {

    xname <- ifelse(is.character(input),
                    input,
                    ifelse(is.name(input),
                           deparse(input),
                           eval(input)))
    
    n.pt = min(length(unique(pred.data[, xname])), 50)
    
    err <- 0
    
    xv <- pred.data[, xname]
    if (is.factor(xv) && !is.ordered(xv)) {
      x.pt <- levels(xv)
      mode <- xv[which.max(tabulate(match(xv, x.pt)))]
      x.data <- pred.data
      x.data[, xname] <- rep(mode, n)
      mode.err <- RMSE(predict(model, x.data), response)
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- factor(rep(x.pt[i], n), levels = x.pt)
        input.cancellation <- (RMSE(predict(model, x.data), response) - mode.err)^2
        if(!is.na(input.cancellation)) err <- err + input.cancellation
      }
    } else {
      if (is.ordered(xv)) 
        xv <- as.numeric(xv)
      x.pt <- seq(min(xv), max(xv), length = n.pt)
      mean <- mean(xv)
      x.data <- pred.data
      x.data[, xname] <- rep(mean, n)
      mean.err <- RMSE(predict(model, x.data), response)
      for (i in seq(along = x.pt)) {
        x.data <- pred.data
        x.data[, xname] <- rep(x.pt[i], n)
        input.cancellation <- (RMSE(predict(model, x.data), response) - mean.err)^2
        if(!is.na(input.cancellation)) err <- err + input.cancellation
      }
    }
    errors[input,] <- err
  }
  
  if(scale) {
    errors <- errors - min(errors, na.rm = TRUE)
    errors <- errors/max(errors, na.rm = TRUE) * 100
  }

  invisible(errors)
  return(errors)
}
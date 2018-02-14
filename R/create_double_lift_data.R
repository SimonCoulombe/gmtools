#' create_double_lift_data
#'
#' This function creates the requisite data for a double lift chart
#' @param modelA A vector that contains the predictions for Model A - preds will be pro-rated
#' @param modelB A vector that contains the predictions for Model B - preds will be pro-rated
#' @param actual A vector that contains the actuals that the models are to be compared to
#' @param weight A vector that contains the row level weights (exposure,claim count). Defaults to NULL
#' @param nbins Passed to the binnarise function - how many equally sized bins should the ratio be cut into
#' @keywords double lift
#' @export
#' @examples
#' 

create_double_lift_data <- function(modelA,modelB,actual,weight=NULL,nbins=10){
  
  ## Make sure data.table package is loaded
  suppressPackageStartupMessages(requireNamespace("data.table"))
  
  ## Deal with null weights
  if(is.null(weight)) weight = rep(1,length(modelA))
  
  ## Check if nbins is positive integer
  if(nbins < 1|nbins %% 1 != 0) stop('ERROR: nbins should be positive integer')
  
  ## Bind everything together into a working table
  dtWorking = data.table(modelA = modelA,modelB = modelB,modelRatio = modelA/modelB,actual = actual,weight = weight)
  
  ## Check for NAs in other data points
  if(anyNA(x = dtWorking[,.(modelA,modelB,actual,weight)])) stop('ERROR: No NANs allowed in scores, weights or actuals')
  
  ## Create the bins
  dtWorking$bin = binnarise(x = dtWorking$modelRatio,w = dtWorking$weight,nbins = nbins)
    
  ## Pro-rata scores
  dtWorking$modelA = dtWorking$modelA * dtWorking$weight
  dtWorking$modelB = dtWorking$modelB * dtWorking$weight
  
  ## rebase columns
  dtWorking$modelA = rebase_col(x = dtWorking$modelA,base = dtWorking$actual,w = dtWorking$weight)
  dtWorking$modelB = rebase_col(x = dtWorking$modelB,base = dtWorking$actual,w = dtWorking$weight)
  
  ## Aggregate
  ret = dtWorking[ , list(mean_modelA = weighted.mean(x = modelA,w = weight),
                          mean_modelB = weighted.mean(x = modelB,w = weight),
                          mean_actual = weighted.mean(x = actual,w = weight),
                          sum_weight  = sum(weight))
                   , by = bin][order(bin),]
  return(ret)
}








#' create_quantile_plot_data
#'
#' This function creates the data required for a quantile plot
#' @param y_pred The predictions that you want to compare the actuals to - these will be pro-rated.
#' @param y_true The actuals you want to compare your model against
#' @param w      A column of row level weights (e.g. exposure, claim count). Defaults to NULL
#' @param nbins  How many quantiles do you want to create? Defaults to 10
#' @keywords quantile plot
#' @importFrom MLmetrics MAE
#' @export
#' @examples
#' 

create_quantile_plot_data <- function(y_pred,y_true,w=NULL,nbins=10){
  
  ## Make sure data.table package is loaded
  ##suppressPackageStartupMessages(requireNamespace("data.table"))
  ##suppressPackageStartupMessages(requireNamespace("MLmetrics"))
  
  ## Deal with null weights
  if(is.null(w)) w = rep(1,length(y_true))
  
  ## Bind all the info into a data.table
  dtWorking = data.table::data.table(y_true = y_true,y_pred = y_pred,w = w)
  
  ## Create the binarised value
  dtWorking$bin = gmtools::binnarise(x = dtWorking$y_pred,w = dtWorking$w,nbins = nbins)
  
  ## Add in some pro-rating as the scores will usually be for 1 year
  dtWorking$y_pred = dtWorking$y_pred * dtWorking$w
  
  ## Rescale the scores to the actuals
  dtWorking$y_pred = gmtools::rebase_col(x = dtWorking$y_pred,base = dtWorking$y_true,w = dtWorking$w)

  ## Do the aggregations
  dtSummary = dtWorking[, list(wmean_pred = weighted.mean(x = y_pred,w = w),
                               wmean_true = weighted.mean(x = y_true,w = w),
                               sum_weight = sum(w))
                        , by = bin][order(bin),]
  
  ## Return the centile data & the bMAE
  return(list(data = dtSummary,bMAE = MLmetrics::MAE(y_true = dtSummary$wmean_true,y_pred = dtSummary$wmean_pred)))
}



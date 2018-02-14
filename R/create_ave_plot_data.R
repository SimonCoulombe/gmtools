#' create_ave_plot_data
#'
#' This function creates a data.table containing average actual & fitted performance over a specified dimension
#' @param x A vector that contains the dimension that you want to aggregate by
#' @param y_true A vector that contains the target variable (think OBS in emblem)
#' @param y_pred A vector that contains the fitted value from a model (think CA in emblem). Defaults to NULL
#' @param w A vector that contains row level weights (e.g. exposure). Defaults to NULL
#' @param rescale A logical indicating whether you want to the resulting series scaled about the largest level
#' @keywords ave
#' @export
#' @examples
#'

create_ave_plot_data <- function(x,y_true,y_pred=NULL,w=NULL,rescale=FALSE){

  ## make sure packages are loaded
  ## suppressPackageStartupMessages(requireNamespace("data.table"))

  ## Deal with null weights
  if(is.null(w)) w = rep(1,length(x))

  ## Deal with null preds
  pred_null = is.null(y_pred)
  if(pred_null) y_pred = rep(1,length(x))

  ## Consume the information into a data.table
  dtWorking = data.table::data.table(x = x,y_pred = y_pred,y_true = y_true,w = w)

  ## Check for NANs
  if(anyNA(dtWorking[,c("y_pred","y_true","w"),with=FALSE])) stop('ERROR: NAs present in working frame - check your inputs')

  ## Rebase the predictions to the actuals
  dtWorking$y_pred = rebase_col(x = dtWorking$y_pred,base = dtWorking$y_pred,w = dtWorking$w)

  ## Do the requisite aggregations
  dtSummary = dtWorking[ , list(wmean_pred = weighted.mean(x = y_pred,w = w),
                                wmean_true = weighted.mean(x = y_true,w = w),
                                sum_weight = sum(w))
                         , by = x][order(x),]

  ## Do a rescaling if required
  if(rescale==TRUE){
    base_level = which.max(dtSummary$sum_weight)

    pred_base = unlist(dtSummary[base_level,'wmean_pred'])
    true_base = unlist(dtSummary[base_level,'wmean_true'])

    dtSummary[,'wmean_pred'] = dtSummary[,'wmean_pred'] / pred_base
    dtSummary[,'wmean_true'] = dtSummary[,'wmean_true'] / true_base
  }

  ## Create a rescaled weight term - always need this for ggplot2
  dtSummary$scaled_weight = dtSummary$sum_weight * min(dtSummary$wmean_true) / max(dtSummary$sum_weight)

  ## If preds were null then remove from data
  if(pred_null) dtSummary[,'wmean_pred'] <- NULL

  ## Return the results
  return(dtSummary)
}

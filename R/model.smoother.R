#' model.smoother
#'
#' A generic function for doing emblem style smoothing using a smoothing spline
#' @param df     A data.frame (or coercible object) that contains the training data
#' @param x      A list of column names that identify the explanatory features 
#' @param y      A column name that identifies that target variable
#' @param w      A column name that contains the name of the weight column. Defaults to NULL
#' @param preds  A vector that contains the model predictions
#' @param pdp    A data.frame from the pdp::partial function
#' @param ...    Further arguments passed to smooth.spline
#' @keywords model smoothing
#' @export
#' @examples
#' 

model.smoother <- function(df,x,y,w=NULL,preds,pdp,...){
 
 ## Coerce df to null
 if(class(df)[1] != "data.frame") df = as.data.frame(df)
 
 ## Check that the key.var is equal to the xvalue
 if(x != names(pdp)[1]) stop('ERROR: PDP is not for the factor specified in argument x')
 
 ## Create the base data
 if(!is.null(w)) w = df[,w]
 baseData = create_ave_plot_data(x = df[,x],y_true = df[,y],y_pred = preds,w = w)
 
 ## Change the name of the key
 names(baseData)[which(names(baseData) == "x")] = x
 
 ## join it all together
 allData = merge(x = baseData,y = pdp,by = x,all = TRUE)
 allData$xvals = seq(1,nrow(allData))
 
 ## Fit the spatial smoothing
 sspline <- smooth.spline(x = allData$xvals,y = allData$yhat,w = allData$sum_weight,...)
 allData$Smoothed = predict(sspline,x = allData$xvals)$y
 allData$adjustment = allData$Smoothed / allData$yhat
 
 return(allData)
}
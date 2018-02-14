#' xgb_band
#'
#' A wrapper function that automates the xgboost training process
#' @param dtrain     A data.frame that contains the training data
#' @param x          A list of column names that identify the explanatory features 
#' @param y          A column name that identifies that target variable
#' @param w          A column name that contains the name of the weight column. Defaults to NULL
#' @param max_levels Whats the maximum number of levels the model should create
#' @param xgbParams  A named list containing the XGBoost Learning Parameters
#' @param ...        Additional Arguments to be passed to train_xgboost
#' @keywords banding xgboost
#' @export
#' @examples
#' 

xgb_band <- function(dtrain,x,y,w=NULL,max_levels,xgbParams,...) {
  
  ## Deal with max_levels - using number of leaves = max_depth^2
  max_depth = floor(sqrt(max_levels))
  xgbParams$max_depth = max_depth
  
  ## Train the model
  mdl = train_xgboost(dtrain = dtrain,
                      x = x,
                      y = y,
                      w = w,
                      xgbParams = xgbParams,
                      ...)
  
  ## Extract the leaf information
  leafPreds = predict(object = mdl$finalModel,newdata = mdl$dtrain,predleaf = TRUE)[,mdl$opt_nrounds]
  
  ## Build the mapping table
  dtWorking = data.table::data.table(leafPreds = leafPreds,xval = dtrain[,x])
  dtWorking = dtWorking[ , list(start = min(xval),end = max(xval)) , by = leafPreds][order(start),]
  
  ## Make sure the bins are contiguous
  for(i in 1:(nrow(dtWorking)-1)){
    dtWorking[i,'end'] = dtWorking[(i+1),'start'] - 1e-8
  }
  
  ## Create a binned feature
  dtWorking$bin = seq(1,nrow(dtWorking))
  dtWorking$leafPreds <- NULL

  ## Return the info
  return(dtWorking)
}

#' apply_xgb_band
#'
#' A helper function to apply the mapping table to new data
#' @param x       A list of column names that identify the explanatory features 
#' @param mapping A column name that identifies that target variable
#' @keywords apply xgboost banding
#' @export
#' @examples
#' 
apply_xgb_band <- function(x,mapping){
  
  ## Make sure the mapping table is well formed
  if(class(mapping)[1] != 'data.table') mapping = data.table::as.data.table(mapping)
  if(length(setdiff(names(mapping),c('start','end','bin')))>0) stop("ERROR: Mapping table should contain the following: start,end,bin")
  data.table::setkey(mapping,start,end)
  
  ## Coerce to data.table
  dtWorking = data.table::data.table(x = x,rowIndex = seq(1,length(x)))
  dtWorking[ , c('start','end'):= x ]
  data.table::setkey(dtWorking,start,end)

  ## Do the lookup on the mapping table
  dtWorking$assignment = mapping$bin[data.table::foverlaps(dtWorking,mapping,which=TRUE)$yid]
  
  ## Get it back into the right order
  ret = dtWorking %>% dplyr::arrange(rowIndex) %>% dplyr::select(assignment) %>% unlist() %>% as.numeric()

  ## Build the start & end variables in the table to be mapped
  return(ret)
  
}



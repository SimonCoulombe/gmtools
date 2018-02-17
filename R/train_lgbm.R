#' train_lightgbm
#'
#' A wrapper function that automates the lightGBM training process
#' @param dtrain                 A data.frame that contains the training data
#' @param dvalid                 A data.frame that contains the validation data. If NULL then the function uses cross validation. Defaults to NULL.
#' @param x                      A list of column names that identify the explanatory features 
#' @param y                      A column name that identifies that target variable
#' @param w                      A column name that contains the name of the weight column. Defaults to NULL
#' @param LgbParams              A named list containing the XGBoost Learning Parameters
#' @param nrounds                The maximum number of iterations 
#' @param early_stopping_rounds  If performance doesnt improve for this many rounds then stop training
#' @param nfold                  How many folds to use if doing cross validation   
#' @param folds                  A list of pre defined fold indicies (test indicies) - see main xgboost docs for more details.
#' @param verbose                Print run time messages  
#' @param seed                   An integer which will be used as the random seed. Defaults to 1921
#' @param ...                    Additional Arguments to be passed to xgb.cv and/or xgb.train     
#' @keywords lightgbm train
#' @import lightgbm
#' @export
#' @examples
#' 

train_lightgbm <- function(dtrain,dvalid=NULL,x,y,w=NULL,LgbParams,nrounds=5000,stratified=FALSE,early_stopping_rounds=5,nfold = 5,folds=NULL,verbose=1,seed=1921,...){
    
  ## Are we doing CV?
  is_cv = is.null(dvalid)
  null_weights = is.null(w)
  
  ## Make sure input is data.frame & deal with null weights
  if(verbose) message('>>>>> Checking data')
  dtrain = as.data.frame(dtrain)
  
  if(null_weights){ w = 'w'
                    dtrain$w = rep(1,nrow(dtrain)) }
  if(!is_cv) { dvalid = as.data.frame(dvalid)
               if(null_weights) dvalid$w = rep(1,nrow(dvalid))}
  
  ## Create the various matricies
  if(verbose) message('>>>>> Creating Matricies')
  dtrain = lgb.Dataset(data = as.matrix(dtrain[,x]),label = dtrain[,y],weight = dtrain[,w])
  if(!is_cv) dvalid = lgb.Dataset(data = as.matrix(dvalid[,x]),label = dvalid[,y],weight = dvalid[,w])
  
  ## Add the seeds into the parameters list if they dont exist
  if(is.null(lgbParams$bagging_seed)) lgbParams$bagging_seed = seed
  if(is.null(lgbParams$feature_fraction_seed)) lgbParams$feature_fraction_seed = seed

  ## If we're doing CV pick nrounds
  if(is_cv){ if(verbose) message('>>>>> Fitting CV Model')
  			 set.seed(seed)
             LgbCV = lightgbm::lgb.cv(params  = LgbParams,
                                      data    = dtrain,
                                      nrounds = nrounds,
                                      record  = TRUE,
                                      early_stopping_rounds = early_stopping_rounds,
                                      stratified = stratified,
                                      nfold   = nfold,
                                      folds   = folds,
                                      verbose = verbose,
                                      ...) } else LgbCV = NULL 
  
  ## Sort out the parameters
  if(verbose) message('>>>>> Extracting Parameters')
  opt_nrounds = ifelse(is_cv,LgbCV$best_iter,nrounds)
  watchlist = if(is_cv) list(train = dtrain) else list(train = dtrain,valid = dvalid)
  early_stopping_rounds = if(is_cv) NULL else early_stopping_rounds
 
  ## Now train the final model
  if(verbose) message('>>>>> Training Final Model')
  set.seed(seed)
  finalModel = lightgbm::lgb.train(params    = LgbParams,
                                   data      = dtrain, 
                                   nrounds   = opt_nrounds,
                                   valids    = watchlist,
                                   early_stopping_rounds = early_stopping_rounds,
                                   verbose = verbose,
                                   ...)
  
  ## Return final model
  return(list(finalModel=finalModel,
              LgbCV = LgbCV,
              dtrain=dtrain,
              dvalid=dvalid,
              fin_early_stopping_rounds = early_stopping_rounds,
              opt_nrounds=opt_nrounds,
              is_cv = is_cv,
              null_weights=null_weights))
}
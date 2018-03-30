#' xgb_autotune
#'
#' Run an automatic bayesian optimisation of xgboost
#' @keywords xgboost autotune
#' @param dtrain      The training data for the optimisation
#' @param x           A list that identifies the features
#' @param y           A string that identifies the label
#' @param w           A string that identifies the weight column. Defaults to NULL.
#' @param base_margin A string that identifies the base_margin (offset). Defaults to NULL
#' @param xgbParams   A list of extra parameters to be passed to xgb.cv
#' @param nrounds     the max number of iterations
#' @param early_stopping_rounds If set to an integer k, training with a validation set will stop if the performance doesn't improve for k rounds. 
#' @param nfold       the original dataset is randomly partitioned into nfold equal size subsamples.
#' @param folds       list provides a possibility to use a list of pre-defined CV folds (each element must be a vector of test fold's indices).
#' @param verbose     Whether or not to print progress.
#' @param seed        Random State
#' @param maximize    Should the loss function be maximized or not?
#' @param bounds      A named list of lower and upper bounds for each hyperparameter. Please use "L" suffix to indicate integer hyperparameter.
#' @param init_points Number of randomly chosen points to sample the target function before Bayesian Optimization fitting the Gaussian Process.
#' @param n_iter      Total number of times the Bayesian Optimization is to repeated.
#' @param init_grid_dt User specified points to sample the target function, should be a data.frame or data.table with identical column names as bounds. 
#' @param ...         Additional args to be passed to BayesOptim
#' @import rBayesianOptimization
#' @export
#' @examples
#' 

xgb_autotune <- function(dtrain,x,y,w=NULL,base_margin=NULL,xgbParams,nrounds,early_stopping_rounds,nfold,folds=NULL,verbose=TRUE,seed = 1921,maximize=FALSE,bounds,init_points,n_iter,init_grid_dt=NULL,...) {
 
  ## Stop the function from compiling for now
  ## stop('The Codez are not ready yet')

  ## Define function here - therefore it will inherit the other params (xgbParams etc) from the calling scope
  xgb_cv_wrapper <- function(max_depth=NULL,min_child_weight=NULL,gamma=NULL,colsample_bytree=NULL,subsample=NULL,lambda=NULL,max_delta_step=NULL){
    
      ## make sure that max_depth is an integer
      if(!is.null(max_depth)) xgbParams$max_depth = round(max_depth,digits = 0)
      if(!is.null(min_child_weight)) xgbParams$min_child_weight = min_child_weight
      if(!is.null(gamma)) xgbParams$gamma = gamma
      if(!is.null(colsample_bytree)) xgbParams$colsample_bytree = colsample_bytree
      if(!is.null(subsample)) xgbParams$subsample = subsample
      if(!is.null(lambda)) xgbParams$lambda = lambda
      if(!is.null(max_delta_step)) xgbParams$max_delta_step = max_delta_step
      
      ## Fit the model
      set.seed(seed)
      xgbCV = xgb.cv(params = xgbParams,
                     data = dtrain,
                     nrounds = nrounds,
                     nfold = nfold,
                     folds = folds,
                     maximize = maximize,
                     prediction = TRUE,
                     early_stopping_rounds = early_stopping_rounds,
                     verbose = verbose)
      
      ## Output the results
      if(maximize==FALSE) Score = -xgbCV$evaluation_log[xgbCV$best_iteration, 4] else Score = xgbCV$evaluation_log[xgbCV$best_iteration, 4]
      return(list(Score = Score,Pred = xgbCV$pred))
   }
  
  ## Create the dmatricies
  dtrain = xgb_create_dmatrix(data = dtrain,x = x,y = y,w = w,base_margin = base_margin)  
  #if(!is.null(dvalid)) dvalid = xgb_create_dmatrix(data = dvalid,x = x,y = y,w = w,base_margin = base_margin)

  ## Run the optimisation
  set.seed(seed)
  bayesOpt = BayesianOptimization(FUN = xgb_cv_wrapper,bounds = bounds,init_grid_dt = init_grid_dt,n_iter = n_iter,init_points = init_points,verbose = verbose)
  
  ## Return the info
  return(list(BayesOptim = bayesOpt,BstXgbParams = c(xgbParams,unlist(bayesOpt$Best_Par))))
}




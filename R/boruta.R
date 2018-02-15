#' .create_shadow
#'
#' Internal function that creates a randomly shuffled copy of a vector
#' @param x The vector to be shuffled
#' @keywords create shadow
#' @export
#' @import xgboost
#' @examples
#' 
.create_shadow <- function(x){ x[sample(x = seq(1,length(x)),size = length(x))] }

#' create_shadow_features
#'
#' Wrapper for .create_shadow to make creating a large number of shadow features easy
#' @param df       A data.frame (or coercible object) that contains the features to be shadowed
#' @param features A list of feature names to be processed
#' @keywords create shadow
#' @export
#' @examples
#' 
create_shadow_features <- function(df,features){
  
  ## Check if df is the right type
  if(class(df)[1] != 'data.frame') df = as.data.frame(df)
  
  ## Begin looping over df and creating the features
  for(i in 1:length(features)){
    f = features[i]
    df[,paste('shadow',f,sep='_')] = .create_shadow(x = df[,f])
  }
  
  ## Return the new shadow feature frame
  return(df)
}

#' get_feat_names
#'
#' Noddy function to get shadow feature names
#' @param features A list of feature names to be processed
#' @keywords shadow names
#' @export
#' @examples
#' 
get_feat_names <- function(features){ c(features,paste0('shadow_',features))  }

#' process_importances
#'
#' A function to aggregate a list of importances into some useful feature selection information
#' @param importances a list of xgboost style importances
#' @keywords process importances
#' @export
#' @examples
#' 
process_importances <- function(importances){
  
  ## Bind them all together into a frame
  importances = do.call(rbind,importances)
  
  ## Squash the data
  smry = importances[ , list(meanGain = mean(Gain),sdGain = sd(Gain),length = length(Gain)) , by = Feature]
  smry$se = smry$sdGain / sqrt(smry$length) ##Make a standard error
  smry$parent = gsub(pattern = 'shadow_',replacement = '',x = smry$Feature)
  
  ## Pull the shadows and reals apart and put them back together
  shadowIdx = grepl(pattern = 'shadow',x = smry$Feature)
  comparisons = merge(x    = smry[!shadowIdx,],
                      y    = smry[shadowIdx,c('meanGain','se','parent') ],
                      by.x = 'Feature',
                      by.y = 'parent',
                      all.x= TRUE)
  comparisons$class = ifelse((comparisons$meanGain.x - 1.96*comparisons$se.x)>(comparisons$meanGain.y + 1.96*comparisons$se.y),1,0)
  ret = data.frame(Feature        = comparisons$Feature,
                   RealMeanGain   = comparisons$meanGain.x,
                   RealSEGain     = comparisons$se.x,
                   ShadowMeanGain = comparisons$meanGain.y,
                   ShadowSEGain   = comparisons$se.y,
                   class          = comparisons$class) %>% arrange(-RealMeanGain)
  return(ret)
  
}

#' xgboost_boruta
#'
#' Xgboost flavoured boruta implementation for feature selection. WARN: This method is all brute force so training time can be long.
#' @param df        A data.frame (or coercible object) that contains the training data
#' @param x         A list of feature names to be appraised
#' @param y         The name of the target column
#' @param w         The name of the weights column. defaults to NULL.
#' @param niter     How many xgboost models should we train
#' @param xgbParams List of learning parameters to be passed to XGBoost
#' @keywords xgboost boruta
#' @export
#' @examples
#' 
xgboost_boruta <- function(df,x,y,w=NULL,niter = 10,xgbParams){

  ## Load required packages
  ## suppressPackageStartupMessages(requireNamespace("xgboost"))
  
  ## Create all the shadow features
  message('>>>>>> Creating Shadow Features')
  dfShadow = create_shadow_features(df = df,features = x)
  useFeatures = get_feat_names(features = x)
  
  ## Create the data for use in the loop
  message('>>>>>> Creating Training Matrix')
  if(is.null(w)) w = rep(1,length(y))
  dtrain = xgboost::xgb.DMatrix(data = as.matrix(dfShadow[,useFeatures]),label = y,weight = w)
  
  ## Begin the iterative loop
  init_seed = 1988; 
  importances = lapply(seq(1,niter),function(i){
    
    ## Print run message
    message('>>>>>>> Running Iteration: ',i)
    
    ## Fit the model
    set.seed(init_seed+i)
    xgbCV = xgboost::xgb.cv(params  = xgbParams,
                            data    = dtrain,
                            nrounds = 1000,
                            nfold   = 5,
                            print_every_n = 10,
                            early_stopping_rounds = 5,
                            verbose = FALSE)
    
    set.seed(init_seed+i)
    xgbFinal = xgboost::xgb.train(params    = xgbParams,
                                  data      = dtrain,
                                  nrounds   = xgbCV$best_iteration,
                                  verbose = FALSE,
                                  #watchlist = list(train = dtrain),
                                  print_every_n = 10)
    
    silent = gc(verbose = FALSE)
    
    importance = xgboost::xgb.importance(feature_names = useFeatures,model = xgbFinal)
    return(importance)
  })
  
  procImportance = process_importances(importances = importances)
  return(procImportance)
}



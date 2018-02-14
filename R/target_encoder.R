## Function adapted from Kaggle post
## https://www.kaggle.com/ogrellier/python-target-encoding-for-categorical-features

#' .add_noise
#'
#' This function injects gaussian noise into a numeric series
#' @param series      The series to inject noise to
#' @param noise_level The level of noise expressed as a proportion of the original series
#' @keywords add noise
#' @export
#' @examples
#' 
.add_noise <- function(series,noise_level){
  return(series * (1 + noise_level * rnorm(n = length(series))))
}

#' .create_smoothing
#'
#' This function smooths a table of category means using an exponential smoothing
#' @param weights          A vector that contains the weight in each category
#' @param min_samples_leaf A scalar that represents the minimum amount of weight deemed to be credible
#' @param smoothing        A scalar that defines the level of smoothing
#' @keywords create smoothing
#' @export
#' @examples
#'
.create_smoothing <- function(weights,min_samples_leaf,smoothing){
  ret = 1 / (1 + exp(-(weights- min_samples_leaf)/smoothing))
  return(ret)
}

#' .target_encode
#'
#' A worker function that does the target encoding of a column
#' @param trn_x             A vector that contains the original x values for the train data
#' @param tst_x             A vector that contains the x values for the test data
#' @param trn_y             A vector that contains the train set actuals
#' @param trn_w             A vector that contains row level weights. Defaults to NULL
#' @param min_samples_leaf  Whats the smallest amount of weight that's deemed to be credible?
#' @param smoothing         A parameter that controls the aggressiveness of the smoothing
#' @param noise_level       How much noise should be injected into the results?
#' @keywords target encode
#' @export
#' @examples
#' 
.target_encode <- function(trn_x,tst_x,trn_y,trn_w = NULL,min_samples_leaf=1,smoothing = 1,noise_level = 0){
  
  ## Load required packages
  suppressPackageStartupMessages(requireNamespace("data.table"))
  
  ## Deal with null weights
  if(is.null(trn_w)) trn_w = rep(1,length(trn_x))
  
  ## Compute the group averages
  dtWorking = data.table(xval = trn_x,yval = trn_y,wval = trn_w)
  averages = dtWorking[ , list( wmean_target = weighted.mean(x = yval,w = wval),
                                sum_weight   = sum(wval)) 
                        , by = xval][order(xval),]
  
  ## Compute smoothing factor
  smoothing = .create_smoothing(weights = averages$sum_weight,min_samples_leaf = min_samples_leaf,smoothing = smoothing)
  
  ## Calculate prior
  prior = mean(dtWorking$yval)
  
  ## Adjust the calculated average using the smoothing term & prior
  averages[,'wmean_target'] = prior*(1 - smoothing) + averages[,'wmean_target']*smoothing
  
  ## Do the mapping to the test series and apply some random noise
  ret = .add_noise(averages[match(tst_x,averages$xval), 'wmean_target']$wmean_target,noise_level = noise_level)
  
  ## return the mapping
  return(ret)
}

#' target_encoder
#'
#' A wrapper for .target_encode that makes target encoding a large number of columns easy
#' @param train_df A data.frame (or coercible object) that contains the training set columns to be encoded
#' @param test_df  A data.frame (or coercible object) that contains the test set columns to be encoded
#' @param y        A string containing the name of the target column
#' @param x        A string containing the name of the xvalue column
#' @param w        A string containing the name of the weight column. Defaults to NULL.
#' @param verbose  A logical indicating whether to print run time progress messages or not.
#' @param ...      Further arguments to be passed to .target_encode
#' @keywords target encode
#' @export
#' @examples
#' 
target_encoder <- function(train_df,test_df,y,x,w=NULL,verbose=FALSE,...){
  
  ## Do some parameter checking
  if(class(train_df)[1] != 'data.frame') train_df = as.data.frame(train_df)
  if(class(test_df)[1] != 'data.frame') test_df = as.data.frame(test_df)
  
  ## Deal with NULL weights
  if(is.null(w)) wvec = rep(1,nrow(train_df)) else wvec = train_df[,w]

  ## Do the encoding
  encoded_vals = do.call(cbind,lapply(X = x,function(f){
    
    index = which(x == f); if(verbose & (index == 1 | index %% 10 == 0)) message('>>>>>> iteration ',index,'/',length(x))

    ret = data.frame(eval = .target_encode(trn_x = train_df[,f],tst_x = test_df[,f],trn_y = train_df[,y],trn_w = wvec,...))
    names(ret) = paste0('enc_',f)
    return(ret)
    
  }))
  
  ## return the encoded information to the user
  return(encoded_vals)
}




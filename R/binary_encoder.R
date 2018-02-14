
#' .binary_encode
#'
#' Internal function that does binary encoding on a single column. Can give good compression on high cardinality columns.
#' @param x      A vector that contains the information to be encoded
#' @param nbits  How many bits should be encode the information (32 is a good number ;))
#' @keywords     binary encode
#' @export
#' @examples
#' 

.binary_encode <- function(x,nbits=32){
  
  ## Deal with NAs if present
  x = factor(x,exclude = NULL)
  
  ## Find out how many bits are required to store all the info
  bitsReq = ceiling(log(length(unique(x)) + 1)/log(2))
  
  ## Binary Encode the feature
  binEncode = matrix(as.integer(intToBits(as.integer(x))),
                     ncol  = nbits,
                     nrow  = length(x),
                     byrow = TRUE)[, 1:bitsReq]
  
  ## Return the info
  return(binEncode)
}

#' binary_encoder
#'
#' Wrapper function that makes binary encoding of a large number of columns easy
#' @param df       A data.frame or coercible object that contains the features to be encoded
#' @param features A list of names that represent the features to be encoded
#' @param nbits    How many bits should be encode the information (32 is a good number ;))
#' @keywords       binary encode
#' @export
#' @examples
#' 

binary_encoder <- function(df,features,nbits=32){
  
  ## Check that the input is a data.frame
  if(!class(df)[1] == 'data.table') df = as.data.frame(df)
  
  ## Loop over the features and create binary encodings
  encoded = do.call(cbind,lapply(features,function(f){
    benc = as.data.frame(.binary_encode(x     = df[,f],
                                        nbits = nbits))
    names(benc) = paste(f,seq(1,ncol(benc)),sep = '_')
    return(benc)
  }))
  
  ## Return the information
  return(encoded)
}


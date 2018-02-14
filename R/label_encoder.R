#' .label_encode
#'
#' Internal function that label encodes a column
#' @param x A vector containing the information to be label encoded
#' @keywords label encode
#' @export
#' @examples
#'

## Internal worker function
.label_encode <- function(x){ as.numeric(factor(x = x,exclude = NULL)) }

#' label_encoder
#'
#' Wrapper for .label_encode to aid in the label encoding of many columns
#' @param df       A data.table (or coercible object) that contains the columns to be encoded
#' @param features A list of column names to be encoded
#' @keywords label_encoder
#' @export
#' @examples
#'

## Frame encoder
label_encoder <- function(df,features){
  
  ## Check data.table is loaded
  suppressPackageStartupMessages(requireNamespace("data.table"))
  
  ## Convert the input object to a data.frame
  if(class(df)[1] != "data.table") df = as.data.table(df)
  
  ## Apply the label encoder internal function
  df[ , (features):=lapply(.SD,.label_encode) , .SDcols = features]   
  
  ## Return the encoded frame
  return(df)
}


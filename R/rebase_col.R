#' rebase_col
#'
#' A function that rebases a column to another column. Accepts row weights.
#' @param x    The column that you wish to rebase
#' @param base The column that you wish to rebase x to
#' @param w    A vector of row weights. Defaults to NULL
#' @keywords rebase col
#' @export
#' @examples
#' 

rebase_col <- function(x,base,w=NULL){
  
  ## Deal with null weight vector
  if(is.null(w)) w = rep(1,length(x))
  
  ## Check series are the same length
  if((length(x)+length(w)+length(base)) != 3*length(x)) stop('ERROR: Series are not the same length')
  
  ## Check for NAs in either series
  if(anyNA(data.frame(x = x,base = base,w = w))) stop('ERROR: NAs present in arguments passed to rebase_col')
  
  ## Calculate the rebasing factor and apply
  x = x * (sum(base*w)/sum(x*w))
  
  ## Return the info
  return(x)
}


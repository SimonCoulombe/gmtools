#' remove_colinear_features
#'
#' Function remove colinear features
#' @param df        A data.frame that contains the columns to be processed
#' @param features  A character vector containing the colnames to be processed
#' @param threshold What level of correlation is too high? Defaults to 0.75 
#' @param ...       Additional arguments to be passed to stats::cor
#' @keywords        remove colinear
#' @export
#' @examples
#' 

  remove_colinear_features <- function(df,features,threshold=0.75,...){
    
    ## TODO: Add a method argument so we can h2o's parrallel correlation function

    ## Make sure that the input is a data.frame
       if(class(df)[1] != 'data.frame') df = as.data.frame(df)
    
    ## Create the correlation matrix
       m = cor(x = df[,features],...)
       
    ## Bind the results back into a data.frame
       q = data.frame(row=rownames(m)[row(m)[upper.tri(m)]], 
                      col=colnames(m)[col(m)[upper.tri(m)]], 
                      corr=m[upper.tri(m)])
       
    ## Define some working lists
       inc_features = features; rem_features = character()
    
    ## Filter the correlation matrix 
       qthresh = q[ abs(q$corr) > threshold, ]
       
    ## Work out what features were going to use
    ## TODO: Find a more efficient way to do this 
       for(i in 1:nrow(qthresh)){
         if(qthresh[i,"row"] %in% rem_features) removal = qthresh[i,"row"] else removal = qthresh[i,"col"]
         rem_features = c(rem_features,as.character(removal))   
       }
       
    ## Final lists and return
       inc_features = setdiff(features,rem_features)
       return(list(correlation_matrix = m,included_features = inc_features))
  }
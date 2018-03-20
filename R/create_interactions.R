#' .prepare_interaction
#'
#' Internal function that prepares terms for interaction creation
#' @param df A data.frame that contains the interaction terms to be prepared
#' @param intTerms A list of names that identify the columns in df to be prepared
#' @param nlvl Max number of levels allowed in an interaction term before its binned
#' @param ... Additional arguments to be passed to the binnarise function
#' @keywords prepare interaction
#' @export
#' @examples
#' 

.prepare_interaction <- function(df,intTerms,nlvl,...){
   ret = as.data.frame(lapply(intTerms,function(t){
     needs_binning = length(unique(x = df[,t])) > nlvl #Does the feature need binning?
     if(needs_binning & (class(df[,t]) %in% c('character','factor'))) stop('ERROR: Character feature cannot be binned')
     if(needs_binning) ret = binnarise(x = df[,t],...) else ret = df[,t] 
     return(ret)  
   }))
   return(ret)
 }


#' make_interaction
#'
#' Function that creates an interaction term from a recipe
#' @param df A data.frame that contains the interaction terms to be prepared
#' @param recipe A character string containing interaction terms delimited by | e.g. "age|sex". 
#' @param nlvl Max number of levels allowed in an interaction term before its binned
#' @param ... Additional arguments to be passed to the prepare interaction function
#' @keywords make interaction
#' @export
#' @examples
#' 

make_interaction <- function(df,recipe,nlvl,...){
  
  ## Define some meta information for the interaction
  nways   = nchar(recipe) - nchar(gsub(pattern = '|',replacement = '',fixed = TRUE,x = recipe)) + 1
  intName = gsub(pattern = '|',replacement = '_X_',x = recipe,fixed = TRUE)
  intTerms= unlist(strsplit(x = recipe,split = '|',fixed = TRUE))
  
  ## Define all the interaction components - binning if required
  intCols = .prepare_interaction(df = df,intTerms = intTerms,nlvl = nlvl,...)
  
  ## Create the output in the right format
  names(intCols) = intTerms
  intVal  = as.numeric(factor(apply(X = intCols,MARGIN = 1,function(x){paste(x,collapse = '_')}),exclude = NULL))
  intVal  = as.data.frame(intVal)
  names(intVal) = intName
  
  ## Return the information
  return(list(intVal = intVal,nways = nways,intName = intName))
}

#' create_interactions
#'
#' Wrapper function for make_interaction - assists in the creation of many interaction terms
#' @param df A data.frame that contains the interaction terms to be prepared
#' @param intList A list that contains a number of recipes from which to create interactions. Recipes should be character strings delimited by |'s. E.g. "Age|Sex"
#' @param nlvl Max number of levels allowed in an interaction term before its binned
#' @param ... Additional arguments to be passed to the make_interaction
#' @keywords create interactions
#' @export
#' @examples
#'

create_interactions <- function(df,intList,nlvl=25,...){
  
 ## Loop over interactions
 interactions = do.call(cbind,lapply(X = intList,
                              function(i){ temp = make_interaction(df = df,
                                                                   recipe = i,
                                                                   nlvl = nlvl,...)
                                           ret  = temp$intVal
                                           names(ret) = temp$intName
                                           return(ret)
                                           }))
 
 ## Return the info
 return(interactions)
  
}
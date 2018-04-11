#' One Hot Encoder
#'
#' This function one-hot encodes a list of features
#' @param df       A data.frame (or coercible) object that contains the columns to be encoded
#' @param features A list of names that identify the columns to be encoded
#' @keywords one hot encode
#' @export
#' @import caret
#' @examples
#'

one_hot_encoder <- function(df,features){
  if(class(df)[1] != "data.frame") df = as.data.frame(df)
  frmla   = as.formula(paste0('~ ',paste(features,collapse = ' + ')))
  oh_xfrm = caret::dummyVars(formula = frmla,data = df,sep = '.')
  oh_dat  = predict(oh_xfrm,df)
  #keepCol = names(df)[-which(names(df) %in% features)]
  #df      = cbind(df[,keepCol],oh_dat)
  df      = cbind(df,oh_dat)
  return(df)
}


# mtcars$carb = as.factor(mtcars$carb)
# mtcars$gear = as.factor(mtcars$gear)
# aa = one_hot_encoder(df = mtcars,features = c('carb','gear'))  
# str(aa)
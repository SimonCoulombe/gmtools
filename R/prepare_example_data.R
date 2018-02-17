#' prepare_example_data
#'
#' Internal function that label encodes a column
#' @keywords example data
#' @export
#' @examples
#'

prepare_example_data <- function(){
   ## Get test data
   data(dataCar,package = 'insuranceData')
   features = c("veh_value","veh_body","veh_age","gender","area","agecat")
   lbl_features = c("veh_body","gender","area","agecat")
  
   ## Encode the data
   encoded_data = label_encoder(df = dataCar,features = lbl_features)
   encoded_data$freq = encoded_data$numclaims / encoded_data$exposure
  
   ## Return the info
   return(list(data = encoded_data,features = features))
}

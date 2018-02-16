#' load_h2o_data
#'
#' Function to load R objects into h2o.Frames - built the write/read trick under certain circumstances.
#' @param data       An r data.frame to be transfered into the h2o cluster
#' @param key        The character key that will be used to identify the h2o.frame in the cluster
#' @param write_read A logical allowing the user to specify which method to be used
#' @keywords h2o load data
#' @export
#' @import h2o slam
#' @importFrom utils object.size
#' @importFrom data.table fwrite
#' @examples
#' 

load_h2o_data <- function(data,key,write_read=NULL){

  ## How big is the data?
     data_size = object.size(x = data) / 1024**2 ##Object size in Mb
     if(data_size == 0) return(NULL)
     write_read = ifelse(is.null(write_read),data_size > 1,write_read)

     if(write_read){
       ## If its large then the old write/read might be quicker
       
       ## Write the file to disk
       og_loc = tempfile()
       data.table::fwrite(x = data,file = og_loc)

       ## reimport the file using h2o.importFile
       assign(x = key,h2o.importFile(path = og_loc,destination_frame = key))
     } else {
       ## Else we can use as.h2o method
          options("h2o.use.data.table"=TRUE)
          assign(x = key,value = as.h2o(x = data,destination_frame = key))
     }
    
  ## Return the object
     return(get(x = key))
}

#' get_h2o_predictions
#'
#' Function to return predictions from an h2o model to a local r object
#' @param mdl     An h2o model object to predict from
#' @param newdata A new data set to make predictions on. Defaults to NULL
#' @param xval    Do you want to extract the out of fold predictions? Defaults to FALSE. Note: keep_cross_validation_predictions must equal TRUE in the model object
#' @keywords h2o predict
#' @export
#' @import h2o 
#' @examples
#' 

get_h2o_predictions <- function(mdl,newdata=NULL,xval=FALSE){

  ## Check to see if we have the predictions to return
  if(xval & mdl@allparameters$keep_cross_validation_predictions==FALSE) stop('ERROR: Requested OOF Preds but keep_cross_validation_predictions = FALSE')

  ## Extract the predictions from the h2o model
  if(xval) {
    h2o_preds = h2o.getFrame(mdl@model[["cross_validation_holdout_predictions_frame_id"]][["name"]])
  } else {
    h2o_preds = h2o.predict(mdl,newdata) }

  ## Return the values as a native r object
  ret = as.numeric(as.data.frame(h2o_preds)$predict)
  return(ret)

}

#' fit_h2o_mdl
#'
#' Function to fit any exposed h2o model method. This is a simple wrapper to the h2o api method.
#' @param algo             The name of the function used to train the algorithm i.e. "h2o.glm". Defaults to "h2o.glm"
#' @param x                A list of names indicating the features to be used for training   
#' @param y                A character indicating the target column
#' @param weights_column   A character indicating the column to be used as row level weights
#' @param training_frame   An r object pointing to the training data
#' @param validation_frame An r object pointing to the validation data
#' @param ...              Additional arguments that will be passed to the underlying training function
#' @keywords h2o mdl fit
#' @export
#' @import h2o 
#' @examples
#' 

fit_h2o_mdl <- function(algo='h2o.glm',x,y,weights_column=NULL,training_frame,validation_frame=NULL,...){

  ## Connect to Cluster
     local_h2o = h2o::h2o.init()

  ## Get the data into the cluster
     int_train_data = load_h2o_data(data = training_frame  ,key = 'int_train_data')
     int_valid_data = load_h2o_data(data = validation_frame,key = 'int_valid_data')

  ## Fit the h2o mdl
     mdlParams = list(x = x,
                      y = y,
                      weights_column  = weights_column,
                      training_frame  = int_train_data,
                      validation_frame= validation_frame,
                      ...)

     fittedMDL = do.call(what = algo,args = mdlParams)

  ## return the fitted model object
     return(fittedMDL)
}






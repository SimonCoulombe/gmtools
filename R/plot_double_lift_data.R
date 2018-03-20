#' plot_double_lift_data
#'
#' A companion plotting function to create_double_lift_data. It calls the aggregator and then makes a nice plot.
#' @param modelA A vector that contains the predictions for Model A - preds will be pro-rated
#' @param modelB A vector that contains the predictions for Model B - preds will be pro-rated
#' @param actual A vector that contains the actuals that the models are to be compared to
#' @param weight A vector that contains the row level weights (exposure,claim count). Defaults to NULL
#' @param nbins Passed to the binnarise function - how many equally sized bins should the ratio be cut into
#' @param retLabel Keep string labels from binnarise
#' @keywords plot double lift
#' @export
#' @import ggplot2
#' @examples
#' 

plot_double_lift_data <- function(modelA,modelB,actual,weight=NULL,nbins=10,retLabel=FALSE){
  
 ## Build the data for plotting  
 plotData = create_double_lift_data(modelA = modelA,
                                    modelB = modelB,
                                    actual = actual,
                                    weight = weight,
                                    nbins  = nbins,
                                    retLabel = retLabel)
 
 ## Scale the weight so we get it all on the same axes
 plotData$scaled_weight = plotData$sum_weight * (mean(plotData$mean_actual)/max(plotData$sum_weight)) 
 
 ## Build the plot
 thePlt = ggplot(data = plotData,aes(x = bin)) + 
           geom_point(aes(y = mean_modelA,colour = 'modelA')) + 
           geom_line(aes(y = mean_modelA,colour = 'modelA'))  + 
           geom_point(aes(y = mean_modelB,colour = 'modelB')) + 
           geom_line(aes(y = mean_modelB,colour = 'modelB'))  + 
           geom_point(aes(y = mean_actual,colour = 'actual')) + 
           geom_line(aes(y = mean_actual,colour = 'actual'))  + 
           geom_bar(aes(y = scaled_weight),stat = 'identity',fill = 'yellow',alpha = 0.3) + 
           scale_color_manual(values = c('red','blue','black','yellow'))
 
 ## Return the info
 return(thePlt)  
}


# require(insuranceData); data(dataCar)
# mdl  = glm(formula = I(numclaims/exposure) ~ veh_value + gender + agecat,data = dataCar,weights = dataCar$exposure,family = poisson)
# mdl2 = glm(formula = I(numclaims/exposure) ~ agecat,data = dataCar,weights = dataCar$exposure,family = poisson)
# plot_double_lift_data(modelA = exp(predict(mdl,dataCar)),
#                       modelB = exp(predict(mdl2,dataCar)),
#                       actual = dataCar$numclaims,
#                       weight = dataCar$exposure)
# 

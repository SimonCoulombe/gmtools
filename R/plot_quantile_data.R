#' plot_quantile_data
#'
#' A companion function to create_quantile_plot_data. It runs the aggregator and then makes a nice plot.
#' @param y_true The actuals you want to compare your model against
#' @param y_pred The predictions that you want to compare the actuals to - these will be pro-rated.
#' @param w      A column of row level weights (e.g. exposure, claim count). Defaults to NULL
#' @param nbins  How many quantiles do you want to create? Defaults to 10
#' @keywords plot quantile
#' @export
#' @import ggplot2
#' @examples
#' 

plot_quantile_data <- function(y_pred,y_true,w=NULL,nbins=10){
  
  ## Build the plot data
  plotData = create_quantile_plot_data(y_true = y_true,y_pred = y_pred,w = w,nbins = nbins)

  ## Make the scaled weight column
  plotData$data$scaled_weight = plotData$data$sum_weight * (mean(plotData$data$wmean_pred)/max(plotData$data$sum_weight))

  ## Make the plot
  thePlt = ggplot(data = plotData$data,aes(x = bin)) +
            geom_point(aes(y = wmean_pred,colour = 'Fitted')) +
            geom_line(aes(y = wmean_pred,colour = 'Fitted'))  +
            geom_point(aes(y = wmean_true,colour = 'Actual')) +
            geom_line(aes(y = wmean_true,colour = 'Actual'))  +
            geom_bar(aes(y = scaled_weight),stat = 'identity',fill = 'yellow',alpha = 0.3) + 
            theme(legend.title = element_blank(),
                  legend.position = "bottom",
                  legend.text = element_text(size = 15)) + 
            scale_color_manual(values = c('red','blue','yellow'))
            

  ## Return the plot
  return(thePlt)
}

# require(insuranceData); data(dataCar)
# mdl = glm(formula = I(numclaims/exposure) ~ veh_value + gender + area + agecat,data = dataCar)
# plot_quantile_data(y_pred = exp(predict(mdl,dataCar)),
#                    y_true = dataCar$numclaims,
#                    w = dataCar$exposure,
#                    nbins = 10) + 
#   coord_cartesian(ylim = c(0.07,0.115)) +
#   labs(x = 'Decile of Score',y = 'Average Frequency',title = 'Quantile Plot for Simple Model')

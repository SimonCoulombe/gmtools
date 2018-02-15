#' plot_ave_data
#'
#' A companion function to create_ave_plot_data. This function calls the aggregator and then makes a nice plot.
#' @param x A vector that contains the dimension that you want to aggregate by
#' @param y_true A vector that contains the target variable (think OBS in emblem)
#' @param y_pred A vector that contains the fitted value from a model (think CA in emblem). Defaults to NULL
#' @param w A vector that contains row level weights (e.g. exposure). Defaults to NULL
#' @param rescale A logical indicating whether you want to the resulting series scaled about the largest level
#' @keywords plot ave
#' @export
#' @examples
#'

plot_ave_data <- function(x,y_true,y_pred=NULL,w=NULL,rescale=FALSE){
    
  plotData = gmtools::create_ave_plot_data(x = x,y_true = y_true,y_pred = y_pred,w = w,rescale = rescale)
  plotData$x = as.factor(plotData$x)
  
  thePlot = ggplot(data = plotData) + 
            geom_bar(aes(x = x,y = scaled_weight),stat = 'identity',alpha = 0.3,fill = 'yellow') +
            geom_line(aes(x = x,y = wmean_true,colour = 'actual',group = 1)) + 
            geom_point(aes(x = x,y = wmean_true,colour = 'actual',group = 1))
          
  if(!is.null(y_pred)) {
    
    thePlot = thePlot + 
              geom_line(aes(x = x,y = wmean_pred,colour = 'fitted',group = 1)) + 
              geom_point(aes(x = x,y = wmean_pred,colour = 'fitted',group = 1))
    
  } 
  
  thePlot = thePlot + scale_color_manual(values = c('blue','green')) + labs(y = 'average actual')
  
  return(thePlot)
}

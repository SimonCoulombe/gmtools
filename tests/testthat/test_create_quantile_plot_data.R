## Set the context
   context("Check the create_quantile_plot_data function works properly")   
   
## Load Package & Data
   require(data.table)
   data(mtcars)
  
## Do some high level testing
   
   test_that(desc = 'create_quantile_plot_data works for weighted data',code = {
     
     mdl = glm(formula = mpg ~ disp + drat,data = mtcars,weight = gear)
     val = data.frame(y_true = mtcars$mpg,y_pred = predict(mdl,newdata = mtcars),weight = mtcars$gear)
     
     ledata = create_quantile_plot_data(y_true = val$y_true,y_pred = val$y_pred,w = val$weight,nbins = 5)
     
     expect_equal(weighted.mean(x = ledata$data$wmean_pred,w = ledata$data$sum_weight),
                  weighted.mean(x = ledata$data$wmean_true,w = ledata$data$sum_weight))
     
     expect_equal(nrow(ledata$data),5)
     
   })
   
   test_that(desc = 'create_quantile_plot_data works for weighted data',code = {
     
     mdl = glm(formula = mpg ~ disp + drat,data = mtcars,weight = gear)
     val = data.frame(y_true = mtcars$mpg,y_pred = predict(mdl,newdata = mtcars))
     
     ledata = create_quantile_plot_data(y_true = val$y_true,y_pred = val$y_pred,nbins = 5)
     
     expect_equal(weighted.mean(x = ledata$data$wmean_pred,w = ledata$data$sum_weight),
                  weighted.mean(x = ledata$data$wmean_true,w = ledata$data$sum_weight))
     
     expect_equal(nrow(ledata$data),5)
     
   })

   
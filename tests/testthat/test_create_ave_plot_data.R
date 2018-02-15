## Set the context
   context("Check the create_ave_plot_data function works")   

## Load Package & Data
   data(dataCar,package = 'insuranceData')

## Start the testing
   mdl_weight = glm(formula = mpg ~ disp + drat + gear,data = mtcars,weights = carb)
   mdl_noweight = glm(formula = mpg ~ disp + drat + gear,data = mtcars)

   test_that(desc = 'create_ave_plot_data works for weighted data',code = {

     leData = create_ave_plot_data(x = mtcars$gear,y_pred = predict(mdl_weight,mtcars),y_true = mtcars$mpg,w = mtcars$carb)

     expect_equal(weighted.mean(x = leData$wmean_pred,w = leData$sum_weight),
                  weighted.mean(x = leData$wmean_true,w = leData$sum_weight))
     expect_equal(nrow(leData),length(unique(mtcars$gear)))
     expect_equal(sum(leData$sum_weight),sum(mtcars$carb))

   })
   test_that(desc = 'create_ave_plot_data works for weighted data',code = {

     leData = create_ave_plot_data(x = mtcars$gear,y_pred = predict(mdl_noweight,mtcars),y_true = mtcars$mpg)

     expect_equal(weighted.mean(x = leData$wmean_pred,w = leData$sum_weight),
                  weighted.mean(x = leData$wmean_true,w = leData$sum_weight))
     expect_equal(nrow(leData),length(unique(mtcars$gear)))
     expect_equal(sum(leData$sum_weight),nrow(mtcars))

   })
   test_that(desc = 'create_ave_plot_data handles bad params',code = {

     expect_error(create_ave_plot_data(x = mtcars$gear,y_pred = rep(NA,nrow(mtcars)),y_true = mtcars$mpg))
     expect_error(create_ave_plot_data(x = mtcars$gear,y_pred = mean(mtcars$mpg),y_true = rep(NA,nrow(mtcars))))

     ## Should not error as NAs are OK in X
     leData = create_ave_plot_data(x = rep(NA,nrow(mtcars)),y_pred = predict(mdl_noweight,mtcars),y_true = mtcars$mpg)
     expect_equal(weighted.mean(x = leData$wmean_pred,w = leData$sum_weight),
                  weighted.mean(x = leData$wmean_true,w = leData$sum_weight))

   })
   test_that(desc = 'create_ave_plot_data handles null pred series',code = {

     leData = create_ave_plot_data(x = mtcars$gear,y_true = mtcars$mpg,w = mtcars$carb)
     expect_equal(('wmean_pred' %in% names(leData)),FALSE)
     expect_equal(nrow(leData),length(unique(mtcars$gear)))
     expect_equal(sum(leData$sum_weight),sum(mtcars$carb))

   })

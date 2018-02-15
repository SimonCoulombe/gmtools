## Set the context
   context("Check the create_double_lift_data functions work properly")   

## Load the test data
   data(dataCar,package = 'insuranceData')

## Do the testing

   ## Because we're using the dodgy n/e w = e fudge to make it a poisson offset R will complain
   ## This is mathematically equivalent to n = offset(log(e)) + x
   glm_one = suppressWarnings(glm(formula = I(numclaims/exposure) ~ veh_age,data = dataCar,family = poisson,weights = dataCar$exposure))
   glm_two = suppressWarnings(glm(formula = I(numclaims/exposure) ~ veh_age + veh_body + veh_value + agecat,
                                  data = dataCar,
                                  family = poisson,
                                  weights = dataCar$exposure))
   
   
   # Tests for weighted data -------------------------------------------------
   ret = create_double_lift_data(modelA = exp(predict(glm_one,dataCar)),
                                 modelB = exp(predict(glm_two,dataCar)),
                                 actual = dataCar$numclaims,
                                 weight = dataCar$exposure,
                                 nbins  = 10)
   
   test_that(desc = 'the function works for weighted data',code = { 
   
     expect_equal(sum(ret$sum_weight),sum(dataCar$exposure))
     
     ## By transivity they A = B
     expect_equal(sum(ret$mean_modelA*ret$sum_weight),sum(ret$mean_actual*ret$sum_weight))
     expect_equal(sum(ret$mean_modelB*ret$sum_weight),sum(ret$mean_actual*ret$sum_weight))
   
   })
   
   # Tests for unweighted data -------------------------------------------------
   ret = create_double_lift_data(modelA = exp(predict(glm_one,dataCar)),
                                 modelB = exp(predict(glm_two,dataCar)),
                                 actual = dataCar$numclaims,
                                 nbins  = 10)
   
   test_that(desc = 'the function works for unweighted data',code = { 
     
     ## Check we've still got it all at the end
     expect_equal(sum(ret$sum_weight),nrow(dataCar))
     
     ## By transivity they A = B
     expect_equal(sum(ret$mean_modelA*ret$sum_weight),sum(ret$mean_actual*ret$sum_weight))
     expect_equal(sum(ret$mean_modelB*ret$sum_weight),sum(ret$mean_actual*ret$sum_weight))
     
   })
   
   
   # test for error handling -------------------------------------------------
   
   test_that(desc = 'function handles errors',code = {
     
     expect_error(object = create_double_lift_data(modelA = exp(predict(glm_one,dataCar)),
                                                   modelB = exp(predict(glm_two,dataCar)),
                                                   actual = dataCar$numclaims,
                                                   weight = rep(NA,nrow(dataCar)),
                                                   nbins  = 10))
     
     expect_error(object = create_double_lift_data(modelA = exp(predict(glm_one,dataCar)),
                                                   modelB = exp(predict(glm_two,dataCar)),
                                                   actual = dataCar$numclaims,
                                                   weight = dataCar$exposure,
                                                   nbins  = 1.2))
     
     expect_error(object = create_double_lift_data(modelA = exp(predict(glm_one,dataCar)),
                                                   modelB = exp(predict(glm_two,dataCar)),
                                                   actual = dataCar$numclaims,
                                                   weight = dataCar$exposure,
                                                   nbins  = -1))
     
   })

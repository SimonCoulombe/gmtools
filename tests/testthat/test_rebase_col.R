## Set the context
   context("Check the rebase_col function works properly") 

## Load Package & Data
   require(testthat);data(mtcars)

## Begin the tests
   test_that(desc = 'The function can rebase a numeric col',code = {
     
     expect_equal(object = mean(rebase_col(x = mtcars$drat,base = mtcars$disp)),expected = 230.7219,tolerance = 0.0001)
     expect_equal(object = weighted.mean(x = rebase_col(x = mtcars$drat,base = mtcars$disp,w = mtcars$gear),w = mtcars$gear),
                  expected = 217.3754,
                  tolerance = 0.0001)
   })
   
   test_that(desc = 'The function will handle errors',code = {
     
     expect_error(object = rebase_col(x = mtcars$mpg,base = rep(NA,nrow(mtcars))))
     expect_error(object = rebase_col(x = mtcars$mpg,base = rep(NA,1)))
     
   })
   




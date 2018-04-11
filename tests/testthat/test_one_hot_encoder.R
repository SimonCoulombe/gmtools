
## Set the test context
   context("Check the one_hot_encoder function works properly")

## Load data for tests
   data(dataCar,package = 'insuranceData')

## run the tests
   require(caret)
   mtcars$carb = as.factor(mtcars$carb)
   mtcars$gear = as.factor(mtcars$gear)
   aa = one_hot_encoder(df = mtcars,features = c('carb','gear'))  
   
## Create tests
   test_that(desc = "Run some tests",
   			 code = { expect_equal(ncol(aa),ncol(mtcars)+length(unique(mtcars$carb))+length(unique(mtcars$gear)))
					  expect_equal(nrow(aa),nrow(mtcars)) })   
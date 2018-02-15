## Set the context
   context("Check the train_xgboost function works properly")      

## Load data
   load(file = "C:\\Users\\George\\Documents\\Development\\gmtools\\tests\\testthat\\dataCar.rda")

## Start testing
   features = c('veh_value','agecat','veh_age')
   
   test_that(desc = 'test the train function works for cv models',code = {
   
     devNull = capture.output({tmp = train_xgboost(dtrain = dataCar,
                               x = features,
                               y = 'clm',
                               xgbParams = list(objective = 'binary:logistic',nthread = 10),
                               nrounds = 20,
                               nfold = 5)})
     
     ## Tests all around the CV parameters
     expect_equal(tmp$dvalid,NULL)
     expect_equal(tmp$is_cv,TRUE)
     expect_equal(class(tmp$xgbCV),"xgb.cv.synchronous")
     expect_equal(length(tmp$xgbCV$folds),5)
     
     ## Check if the numbner of optimal rounds was observed
     expect_equal(nrow(tmp$finalModel$evaluation_log),tmp$opt_nrounds)
     expect_equal(tmp$fin_early_stopping_rounds,NULL)
     
     ## Check if the weights are OK - if we get problems we can add another test for weighted stuff
     expect_equal(max(getinfo(tmp$dtrain,'weight')),1)
     
   })
   test_that(desc = 'test the train function works for non-cv models',code = {
     
     devNull = capture.output({tmp = train_xgboost(dtrain = dataCar,
                               dvalid = dataCar,
                               x = features,
                               y = 'clm',
                               xgbParams = list(objective = 'binary:logistic',nthread = 10),
                               nrounds = 20,
                               nfold = 5)})
     
     ## Tests all around the CV parameters
     expect_equal(nrow(tmp$dvalid),nrow(dataCar))
     expect_equal(tmp$is_cv,FALSE)
     expect_equal(class(tmp$xgbCV),"NULL")
     
     ## Check if the numbner of optimal rounds was observed
     expect_equal(tmp$fin_early_stopping_rounds,5)
     
     ## Check if the weights are OK - if we get problems we can add another test for weighted stuff
     expect_equal(max(getinfo(tmp$dtrain,'weight')),1)
     
   })
## Set the context
   context("Check the train_xgboost function works properly")      

## Load data
   data(dataCar,package = 'insuranceData')
   data(polishBank,package = "gmtools")

## test the xgb_create_dmatrix
   test_that(desc = "The xgb_create_dmatrix works",code = {
  
               ## Create reference
               all_opt = xgb_create_dmatrix(data = polishBank,
                                            x = names(polishBank)[1:5],
                                            y = names(polishBank)[6],
                                            w = names(polishBank)[7],
                                            base_margin = names(polishBank)[8])  
               
               ## Run generic tests
               expect_equal(unlist(dimnames(all_opt)[2]),names(polishBank)[1:5])
               expect_equal(object = getinfo(all_opt,"label"),polishBank[,6],tolerance = 1e-7)
               expect_equal(object = getinfo(all_opt,"weight"),expected = polishBank[,7],tolerance = 1e-7)
               expect_equal(object = getinfo(all_opt,"base_margin"),expected = polishBank[,8],tolerance = 1e-7)
               
               ## checking that the null args work
               all_opt = xgb_create_dmatrix(data = polishBank,
                                            x = names(polishBank)[1:5],
                                            y = names(polishBank)[6])  
               
               
               ## Run some more tests
               expect_equal(unlist(dimnames(all_opt)[2]),names(polishBank)[1:5])
               expect_equal(getinfo(all_opt,"label"),polishBank[,6],tolerance = 1e-7)
               expect_equal(getinfo(all_opt,"base_margin"),NULL)
               expect_equal(getinfo(all_opt,"weight"),rep(1,nrow(polishBank)))
               
              })  


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
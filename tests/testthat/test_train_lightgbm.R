## Set the context
   context("Check the train_lightgbm function works properly")      

## Load data
   example_data = prepare_example_data()

## Create some lgb matricies to work with   
   dtrain = lgb.Dataset(data   = as.matrix(example_data$data[,example_data$features,with=FALSE]),
                        label  = example_data$data$freq,
                        weight = example_data$data$exposure)

   dvalid = lgb.Dataset(data   = as.matrix(example_data$data[,example_data$features,with=FALSE]),
                        label  = example_data$data$freq,
                        weight = example_data$data$exposure)

## Start the testing   

    test_that(desc = 'test the train function works for cv models',code = {
   
     devNull = capture.output({tmp = train_lightgbm(dtrain = example_data$data,
                               						x = example_data$features,
                               						y = 'clm',
                               						LgbParams = list(objective = "binary"),
                               						nrounds = 100,
                               						verbose = 0,
                               						nfold = 5)})
     
     ## Tests all around the CV parameters
     expect_equal(tmp$dvalid,NULL)
     expect_equal(tmp$is_cv,TRUE)
     expect_equal(class(lgb_mdl$LgbCV)[1],"lgb.CVBooster")
     expect_equal(length(tmp$LgbCV$boosters),5)
     
     ## Check if the numbner of optimal rounds was observed
     expect_equal(length(tmp$finalModel$record_evals$train$binary_logloss$eval),tmp$opt_nrounds)
     expect_equal(tmp$fin_early_stopping_rounds,NULL)
     
     ## Check if the weights are OK - if we get problems we can add another test for weighted stuff
     expect_equal(max(getinfo(tmp$dtrain,'weight')),1)
     
   })


   test_that(desc = 'test the train function works for non-cv models',code = {
     
     devNull = capture.output({tmp = train_lightgbm(dtrain = example_data$data,
                                                    dvalid = example_data$data,
                                                    x = example_data$features,
                                                    y = 'clm',
                                                    LgbParams = list(objective = 'binary'),
                                                    nrounds = 20,
                                                    verbose = 0,
                                                    nfold = 5)})
     
     ## Tests all around the CV parameters
     expect_equal(nrow(tmp$dvalid),nrow(dataCar))
     expect_equal(tmp$is_cv,FALSE)
     expect_equal(class(tmp$LgbCV),"NULL")
     
     ## Check if the numbner of optimal rounds was observed
     expect_equal(tmp$fin_early_stopping_rounds,5)
     
     ## Check if the weights are OK - if we get problems we can add another test for weighted stuff
     expect_equal(max(getinfo(tmp$dtrain,'weight')),1)
     
   })
## Set the context
   context("Check that the fit_h2o_mdl and associated helpers work")

## Load the test data
   data(dataCar,package = "insuranceData")
   dataCar$freq = dataCar$numclaims / dataCar$exposure

## Stop Progress Messages
   h2o.no_progress()

## Begin the testing proper
   glm = fit_h2o_mdl(algo           = 'h2o.glm',
                     x              = c('veh_value','veh_age','agecat'),
                     y              = 'freq',
                     weights_column = 'exposure',
                     training_frame = dataCar,
                     keep_cross_validation_predictions = TRUE,
                     nfold          = 5,
                     family         = 'poisson')

   test_that(desc = 'The basic capapbility works properly',code={
    expect_equal(glm@allparameters$training_frame,'int_train_data')
    expect_equal(glm@allparameters$validation_frame,NULL)
    expect_equal(glm@allparameters$x,c('veh_value','veh_age','agecat','exposure'))
    expect_equal(glm@allparameters$nfolds,5)
    expect_equal(glm@algorithm,'glm')
   })

   gbm = fit_h2o_mdl(algo           = 'h2o.gbm',
                     x              = c('veh_value','veh_age','agecat'),
                     y              = 'freq',
                     weights_column = 'exposure',
                     training_frame = dataCar,
                     keep_cross_validation_predictions = FALSE,
                     nfold          = 5,
                     distribution   = 'poisson')

   test_that(desc = 'The algo argument actually works',code={
    expect_equal(gbm@allparameters$training_frame,'int_train_data')
    expect_equal(gbm@allparameters$validation_frame,NULL)
    expect_equal(gbm@allparameters$x,c('veh_value','veh_age','agecat','exposure'))
    expect_equal(gbm@allparameters$nfolds,5)
    expect_equal(gbm@algorithm,'gbm')
   })


   test_that(desc = "The get_h2o_predictions function works as expected",code={
   	glm_preds = get_h2o_predictions(mdl = glm,xval = TRUE)
    expect_equal(length(glm_preds),nrow(dataCar))
    expect_equal(anyNA(glm_preds),FALSE)
    expect_equal(class(glm_preds),'numeric')

    expect_error(object = {gbm_preds = get_h2o_predictions(mdl = gbm,xval = TRUE)},regexp = 'ERROR: Requested OOF')
    gbm_preds = get_h2o_predictions(mdl = gbm,newdata = h2o.getFrame("int_train_data"))
    expect_equal(length(gbm_preds),nrow(dataCar))
    expect_equal(anyNA(gbm_preds),FALSE)
    expect_equal(class(gbm_preds),'numeric')
   })

## TODO: Write some proper unit tests for the load_h2o_data function ;)


## Set the test context
   context("Check the boruta family of functions work properly")   

## Load some test data    
   pkgs = c('insuranceData'); sapply(pkgs,require,character.only = TRUE)
   data(dataCar) 

## Run the tests    
   test_that(desc = 'the .create_shadow function works',code = {
     expect_equal(object = sum(dataCar$veh_value),expected = sum(.create_shadow(x = dataCar$veh_value)))
     expect_false(isTRUE(all.equal(dataCar$veh_value,.create_shadow(x = dataCar$veh_value))))
   })

   test_that(desc = 'the create_shadow_features function works',code = {
     ret = create_shadow_features(df = dataCar,features = c('veh_value','gender'))
     expect_equal(ncol(ret),ncol(dataCar)+2)
     expect_equal(sum(grepl(pattern = 'shadow_',x = setdiff(names(ret),names(dataCar)))),2)
     expect_equal(nrow(ret),nrow(dataCar))
     
     ret = create_shadow_features(df = data.table(dataCar),features = c('veh_value','gender'))
     expect_equal(ncol(ret),ncol(dataCar)+2)
     expect_equal(sum(grepl(pattern = 'shadow_',x = setdiff(names(ret),names(dataCar)))),2)
     expect_equal(nrow(ret),nrow(dataCar))
   })

   test_that(desc = 'the get_feat_names function works',code = {
     features = c('a','b')
     expected = c('a','b','shadow_a','shadow_b')
     expect_equal(object = get_feat_names(features = features),expected)
   })

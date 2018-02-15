## Set the context
   context("Check the target_encoder function works properly")   
   
## Load packages and data   
   load(file = "C:\\Users\\George\\Documents\\Development\\gmtools\\tests\\testthat\\dataCar.rda")

## tests for target encode
   tar = .target_encode(trn_x = dataCar$gender,tst_x = dataCar$gender,trn_y = dataCar$numclaims,trn_w = dataCar$exposure,min_samples_leaf = 0,smoothing = 1,noise_level = 0)
   ref = unique(tar)
   exp = as.numeric(unlist(unique(data.table(dataCar)[, weighted.mean(x = numclaims,w = exposure) , by = gender][,2])))
   
   expect_equal(ref,exp)
   expect_equal(length(tar),nrow(dataCar))
   
   ## tests for .add_noise
   expect_equal(.add_noise(series = seq(1,100),noise_level = 0),seq(1,100))
   expect_equal(length(.add_noise(series = seq(1,100),noise_level = 0)),100)
   
   ## tests for .create_smoothing
   expect_equal(length(.create_smoothing(weights = c(1,2,3),min_samples_leaf = 2,smoothing = 1)),3)
   
   ## Test for main function
   test_frame = dataCar[,c('gender','area','veh_body','numclaims','exposure')]
   test_frame = cbind(test_frame,
                      target_encoder(train_df = dataCar,
                                     test_df = dataCar,
                                     y = 'numclaims',
                                     x = c('veh_body','area','gender'),
                                     w = "exposure",
                                     min_samples_leaf = 0,
                                     smoothing = 1,
                                     noise_level = 0))
   
   testSummary = data.table(test_frame)[ , list(sack = weighted.mean(x = numclaims,w = exposure)) , by = gender]
   expect_equal(sort(unique(testSummary$sack)),sort(unique(test_frame$enc_gender)))
   expect_equal(nrow(dataCar),nrow(test_frame))
   expect_equal(ncol(test_frame),8)
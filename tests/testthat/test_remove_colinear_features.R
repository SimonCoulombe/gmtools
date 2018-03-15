## Set the test context
   context("Check that the remove_colinear_features function works")

## Load some test data
   data(polishBank,package = "gmtools")

## Set up the feature list
   features = names(polishBank)[1:64]

## Run the tests
   test_that(desc = 'check that the remove_colinear_features function works',
   			 code = { cor_info = remove_colinear_features(df = polishBank,features = features,use = "complete.obs")
   			 		  expect_equal(cor_info$correlation_matrix,cor(polishBank[,features],use = "complete.obs"))
   			 		  expect_equal(length(cor_info$included_features),34) })

## Set the test context
   context("Check that the remove_colinear_features function works")

## Load some test data
   load("../Downloads/polishBank.rda")

## Set up the feature list
   features = names(polishBank)[1:64]

## Run the tests
   test_that(desc = 'check that the remove_colinear_features function works',
   			 code = { cor_info = remove_colinear_features(df = polishBank,features = features,use = "complete.obs")
   			 		  all.equal(cor_info$correlation_matrix,cor(polishBank[,features],use = "complete.obs"))
   			 		  all.equal(cor_info$included_features,34) })

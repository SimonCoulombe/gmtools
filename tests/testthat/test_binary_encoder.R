## Set the test context
   context("Check the binary encoder functions properly")

## Load data for tests
   data(dataCar,package = 'insuranceData')

## Run the tests
   test_that(desc = 'the binary_encode help works properly',
             code = {ret = .binary_encode(x = dataCar$veh_body)
             expect_equal(ncol(ret),ceiling(log(length(unique(dataCar$veh_body)) + 1)/log(2)))
             expect_equal(nrow(dataCar),nrow(ret)) })

   test_that(desc = 'the binary encoder wrapper works properly',
             code = { bitsReq <- function(x){ceiling(log(length(unique(x)) + 1)/log(2))}
             exp_cols = bitsReq(x = dataCar$veh_body) + bitsReq(x = dataCar$gender) + bitsReq(x = dataCar$area)
             
             ccc = binary_encoder(df = dataCar,features = c('gender','veh_body','area'))
             expect_equal(ncol(ccc),exp_cols)
             expect_equal(nrow(ccc),nrow(dataCar))  })


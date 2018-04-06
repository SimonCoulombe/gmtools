## Set the test context
   context("Check the describe function works properly")

## Load data for tests
   data(dataCar,package = 'insuranceData')

## Run the tests
   test_that(desc = "The generic function capability works",code = {
 
      ## generate the description
      descriptor = gmtools::describe(df = dataCar)
      expect_equal(nrow(descriptor),ncol(dataCar))
      expect_equal(names(descriptor),c("feature","class","nlevels","numNA"))
      expect_equal(unlist(descriptor[descriptor$feature == "veh_value", "nlevels"]),length(unique(dataCar$veh_value)))
      expect_equal(sum(sapply(dataCar,is.numeric)),length(descriptor[ descriptor$class %in% c("numeric","integer"), 1]))
      expect_equal(sum(sapply(dataCar,is.na)),sum(descriptor$numNA))

    })


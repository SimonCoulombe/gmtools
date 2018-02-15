
## Set the context
   context("Check the binnarise function works properly")   

## Load Package & Data
   ## require(data.table)
   data(dataCar,package = 'insuranceData')
 
## Run tests
   test_that(desc = 'binnarise function works for weighted data',code = {
     
     ## Vanilla
     temp = data.table::data.table(weight = dataCar$exposure,bin = binnarise(x = dataCar$veh_value,w = dataCar$exposure,nbins = 5))
     aggr = prop.table(aggregate(weight ~ bin,data = temp,FUN = sum)[,2])
     test = sum(apply(as.matrix(aggr),1,function(i){ data.table::between(round(i,digits = 2),lower = 0.19,upper = 0.21)}))
     expect_equal(object = test,expected = 5)
     
     ## More Bins
     temp = data.table::data.table(weight = dataCar$exposure,bin = binnarise(x = dataCar$veh_value,w = dataCar$exposure,nbins = 10))
     aggr = prop.table(aggregate(weight ~ bin,data = temp,FUN = sum)[,2])
     test = sum(apply(as.matrix(aggr),1,function(i){ data.table::between(round(i,digits = 2),lower = 0.09,upper = 0.11)}))
     expect_equal(object = test,expected = 10)
     
   })
   
   test_that(desc = 'binnarise function works for unweighted data',code = {
     
     ## Vanilla
     temp = data.table::data.table(weight = rep(1,nrow(dataCar)),bin = binnarise(x = dataCar$veh_value,nbins = 5))
     aggr = prop.table(aggregate(weight ~ bin,data = temp,FUN = sum)[,2])
     test = sum(apply(as.matrix(aggr),1,function(i){ data.table::between(round(i,digits = 2),lower = 0.19,upper = 0.21)}))
     expect_equal(object = test,expected = 5)
     
     ## More bins
     temp = data.table::data.table(weight = rep(1,nrow(dataCar)),bin = binnarise(x = dataCar$veh_value,nbins = 10))
     aggr = prop.table(aggregate(weight ~ bin,data = temp,FUN = sum)[,2])
     test = sum(apply(as.matrix(aggr),1,function(i){ data.table::between(round(i,digits = 2),lower = 0.09,upper = 0.11)}))
     expect_equal(object = test,expected = 10)
     
     
   })
   
   test_that(desc = 'binnarise function handles bad parameters',code = {
     
     expect_error(object = binnarise(x = dataCar$veh_value,w = rep(NA,nrow(dataCar))))
     expect_error(object = binnarise(x = dataCar$veh_value,w = dataCar$exposure,nbins = 1))
     
   })
   
   
   
   
   
   
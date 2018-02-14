## Set the context
   context("Check the label_encoder works properly")   

## Load Package & Data
   require(insuranceData); require(data.table)
   data(dataCar)
     
## Run the tests
   
   test_that(desc = 'the label encoder function works in general + conversions from data.frame',code = {
     
     lbl_encoded = label_encoder(df = dataCar,features = "veh_body")
     expect_equal(object = class(lbl_encoded$veh_body),'numeric')
     expect_equal(object = length(unique(dataCar$veh_body)),length(unique(lbl_encoded$veh_body)))
     
   })
   
   test_that(desc = 'the label encoder can handle data.tables',code = {
     
     lbl_encoded = label_encoder(df = as.data.table(dataCar),features = "veh_body")
     expect_equal(object = class(lbl_encoded$veh_body),'numeric')
     expect_equal(object = length(unique(dataCar$veh_body)),length(unique(lbl_encoded$veh_body)))
     
   })
   
   
   
   
## Set the context
   context("Check the create_interactions function works properly")  

## Load Some packages & data
   load(file = "C:\\Users\\George\\Documents\\Development\\gmtools\\tests\\testthat\\dataCar.rda")

## Start the testing
   test_that(desc = 'the .prepare_interaction function works',code = {
     
     tmp = .prepare_interaction(df = dataCar,intTerms = c('gender','agecat'),nlvl = 5)
     expect_equal(ncol(tmp),2)
     expect_equal(nrow(tmp),nrow(dataCar))
     expect_true(isTRUE(max(sapply(tmp,function(c){ length(unique(c))}))<11))
     
     dataCarChar = dataCar; dataCarChar$area = as.character(dataCarChar$area)
     expect_error(object = .prepare_interaction(df = dataCarChar,intTerms = c('area','agecat'),nlvl = 5),
                  regexp = "ERROR: Character feature cannot be binned")
   })
   test_that(desc = 'the make_interaction function works',code = {
     
     tmp = make_interaction(df = dataCar,recipe = 'gender|agecat',nlvl=25)
     expect_equal(tmp$nways,2)
     expect_equal(tmp$intName,'gender_X_agecat')
     
     expect = as.numeric(factor(paste(dataCar$gender,dataCar$agecat,sep='_'),exclude = NULL))
     expect_equal(expect,as.numeric(unlist(tmp$intVal)))
   })
   test_that(desc = 'the create_interactions function works',code = {
     
     tmp = create_interactions(df = dataCar,intList = c('gender|agecat','gender|area'),nlvl = 25)
     
     expect = as.numeric(factor(paste(dataCar$gender,dataCar$agecat,sep='_'),exclude = NULL))
     expect_equal(expect,tmp$gender_X_agecat)
     
     expect = as.numeric(factor(paste(dataCar$gender,dataCar$area,sep='_'),exclude = NULL))
     expect_equal(expect,tmp$gender_X_area)
     
     expect_equal(names(tmp),c('gender_X_agecat','gender_X_area'))
   })

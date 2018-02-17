## Set the context
   context('Check that the preparation of example data works')

## Run the tests
   test_that(desc = 'test that the prepare_example_data function works',code = {
    ret = prepare_example_data()
    expect_equal(length(ret$features),6)
    expect_equal(nrow(ret$data),67856)
    expect_equal(sum(sapply(ret$data,is.numeric)),11)
    expect_equal(sum(ret$data$veh_value),120581.5,tolerance = 0.1)
    expect_equal(length(unique(ret$data$gender)),2)   
   })
   
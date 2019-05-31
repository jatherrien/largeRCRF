context("Verify we can have responses of length 1 without a crash")

test_that("CR_Response of length 1 - no censor times", {
  
  CR_Response(1,1)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("CR_Response of length 1 - no censor times", {
  
  CR_Response(1,1,1)
  
  expect_true(T) # show Ok if we got this far
  
})
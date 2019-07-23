context("Verify we can have responses of length 1 without a crash")

test_that("CR_Response of length 1 - no censor times", {
  
  CR_Response(1,1)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("CR_Response of length 1 - no censor times", {
  
  CR_Response(1,1,1)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Can sub-index CR_Response - no censor times", {
  
  x <- CR_Response(1:5, 1:5)
  
  index <- 5
  
  y <- x[index]
  
  expect_equal(y$eventTime, index)
  expect_equal(y$eventIndicator, index)
  
  expect_equal(rJava::.jcall(y$javaObject, "I", "size"), 1)
  oneJavaItem <- rJava::.jcall(y$javaObject, largeRCRF:::makeResponse(largeRCRF:::.class_Object), "get", 0L)
  oneJavaItem <- rJava::.jcast(oneJavaItem, largeRCRF:::.class_CompetingRiskResponse)
  delta <- rJava::.jcall(oneJavaItem, "I", "getDelta")
  
  expect_equal(delta, index)
  
})

test_that("Can sub-index CR_Response - censor times", {
  
  x <- CR_Response(1:5, 1:5, 1:5)
  
  index <- 5
  
  y <- x[index]
  
  expect_equal(y$eventTime, index)
  expect_equal(y$eventIndicator, index)
  expect_equal(y$censorTime, index)
  
  expect_equal(rJava::.jcall(y$javaObject, "I", "size"), 1)
  oneJavaItem <- rJava::.jcall(y$javaObject, largeRCRF:::makeResponse(largeRCRF:::.class_Object), "get", 0L)
  oneJavaItem <- rJava::.jcast(oneJavaItem, largeRCRF:::.class_CompetingRiskResponseWithCensorTime)
  delta <- rJava::.jcall(oneJavaItem, "D", "getC")
  
  expect_equal(delta, index)
  
})
context("Train Naive Concordance")

test_that("Naive Concordance Works and gives accurate results", {

  responses = CR_Response(c(1,0,2,1), c(5,6,8,3))
  
  mortalities = list(
    c(1,4,3,9)
  )
  score = naiveConcordance(responses, mortalities)
  
  expect_equal(score, 1-3/5)
  
})

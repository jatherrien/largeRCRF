context("Calculate integrated Brier score without error")

# This code is more concerned that the code runs without error. The tests in the
# Java code check that the results it returns are accurate.
test_that("Can calculate Integrated Brier Score", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$T <- sample(0:4, size=100, replace = TRUE) # the censor distribution we provide needs to conform to the data or we can get NaNs
  sampleData$delta <- sample(0:2, size = 100, replace = TRUE)
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest, testData)
  
  scores_test <- integratedBrierScore(CR_Response(testData$delta, testData$T), predictions, event = 1, time = 4,
                                 censoringDistribution = NULL)
  # Check that we don't get a crash if we calculate the error for only one observation
  scores_one <- integratedBrierScore(CR_Response(testData$delta, testData$T)[1], predictions[1], event = 1, time = 4,
                                 censoringDistribution = NULL)
  
  # Make sure our error didn't somehow change
  expect_equal(scores_one, scores_test[1])
  
  # Provide a censoring distribution via censor times
  scores_censoring1 <- integratedBrierScore(CR_Response(testData$delta, testData$T), predictions, event = 1, time = 4,
                                      censoringDistribution = c(0,1,1,2,3,4))
  scores_censoring2 <- integratedBrierScore(CR_Response(testData$delta, testData$T), predictions, event = 1, time = 4,
                                     censoringDistribution = list(x = 0:4, y = 1 - c(1/6, 3/6, 4/6, 5/6, 6/6)))
  scores_censoring3 <- integratedBrierScore(CR_Response(testData$delta, testData$T), predictions, event = 1, time = 4,
                                            censoringDistribution = stepfun(x=0:4, y=1 - c(0, 1/6, 3/6, 4/6, 5/6, 6/6)))
  
  expect_equal(scores_censoring1, scores_censoring2)
  expect_equal(scores_censoring1, scores_censoring3)
  
})

context("Train and predict with NAs") # previously model.frame removed NAs, causing the responses and covariates to not match up

test_that("Competing Risks doesn't crash", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$Time <- rexp(100) + abs(sampleData$x)
  sampleData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  sampleData$x[50:55] <- NA
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(CR_Response(delta, Time) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest, testData)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Regresssion doesn't crash", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$y <- rexp(100) + sampleData$x
  
  sampleData$x[50:55] <- NA
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(y ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest, testData)
  
  expect_true(T) # show Ok if we got this far
  
})
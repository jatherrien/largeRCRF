context("Predict without re-specifying training data")

test_that("Can predict without new data", {

  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Can connect new data", {
  
  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  forest$dataset <- NULL
  forest <- connectToData(forest, CR_Response(trainingData$delta, trainingData$T), trainingData)
  
  predictions <- predict(forest)
  
  expect_true(T) # show Ok if we got this far
  
})
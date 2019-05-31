context("Train and predict without error using an environment")

test_that("Training with environment works", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$T <- rexp(100) + abs(sampleData$x)
  sampleData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  e <- new.env()
  e$data <- trainingData
  rm(trainingData)
  
  forest <- train(CR_Response(delta, T) ~ x, e, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2)
  
  expect_null(e$data)
  
  predictions <- predict(forest, testData)
  
  expect_true(T) # show Ok if we got this far
  
})

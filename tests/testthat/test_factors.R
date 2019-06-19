context("Train and predict with factors")

test_that("Competing Risks doesn't crash", {
  set.seed(5)

  sampleData <- data.frame(x=rnorm(100), z=sample(letters[1:3], replace=TRUE, size=100))
  sampleData$Time <- rexp(100) + abs(sampleData$x)
  sampleData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(CR_Response(delta, Time) ~ x + z, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest, testData)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Regresssion doesn't crash", {
  set.seed(6)
  
  sampleData <- data.frame(x=rnorm(100), z=sample(letters[1:3], replace=TRUE, size=100))
  sampleData$y <- rexp(100) + sampleData$x
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(y ~ x + z, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest, testData)
  
  expect_true(T) # show Ok if we got this far
  
})
context("Use VIMP without error")

test_that("VIMP doesn't crash; no test dataset", {

  data(wihs)
  
  forest <- train(CR_Response(status, time) ~ ., wihs, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, displayProgress=FALSE)
  
  # Run VIMP several times under different scenarios
  importance <- vimp(forest, type="raw", events=1:2, time=5.0)
  vimp(forest, type="raw", events=1, time=5.0)
  vimp(forest, type="raw", events=1:2, time=5.0, eventWeights = c(0.2, 0.8))
  
  # Not much of a test, but the Java code tests more for correctness. This just
  # tests that the R code runs without error.
  expect_equal(ncol(importance), 4) # 4 predictors
  
})


test_that("VIMP doesn't crash; test dataset", {
  
  data(wihs)
  
  trainingData <- wihs[1:1000,]
  testData <- wihs[1001:nrow(wihs),]
  
  forest <- train(CR_Response(status, time) ~ ., trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, displayProgress=FALSE, cores=1)
  
  # Run VIMP several times under different scenarios
  importance <- vimp(forest, newData=testData, type="raw", events=1:2, time=5.0)
  vimp(forest, newData=testData, type="raw", events=1, time=5.0)
  vimp(forest, newData=testData, type="raw", events=1:2, time=5.0, eventWeights = c(0.2, 0.8))
  
  # Not much of a test, but the Java code tests more for correctness. This just
  # tests that the R code runs without error.
  expect_equal(ncol(importance), 4) # 4 predictors
  
})


test_that("VIMP doesn't crash; censoring distribution; all methods equal", {
  
  sampleData <- data.frame(x=rnorm(100))
  sampleData$T <- sample(0:4, size=100, replace = TRUE) # the censor distribution we provide needs to conform to the data or we can get NaNs
  sampleData$delta <- sample(0:2, size = 100, replace = TRUE)
  
  testData <- sampleData[1:5,]
  trainingData <- sampleData[6:100,]
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  importance1 <- vimp(forest, type="raw", events=1:2, time=4.0, randomSeed=50, 
                      censoringDistribution = c(0,1,1,2,3,4))
  importance2 <- vimp(forest, type="raw", events=1:2, time=4.0, randomSeed=50, 
                      censoringDistribution = list(x = 0:4, y = 1 - c(1/6, 3/6, 4/6, 5/6, 6/6)))
  importance3 <- vimp(forest, type="raw", events=1:2, time=4.0, randomSeed=50, 
                      censoringDistribution = stepfun(x=0:4, y=1 - c(0, 1/6, 3/6, 4/6, 5/6, 6/6)))
  
  expect_equal(importance1, importance2)
  expect_equal(importance1, importance3)
  
})

test_that("VIMP doesn't crash; regression dataset", {
  
  data <- data.frame(x1=rnorm(1000), x2=rnorm(1000), x3=rnorm(1000))
  data$y <- data$x1 + 3*data$x2 + 0.05*data$x3 + rnorm(1000)
  
  forest <- train(y ~ ., data, ntree=50, numberOfSplits=100, mtry=2, nodeSize=5, displayProgress=FALSE)
  
  importance <- vimp(forest, type="mean")
  
  expect_true(importance["x2"] > importance["x3"])
  
  # Not much of a test, but the Java code tests more for correctness. This just
  # tests that the R code runs without error.
  expect_equal(length(importance), 3) # 3 predictors
  
})

test_that("VIMP produces mean and z scores correctly", {
  
  data <- data.frame(x1=rnorm(1000), x2=rnorm(1000), x3=rnorm(1000))
  data$y <- data$x1 + 3*data$x2 + 0.05*data$x3 + rnorm(1000)
  
  forest <- train(y ~ ., data, ntree=50, numberOfSplits=100, mtry=2, nodeSize=5, displayProgress=FALSE)
  
  actual.importance.raw <- vimp(forest, type="raw", randomSeed=5)
  actual.importance.mean <- vimp(forest, type="mean", randomSeed=5)
  actual.importance.z <- vimp(forest, type="z", randomSeed=5)
  
  expected.importance.mean <- apply(actual.importance.raw, 2, mean)
  expected.importance.z <- apply(actual.importance.raw, 2, function(x){
    mn <- mean(x)
    return( mn / (sd(x) / sqrt(length(x))) )
  })
  
  expect_equal(expected.importance.mean, actual.importance.mean)
  expect_equal(expected.importance.z, actual.importance.z)
  
})
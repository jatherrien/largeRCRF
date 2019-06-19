context("Verify that when we use `.` in our formula that we don't include our response as covariates!")

test_that("CompetingRisks - response not included", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$Time <- rexp(100) + abs(sampleData$x)
  sampleData$delta <- sample(0:2, size = 100, replace=TRUE)

  forest <- train(CR_Response(delta, Time) ~ ., sampleData, 
                  ntree=50, 
                  numberOfSplits=0, 
                  mtry=1, 
                  nodeSize=5, 
                  cores=1, 
                  displayProgress=FALSE)
  
  expect_equal(rJava::.jcall(forest$covariateList, "I", "size"), 1)
  
})

test_that("CompetingRisksWithCensorTimes - response not included", {
  
  sampleData <- data.frame(x=rnorm(100))
  sampleData$Time <- rexp(100) + abs(sampleData$x)
  sampleData$CensorTime <- rexp(100) + abs(sampleData$x)
  sampleData$delta <- sample(0:2, size = 100, replace=TRUE)

  forest <- train(CR_Response(delta, Time, CensorTime) ~ ., sampleData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=1, displayProgress=FALSE)
  
  expect_equal(rJava::.jcall(forest$covariateList, "I", "size"), 1)
  
})

test_that("Regression - response not included", {

  sampleData <- data.frame(x=rnorm(100))
  sampleData$y <- rexp(100) + sampleData$x
  
  forest <- train(y ~ ., sampleData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=1, displayProgress=FALSE)
  
  expect_equal(rJava::.jcall(forest$covariateList, "I", "size"), 1)
  
})

test_that("Regression - response not included (with Numeric)", {
  
  sampleData <- data.frame(x=rnorm(100))
  sampleData$y <- rexp(100) + sampleData$x
  
  forest <- train(Numeric(y) ~ ., sampleData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=1, displayProgress=FALSE)
  
  expect_equal(rJava::.jcall(forest$covariateList, "I", "size"), 1)
  
})
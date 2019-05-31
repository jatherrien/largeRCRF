context("Test deterministic forests")

test_that("Two forests produce identical results", {

  x1 <- rnorm(100)
  x2 <- rnorm(100)
  y <- 1 + x1 + x2 + rnorm(100)
  
  data <- data.frame(x1, x2, y)
  forest1 <- train(y ~ x1 + x2, data,
                  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                  randomSeed=5)
  
  forest2 <- train(y ~ x1 + x2, data,
                   ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                   randomSeed=5)

  newData <- data.frame(x1=rnorm(10), x2=rnorm(10))
  predictions1 <- predict(forest1, newData)
  predictions2 <- predict(forest2, newData)
  
  expect_equal(round(predictions1, digits=6), round(predictions2, digits=6))
  
  
})

test_that("Finishing an interrupted forest produces the same results as having finished it", {
  
  expect_false(file.exists("trees_deterministic_forests")) # Folder shouldn't exist yet
  
  x1 <- rnorm(1000)
  x2 <- rnorm(1000)
  y <- 1 + x1 + x2 + rnorm(1000)
  
  data <- data.frame(x1, x2, y)
  forest1 <- train(y ~ x1 + x2, data,
                  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                  randomSeed=6)
  
  forest2.incomplete <- train(y ~ x1 + x2, data,
                   ntree=50, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                   randomSeed=6, savePath="trees_deterministic_forests")
  forest2.complete <- train(y ~ x1 + x2, data,
                   ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                   randomSeed=6, savePath="trees_deterministic_forests", 
                   savePath.overwrite="merge")
  
  
  newData <- data.frame(x1=rnorm(10), x2=rnorm(10))
  predictions1 <- predict(forest1, newData)
  predictions2 <- predict(forest2.complete, newData)
  
  expect_equal(round(predictions1, digits=6), round(predictions2, digits=6))
  
  
  unlink("trees_deterministic_forests", recursive=TRUE)
  
})

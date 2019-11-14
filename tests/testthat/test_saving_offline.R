context("Make sure we can save forests while training")

test_that("Can save a random forest while training, and use it afterward", {

  if(file.exists("trees")){ # folder could exist from a previous failed test; delete it
    unlink("trees", recursive=TRUE)
  }
  
  x1 <- rnorm(1000)
  x2 <- rnorm(1000)
  y <- 1 + x1 + x2 + rnorm(1000)
  
  data <- data.frame(x1, x2, y)
  forest <- train(y ~ x1 + x2, data,
                  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                  savePath="trees", forest.output = "online", displayProgress=FALSE)
  
  expect_true(file.exists("trees")) # Something should have been saved
  
  # try making a little prediction to verify it works
  newData <- data.frame(x1=seq(from=-3, to=3, by=0.5), x2=0)
  predictions <- predict(forest, newData)
  
  # Also make sure we can load the forest too
  newforest <- loadForest("trees")
  predictions <- predict(newforest, newData)
  
  
  unlink("trees", recursive=TRUE)
  
})

test_that("Can save a random forest while training, and use it afterward with pure offline forest", {
  
  if(file.exists("trees")){ # folder could exist from a previous failed test; delete it
    unlink("trees", recursive=TRUE)
  }
  
  x1 <- rnorm(1000)
  x2 <- rnorm(1000)
  y <- 1 + x1 + x2 + rnorm(1000)
  
  data <- data.frame(x1, x2, y)
  forest <- train(y ~ x1 + x2, data,
                  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5,
                  savePath="trees", forest.output = "offline", displayProgress=FALSE)
  
  expect_true(file.exists("trees")) # Something should have been saved
  
  # try making a little prediction to verify it works
  newData <- data.frame(x1=seq(from=-3, to=3, by=0.5), x2=0)
  predictions <- predict(forest, newData)
  
  # Also make sure we can load the forest too
  newforest <- loadForest("trees")
  predictions <- predict(newforest, newData)
  
  # Last, make sure we can take the forest online
  onlineForest <- convertToOnlineForest(forest)
  predictions <- predict(onlineForest, newData)
  
  unlink("trees", recursive=TRUE)
  
})
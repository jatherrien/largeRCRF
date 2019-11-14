context("Train, save, and load without error")

test_that("Can save & load regression example", {

  if(file.exists("trees_saving_loading")){
    unlink("trees_saving_loading", recursive=TRUE)
  }
  
  expect_false(file.exists("trees_saving_loading")) # Folder shouldn't exist at this point
  
  x1 <- rnorm(1000)
  x2 <- rnorm(1000)
  y <- 1 + x1 + x2 + rnorm(1000)
  
  data <- data.frame(x1, x2, y)
  forest <- train(y ~ x1 + x2, data,
                  ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5, 
                  displayProgress=FALSE)
  
  
  saveForest(forest, "trees_saving_loading")
  new_forest <- loadForest("trees_saving_loading")
  
  # try making a little prediction to verify it works
  newData <- data.frame(x1=seq(from=-3, to=3, by=0.5), x2=0)
  predictions <- predict(new_forest, newData)              
  
  expect_true(file.exists("trees_saving_loading")) # show Ok if we got this far
  unlink("trees_saving_loading", recursive=TRUE)
  
})

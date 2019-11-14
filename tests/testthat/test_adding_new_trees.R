context("Add trees on existing forest")

test_that("Can add trees on existing online forest", {

  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, mtry=1, nodeSize=5, cores=2, displayProgress=FALSE)
  
  predictions <- predict(forest)
  
  forest.more <- addTrees(forest, 50, cores=2, displayProgress=FALSE) # test multi-core
  
  predictions <- predict(forest)
  
  forest.more <- addTrees(forest.more, 50, cores=1, displayProgress=FALSE) # test single-core
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Can add trees on existing offline forest", {
  
  if(file.exists("trees")){ # folder could exist from a previous failed test; delete it
    unlink("trees", recursive=TRUE)
  }
  
  
  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, 
                  numberOfSplits=0, mtry=1, nodeSize=5, 
                  forestResponseCombiner = CR_FunctionCombiner(events = 1:2, times = 0:10), # TODO - remove specifing times; this is workaround around unimplemented feature for offline forests
                  cores=2, displayProgress=FALSE, savePath="trees",
                  forest.output = "offline")
  warning("TODO - need to implement feature; test workaround in the meantime")
  
  predictions <- predict(forest)
  
  warning_message <- "Assuming that the previous forest at savePath is the provided forest argument; if not true then your results will be suspect"
  
  forest.more <- expect_warning(addTrees(forest, 50, cores=2, displayProgress=FALSE,
                          savePath="trees", savePath.overwrite = "merge",
                          forest.output = "offline"), fixed=warning_message) # test multi-core
  
  predictions <- predict(forest)
  
  forest.more <- expect_warning(addTrees(forest, 50, cores=1, displayProgress=FALSE,
                                         savePath="trees", savePath.overwrite = "merge",
                                         forest.output = "offline"), fixed=warning_message) # test single-core
  
  expect_true(T) # show Ok if we got this far
  
  unlink("trees", recursive=TRUE)
  
})

test_that("Test adding trees on saved forest - using delete", {
  
  if(file.exists("trees")){ # folder could exist from a previous failed test; delete it
    unlink("trees", recursive=TRUE)
  }
  
  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, 
                  mtry=1, nodeSize=5, cores=2,
                  savePath="trees", displayProgress=FALSE
                  )
  
  forest.more <- addTrees(forest, 50, cores=2, savePath = "trees", savePath.overwrite="delete", displayProgress=FALSE)
  
  predictions <- predict(forest)
  
  unlink("trees", recursive=TRUE)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Test adding trees on saved forest - using merge", {
  
  if(file.exists("trees")){ # folder could exist from a previous failed test; delete it
    unlink("trees", recursive=TRUE)
  }
  
  trainingData <- data.frame(x=rnorm(100))
  trainingData$T <- rexp(100) + abs(trainingData$x)
  trainingData$delta <- sample(0:2, size = 100, replace=TRUE)
  
  forest <- train(CR_Response(delta, T) ~ x, trainingData, ntree=50, numberOfSplits=0, 
                  mtry=1, nodeSize=5, cores=2,
                  savePath="trees", displayProgress=FALSE
  )
  
  expect_warning(
    addTrees(forest, 50, 
                          cores=2, 
                          savePath = "trees", 
                          savePath.overwrite="merge", 
                          displayProgress=FALSE),
  "Assuming that the previous forest at savePath is the provided forest argument; if not true then your results will be suspect" )
  
  unlink("trees", recursive=TRUE)
  
  expect_true(T) # show Ok if we got this far
  
})
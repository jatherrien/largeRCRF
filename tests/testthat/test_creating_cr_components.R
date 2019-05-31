context("Create CR Components without error")

# There's a bug where if eventsOfFocus is length 1 rJava can't find the constructor

test_that("Create LogRankSplitFinder", {
  LogRankSplitFinder(1:2,1)
  
  expect_true(T) # show Ok if we got this far
  
})

test_that("Create GrayLogRankSplitFinder", {
  GrayLogRankSplitFinder(1:2,1)
  
  expect_true(T) # show Ok if we got this far
  
})
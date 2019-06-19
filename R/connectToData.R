#' Connect To Data
#'
#' When a trained forest is saved, the training dataset is not saved alongside
#' it. When it's loaded back up, it can be more convenient (and in some cases
#' necessary) to import the training dataset back into the Java environment so
#' that it's readily accessible. There are only two functions that look for the
#' training dataset: \code{predict}, where you can easily just specify an
#' alternative dataset, or \code{\link{addTrees}}, which requires the training
#' dataset be connected.
#' @param forest The forest to connect data too
#' @param responses The responses in the data; aka the left hand side of the formula
#' @param covariateData A data.frame containing all of the covariates used in the training dataset
#' @return The same forest, but connected to the training data.
#' @export
#' @examples 
#' data <- data.frame(x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000))
#' forest <- train(y~x1+x2, data, ntree=100, numberOfSplits=0, nodeSize=1, mtry=1)
#' forest$dataset <- NULL # what the forest looks like after being loaded
#' 
#' forest <- connectToData(forest, data$y, data)
connectToData <- function(forest, responses, covariateData){
  covariateList <- forest$covariateList
  
  numCovariates <- .jcall(covariateList, "I", "size")
  covariateNames <- character(numCovariates)
  
  for(j in 1:numCovariates){
    covariate <- .jcall(covariateList, makeResponse(.class_Object), "get", as.integer(j-1))
    covariate <- .jcast(covariate, .class_Covariate)
    covariateNames[j] <- .jcall(covariate, makeResponse(.class_String), "getName")
  }
  
  forest$dataset <- loadData(covariateData, covariateNames, responses, covariateList)$dataset

  return(forest)
  
}
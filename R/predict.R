

#' Predict
#'
#' Predict on the random forest.
#'
#' @param forest A forest that was previously \code{\link{train}}ed
#' @param newData The new data containing all of the previous predictor
#'   covariates. Note that even if predictions are being made on the training
#'   set, the dataset must be specified. \code{largeRCRF} doesn't keep track of
#'   the dataset after the forest is trained.
#' @param parallel A logical indicating whether multiple cores should be
#'   utilized when making the predictions. Available as an option because it's
#'   been observed that using Java's \code{parallelStream} can be unstable on
#'   some systems. Default value is \code{TRUE}; only set to \code{FALSE} if you
#'   get strange errors while predicting.
#' @param out.of.bag A logical indicating whether predictions should be based on
#'   'out of bag' trees; set only to \code{TRUE} if you're running predictions
#'   on data that was used in the training. Default value is \code{FALSE}.
#' @return A list of responses corresponding with each row of \code{newData} if
#'   it's a non-regression random forest; otherwise it returns a numeric vector.
#' @export
#' @examples
#' # Regression Example
#' x1 <- rnorm(1000)
#' x2 <- rnorm(1000)
#' y <- 1 + x1 + x2 + rnorm(1000)
#'
#' data <- data.frame(x1, x2, y)
#' forest <- train(y ~ x1 + x2, data, ntree=100, numberOfSplits = 5, mtry = 1, nodeSize = 5)
#'
#' # Fix x2 to be 0
#' newData <- data.frame(x1 = seq(from=-2, to=2, by=0.5), x2 = 0)
#' ypred <- predict(forest, newData)
#'
#' plot(ypred ~ newData$x1, type="l")
#'
#' # Competing Risk Example
#' x1 <- abs(rnorm(1000))
#' x2 <- abs(rnorm(1000))
#'
#' T1 <- rexp(1000, rate=x1)
#' T2 <- rweibull(1000, shape=x1, scale=x2)
#' C <- rexp(1000)
#' u <- pmin(T1, T2, C)
#' delta <- ifelse(u==T1, 1, ifelse(u==T2, 2, 0))
#'
#' data <- data.frame(x1, x2)
#'
#' forest <- train(CR_Response(delta, u) ~ x1 + x2, data, ntree=100, numberOfSplits=5, mtry=1, nodeSize=10)
#' newData <- data.frame(x1 = c(-1, 0, 1), x2 = 0)
#' ypred <- predict(forest, newData)
predict.JRandomForest <- function(forest, newData=NULL, parallel=TRUE, out.of.bag=FALSE){
  if(is.null(newData)){
    stop("newData must be specified, even if predictions are on the training set") 
  }
  
  forestObject <- forest$javaObject
  covariateList <- forest$covariateList
  predictionClass <- forest$params$forestResponseCombiner$outputClass
  convertToRFunction <- forest$params$forestResponseCombiner$convertToRFunction

  predictionDataList <- loadPredictionData(newData, covariateList)
  
  if(parallel){
    function.to.use <- "evaluate"
  }
  else{
    function.to.use <- "evaluateSerial"
  }
  
  if(out.of.bag){
    function.to.use <- paste0(function.to.use, "OOB")
  }
  
  predictionsJava <- .jcall(forestObject, makeResponse(.class_List), function.to.use, predictionDataList)

  if(predictionClass == "numeric"){
    predictions <- vector(length=nrow(newData), mode="numeric")
  }
  else{
    predictions <- list()
  }


  for(i in 1:nrow(newData)){
    prediction <- .jcall(predictionsJava, makeResponse(.class_Object), "get", as.integer(i-1))
    prediction <- convertToRFunction(prediction, forest)

    predictions[[i]] <- prediction
  }

  class(predictions) <- paste0(predictionClass, ".List")

  return(predictions)
}



#' Variable Importance
#'
#' Calculate variable importance by recording the increase in error when a given
#' predictor is randomly permuted. Regression forests uses mean squared error;
#' competing risks uses integrated Brier score.
#'
#' @param forest The forest that was trained.
#' @param newData A test set of the data if available. If not, then out of bag
#'   errors will be attempted on the training set.
#' @param randomSeed The source of randomness used to permute the values. Can be
#'   left blank.
#' @param events If using competing risks forest, the events that the error
#'   measure used for VIMP should be calculated on.
#' @param time If using competing risks forest, the upper bound of the
#'   integrated Brier score.
#' @param censoringDistribution (Optional) If using competing risks forest, the
#'   censoring distribution. See \code{\link{integratedBrierScore} for details.}
#' @param eventWeights (Optional) If using competing risks forest, weights to be
#'   applied to the error for each of the \code{events}.
#'
#' @return A named numeric vector of importance values.
#' @export
#'
#' @examples
#' data(wihs)
#'
#' forest <- train(CR_Response(status, time) ~ ., wihs,
#'  ntree = 100, numberOfSplits = 0, mtry=3, nodeSize = 5)
#'
#' vimp(forest, events = 1:2, time = 8.0)
#' 
vimp <- function(
  forest,
  newData = NULL,
  randomSeed = NULL,
  type = c("mean", "z", "raw"),
  events = NULL,
  time = NULL,
  censoringDistribution = NULL,
  eventWeights = NULL){
  
  if(is.null(newData) & is.null(forest$dataset)){
    stop("forest doesn't have a copy of the training data loaded (this happens if you just loaded it); please manually specify newData and possibly out.of.bag")
  }
  
  # Basically we check if type is either null, length 0, or one of the invalid values.
  # We can't include the last statement in the same statement as length(tyoe) < 1,
  # because R checks both cases and a different error would display if length(type) == 0
  typeError = is.null(type) | length(type) < 1
  if(!typeError){
    typeError = !(type[1] %in% c("mean", "z", "raw"))
  }
  if(typeError){
    stop("A valid response type must be provided.")
  }
  
  if(is.null(newData)){
    data.java <- forest$dataset
    out.of.bag <- TRUE
    
  }
  else{ # newData is provided
    data.java <- processFormula(forest$formula, newData, forest$covariateList)$dataset
    out.of.bag <- FALSE
  }
  
  predictionClass <- forest$params$forestResponseCombiner$outputClass
  
  if(predictionClass == "CompetingRiskFunctions"){
    if(is.null(time) | length(time) != 1){
      stop("time must be set at length 1")
    }
    
    errorCalculator.java <- ibsCalculatorWrapper(
      events = events,
      time = time,
      censoringDistribution = censoringDistribution,
      eventWeights = eventWeights)
    
  } else if(predictionClass == "numeric"){
    errorCalculator.java <- .jnew(.class_RegressionErrorCalculator)
    errorCalculator.java <- .jcast(errorCalculator.java, .class_ErrorCalculator)
    
  } else{
    stop(paste0("VIMP not yet supported for ", predictionClass, ". If you're just using a non-custom version of largeRCRF then this is a bug and should be reported."))
    
  }
  
  forest.trees.java <- .jcall(forest$javaObject, makeResponse(.class_List), "getTrees")
  
  vimp.calculator <- .jnew(.class_VariableImportanceCalculator, 
                           errorCalculator.java, 
                           forest.trees.java,
                           data.java,
                           out.of.bag # isTrainingSet parameter
                           )
  
  random.java <- NULL
  if(!is.null(randomSeed)){
    random.java <- .jnew(.class_Random, .jlong(as.integer(randomSeed)))
  }
  random.java <- .object_Optional(random.java)
  
  covariateRList <- convertJavaListToR(forest$covariateList, class = .class_Covariate)
  importanceValues <- matrix(nrow = forest$params$ntree, ncol = length(covariateRList))
  colnames(importanceValues) <- extractCovariateNamesFromJavaList(forest$covariateList)
  
  for(j in 1:length(covariateRList)){
    covariateJava <- covariateRList[[j]]
    covariateJava <- 
    
    importanceValues[, j] <- .jcall(vimp.calculator, "[D", "calculateVariableImportanceRaw", covariateJava, random.java)
  }
  
  if(type[1] == "raw"){
    return(importanceValues)
  } else if(type[1] == "mean"){
    meanImportanceValues <- apply(importanceValues, 2, mean)
    return(meanImportanceValues)
  } else if(type[1] == "z"){
    zImportanceValues <- apply(importanceValues, 2, function(x){
      meanValue <- mean(x)
      standardError <- sd(x)/sqrt(length(x))
      return(meanValue / standardError)
    })
    return(zImportanceValues)
    
  } else{
    stop("A valid response type must be provided.")
  }

  
  return(importance)
  
}

# Internal function
ibsCalculatorWrapper <- function(events, time, censoringDistribution = NULL, eventWeights = NULL){
  if(is.null(events)){
    stop("events must be specified if using vimp on competing risks data")
  }
  
  if(is.null(time)){
    stop("time must be specified if using vimp on competing risks data")
  }
  
  
  java.censoringDistribution <- NULL
  if(!is.null(censoringDistribution)){
    java.censoringDistribution <- processCensoringDistribution(censoringDistribution)
    java.censoringDistribution <- .object_Optional(java.censoringDistribution)
  }
  else{
    java.censoringDistribution <- .object_Optional(NULL)
  }
  
  ibsCalculator.java <- .jnew(.class_IBSCalculator, java.censoringDistribution)
  
  if(is.null(eventWeights)){
    eventWeights <- rep(1, times = length(events))
  }
  
  ibsCalculatorWrapper.java <- .jnew(.class_IBSErrorCalculatorWrapper, 
                                     ibsCalculator.java,
                                     .jarray(as.integer(events)),
                                     as.numeric(time),
                                     .jarray(as.numeric(eventWeights)))
  
  ibsCalculatorWrapper.java <- .jcast(ibsCalculatorWrapper.java, .class_ErrorCalculator)
  return(ibsCalculatorWrapper.java)
  
  
}
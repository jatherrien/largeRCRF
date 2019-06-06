
# Internal function used to convert the Java functions into R functions
# Provided for use as a parameter in CR_FunctionCombiner & CR_ResponseCombiner
convertCompetingRiskFunctions <- compiler::cmpfun(function(javaObject, forest){
  events <- forest$params$forestResponseCombiner$events
  lst <- list(javaObject = javaObject, events = events)
  
  rightContinuousStepFunctionResponseClass <- makeResponse(.class_RightContinuousStepFunction)
  
  kaplanMeier <- .jcall(javaObject, rightContinuousStepFunctionResponseClass, "getSurvivalCurve")
  
  lst$time.interest <- .jcall(.class_RUtils, "[D", "extractTimes", kaplanMeier)
  lst$survivorCurve <- .jcall(.class_RUtils, "[D", "extractY", kaplanMeier)
  
  lst$cif <- matrix(nrow=length(lst$time.interest), ncol=length(events))
  lst$chf <- matrix(nrow=length(lst$time.interest), ncol=length(events))
  
  for(i in events){
    cif <- .jcall(javaObject, rightContinuousStepFunctionResponseClass, "getCumulativeIncidenceFunction", as.integer(i))
    lst$cif[,i] <- .jcall(.class_RUtils, "[D", "extractY", cif)
    
    chf <- .jcall(javaObject, rightContinuousStepFunctionResponseClass, "getCauseSpecificHazardFunction", as.integer(i))
    lst$chf[,i] <- .jcall(.class_RUtils, "[D", "extractY", chf)
  }
  
  class(lst) <- "CompetingRiskFunctions"
  return(lst)
})


#' Competing Risk Predictions
#'
#' @param x The predictions output from a competing risk random forest.
#' @param event The event who's CIF/CHF/Mortality you are interested in.
#' @param time The time to evaluate the mortality for (relevant only for
#'   \code{extractMortalities}).
#'
#' @name CompetingRiskPredictions
NULL

#' @rdname CompetingRiskPredictions
#' @export
#' @description
#' \code{extractCIF} extracts the cumulative incidence function for a prediction.
extractCIF <- function (x, event) {
  UseMethod("extractCIF", x)
}

#' @export
extractCIF.CompetingRiskFunctions <- function(prediction, event){
  fun <- stepfun(prediction$time.interest, c(0, prediction$cif[,event]))
  
  class(fun) <- "function"
  attr(fun, "call") <- sys.call()
  return(fun)
}

#' @export
extractCIF.CompetingRiskFunctions.List <- function(predictions, event){
  return(lapply(predictions, extractCIF.CompetingRiskFunctions, event))
}

#' @rdname CompetingRiskPredictions
#' @export
#' @description
#' \code{extractCHF} extracts the cause-specific cumulative hazard function for a prediction.
extractCHF <- function (x, event) {
  UseMethod("extractCHF", x)
}

#' @export
extractCHF.CompetingRiskFunctions <- function(prediction, event){
  fun <- stepfun(prediction$time.interest, c(0, prediction$chf[,event]))
  
  class(fun) <- "function"
  attr(fun, "call") <- sys.call()
  return(fun)
}

#' @export
extractCHF.CompetingRiskFunctions.List <- function(predictions, event){
  return(lapply(predictions, extractCHF.CompetingRiskFunctions, event))
}


#' @rdname CompetingRiskPredictions
#' @export
#' @description \code{extractSurvivorCurve} extracts the Kaplan-Meier estimator
#' of the overall survivor curve for a prediction.
extractSurvivorCurve <- function (x) {
  UseMethod("extractSurvivorCurve", x)
}

#' @export
extractSurvivorCurve.CompetingRiskFunctions <- function(prediction){
  fun <- stepfun(prediction$time.interest, c(1, prediction$survivorCurve))
  
  class(fun) <- "function"
  attr(fun, "call") <- sys.call()
  return(fun)
}

#' @export
extractSurvivorCurve.CompetingRiskFunctions.List <- function(predictions){
  return(lapply(predictions, extractSurvivorCurve.CompetingRiskFunctions))
}

#' @rdname CompetingRiskPredictions
#' @export
#' @description \code{extractMortalities} extracts the cause-specific
#' mortalities for a function, which here is the CIF integrated from 0 to
#' \code{time}.
extractMortalities <- function(x, event, time){
  UseMethod("extractMortalities", x)
}

#' @export
extractMortalities.CompetingRiskFunctions <- function(prediction, event, time){
  if(is.null(event) | anyNA(event)){
    stop("event must be specified")
  }
  
  if(is.null(time) | anyNA(time)){
    stop("time must be specified")
  }
  
  return(.jcall(prediction$javaObject, "D", "calculateEventSpecificMortality", as.integer(event), time))
}

#' @export
extractMortalities.CompetingRiskFunctions.List <- function(predictions, event, time){
  if(is.null(event) | anyNA(event)){
    stop("event must be specified")
  }
  
  if(is.null(time) | anyNA(time)){
    stop("time must be specified")
  }
  
  return(as.numeric(lapply(predictions, extractMortalities.CompetingRiskFunctions, event, time)))
}
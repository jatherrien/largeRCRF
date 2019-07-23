

#' Integrated Brier Score
#'
#' Used to calculate the Integrated Brier Score, which for the competing risks
#' setting is the integral of the squared difference between each observed
#' cumulative incidence function (CIF) for each observation and the
#' corresponding predicted CIF. If the survivor function (1 - CDF) of the
#' censoring distribution is provided, weights can be calculated to account for
#' the censoring.
#'
#' @return A numeric vector of the Integrated Brier Score for each prediction.
#' @param responses A list of responses corresponding to the provided
#'   mortalities; use \code{\link{CR_Response}}.
#' @param predictions The predictions to be tested against.
#' @param event The event type for the error to be calculated on.
#' @param time \code{time} specifies the upper bound of the integral.
#' @param censoringDistribution Optional; if provided then weights are
#'   calculated on the errors. There are three ways to provide it - \itemize{
#'   \item{If you have all the censor times and just want to use a simple
#'   empirical estimate of the distribution, just provide a numeric vector of
#'   all of the censor times and it will be automatically calculated.} \item{You
#'   can directly specify the survivor function by providing a list with two
#'   numeric vectors called \code{x} and \code{y}. They should be of the same
#'   length and correspond to each point. It is assumed that previous to the
#'   first value in \code{y} the \code{y} value is 1.0; and that the function
#'   you provide is a right-continuous step function.} \item{You can provide a
#'   function from \code{\link[stats]{stepfun}}. Note that this only supports
#'   functions where \code{right = FALSE} (default), and that the first y value
#'   (corresponding to y before the first x value) will be to set to 1.0
#'   regardless of what is specified.}
#'
#'   }
#' @param parallel A logical indicating whether multiple cores should be
#'   utilized when calculating the error. Available as an option because it's
#'   been observed that using Java's \code{parallelStream} can be unstable on
#'   some systems. Default value is \code{TRUE}; only set to \code{FALSE} if you
#'   get strange errors while predicting.
#'
#' @export
#' @references Section 4.2 of Ishwaran H, Gerds TA, Kogalur UB, Moore RD, Gange
#'   SJ, Lau BM (2014). “Random Survival Forests for Competing Risks.”
#'   Biostatistics, 15(4), 757–773. doi:10.1093/ biostatistics/kxu010.
#'
#' @examples
#' data <- data.frame(delta=c(1,1,0,0,2,2), T=1:6, x=1:6)
#'
#' model <- train(CR_Response(delta, T) ~ x, data, ntree=100, numberOfSplits=0, mtry=1, nodeSize=1)
#'
#' newData <- data.frame(delta=c(1,0,2,1,0,2), T=1:6, x=1:6)
#' predictions <- predict(model, newData)
#'
#' scores <- integratedBrierScore(CR_Response(data$delta, data$T), predictions, 1, 6.0)
#' 
integratedBrierScore <- function(responses, predictions, event, time, censoringDistribution = NULL, parallel = TRUE){
  if(length(responses$eventTime) != length(predictions)){
    stop("Length of responses and predictions must be equal.")
  } 
  
  java.censoringDistribution <- NULL
  if(!is.null(censoringDistribution)){
    if(is.numeric(censoringDistribution)){
      # estimate ECDF
      censoringTimes <- .jarray(censoringDistribution, "D")
      java.censoringDistribution <- .jcall(.class_Utils, makeResponse(.class_RightContinuousStepFunction), "estimateOneMinusECDF", censoringTimes)
      
    } else if(is.list(censoringDistribution)){
      # First check that censoringDistribution fits the correct format
      if(is.null(censoringDistribution$x) | is.null(censoringDistribution$y)){
        stop("If the censoringDistribution is provided as a list, it must have an x and a y item that are numeric.")
      }
      
      if(length(censoringDistribution$x) != length(censoringDistribution$y)){
        stop("x and y in censoringDistribution must have the same length.")
      }
      
      if(!is.numeric(censoringDistribution$x) | !is.numeric(censoringDistribution$y)){
        stop("x and y in censoringDistribution must both be numeric.")
      }
      
      java.censoringDistribution <- createRightContinuousStepFunction(censoringDistribution$x, censoringDistribution$y, defaultY = 1.0)
      
    } else if("stepfun" %in% class(censoringDistribution)){
      x <- knots(censoringDistribution)
      y <- censoringDistribution(x)
      
      java.censoringDistribution <- createRightContinuousStepFunction(x, y, defaultY = 1.0)
    }
    else{
      stop("Invalid censoringDistribution")
    }
    
    # Make sure we wrap it in an Optional
    java.censoringDistribution <- .object_Optional(java.censoringDistribution)
    
  }
  else{
    java.censoringDistribution <- .object_Optional(NULL)
  }
  
  predictions.java <- lapply(predictions, function(x){return(x$javaObject)})
  predictions.java <- convertRListToJava(predictions.java)
  
  errors <- .jcall(.class_CompetingRiskUtils, "[D", "calculateIBSError", 
         responses$javaObject, 
         predictions.java, 
         java.censoringDistribution,
         as.integer(event), 
         time,
         parallel)
  
  return(errors)
  
}
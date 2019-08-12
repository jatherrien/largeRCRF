

# Internal function. Takes a censoring distribution and turns it into a
# RightContinuousStepFunction Java object.
processCensoringDistribution <- function(censoringDistribution){

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
    x <- stats::knots(censoringDistribution)
    y <- censoringDistribution(x)
    
    java.censoringDistribution <- createRightContinuousStepFunction(x, y, defaultY = 1.0)
  }
  else{
    stop("Invalid censoringDistribution")
  }
    
  return(java.censoringDistribution)
}
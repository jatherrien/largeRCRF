
# Internal function that takes a formula and processes it for use in the Java
# code. existingCovariateList is optional; if not provided then a new one is
# created internally.
processFormula <- function(formula, data, covariateList.java = NULL, na.penalty = NULL){
  
  # Having an R copy of the data loaded at the same time can be wasteful; we
  # also allow users to provide an environment of the data which gets removed
  # after being imported into Java
  if(class(data) == "environment"){
    if(is.null(data$data)){
      stop("When providing an environment with the dataset, the environment must contain an item called 'data'")
    }
    
    env <- data
    data <- env$data
    env$data <- NULL
    gc()
  }
  
  yVar <- formula[[2]]
  
  responses <- NULL
  variablesToDrop <- character(0)
  
  # yVar is a call object; as.character(yVar) will be the different components, including the parameters.
  # if the length of yVar is > 1 then it's a function call. If the length is 1, and it's not in data, 
  # then we also need to explicitly evaluate it
  if(class(yVar) == "call" || !(as.character(yVar) %in% colnames(data))){
    # yVar is a function like CR_Response
    responses <- eval(expr=yVar, envir=data)
    
    if(class(formula[[3]]) == "name" && as.character(formula[[3]])=="."){
      # do any of the variables match data in data? We need to track that so we can drop them later
      variablesToDrop <- as.character(yVar)[as.character(yVar) %in% names(data)]
    }
    
    formula[[2]] <- NULL
    
  } else if(class(yVar) == "name"){ # and implicitly yVar is contained in data
    variablesToDrop <- as.character(yVar)
  }
  
  # Includes responses which we may need to later cut out if `.` was used on the
  # right-hand-side
  filteredData <- stats::model.frame(formula=formula, data=data, na.action=stats::na.pass)
  
  if(is.null(responses)){ # If this if-statement runs then we have a simple (i.e. numeric) response
    responses <- stats::model.response(filteredData)
  }
  
  # remove any response variables on the right-hand-side
  covariateData <- filteredData[, !(names(filteredData) %in% variablesToDrop), drop=FALSE]
  
  # Now that we know how many predictor variables we have, we should check na.penalty
  if(!is.null(na.penalty)){
    if(!is.numeric(na.penalty) & !is.logical(na.penalty)){
      stop("na.penalty must be either logical or numeric.")
    }
    
    if(is.logical(na.penalty) & length(na.penalty) != 1 & length(na.penalty) != ncol(covariateData)){
      stop("na.penalty must have length of either 1 or the number of predictor variables if logical.")
    }
    
    if(is.numeric(na.penalty) & length(na.penalty) != 1){
      stop("na.penalty must have length 1 if logical.")
    }
    
    if(anyNA(na.penalty)){
      stop("na.penalty cannot contain NAs.")
    }

    
    # All good; now to transform it.
    if(is.numeric(na.penalty)){
      na.threshold <- na.penalty
      na.penalty <- apply(covariateData, 2, function(x){mean(is.na(x))}) >= na.threshold
    }
    else if(is.logical(na.penalty) & length(na.penalty) == 1){
      na.penalty <- rep(na.penalty, times = ncol(covariateData))
    }
    # else{} - na.penalty is logical and the correct length; no need to do anything to it
    
  }
  
  dataset <- loadData(
    covariateData, 
    colnames(covariateData), 
    responses, 
    covariateList.java = covariateList.java,
    na.penalty = na.penalty
  )
  
  return(dataset)
}
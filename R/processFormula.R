
# Internal function that takes a formula and processes it for use in the Java
# code. existingCovariateList is optional; if not provided then a new one is
# created internally.
processFormula <- function(formula, data, covariateList.java = NULL){
  
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
  
  
  dataset <- loadData(covariateData, colnames(covariateData), responses, covariateList.java = covariateList.java)
  
  return(dataset)
}
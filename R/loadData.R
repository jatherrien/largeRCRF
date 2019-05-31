loadData <- function(data, xVarNames, responses){

  if(class(responses) == "integer" | class(responses) == "numeric"){
    responses <- Numeric(responses)
  }

  covariateList.java <- getCovariateList(data, xVarNames)
  
  textColumns <- list()
  for(j in 1:length(xVarNames)){
    textColumns[[j]] <- .jarray(as.character(data[,xVarNames[j]]), "S")
  }
  textData <- convertRListToJava(textColumns)
  
  rowList <- .jcall(.class_RUtils, makeResponse(.class_List), "importDataWithResponses",
                    responses$javaObject, covariateList.java, textData)

  return(list(covariateList=covariateList.java, dataset=rowList))

}

getCovariateList <- function(data, xvarNames){
  covariateList <- .jcast(.jnew(.class_ArrayList, length(xvarNames)), .class_List)

  for(i in 1:length(xvarNames)){
    xName = xvarNames[i]

    column <- data[,xName]

    if(class(column) == "numeric" | class(column) == "integer"){
      covariate <- Java_NumericCovariate(xName, i-1)
    }
    else if(class(column) == "logical"){
      covariate <- Java_BooleanCovariate(xName, i-1)
    }
    else if(class(column) == "factor"){
      lvls <- levels(column)
      covariate <- Java_FactorCovariate(xName, i-1, lvls)
    }
    else{
      stop("Unknown column type")
    }

    .jcall(covariateList, "Z", "add", covariate)

  }

  return(covariateList)

}

loadPredictionData <- function(newData, covariateList.java){
  
  xVarNames <- character(.jcall(covariateList.java, "I", "size"))
  for(j in 1:length(xVarNames)){
    covariate.java <- .jcast(
      .jcall(covariateList.java, makeResponse(.class_Object), "get", as.integer(j-1)),
      .class_Covariate
    )
    
    xVarNames[j] <- .jcall(covariate.java, makeResponse(.class_String), "getName")
  }
  
  if(any(!(xVarNames %in% names(newData)))){
    varsMissing = xVarNames[!(xVarNames %in% names(newData))]
    
    error <- paste0("The following covariates are not present in newdata: ", paste(varsMissing, collapse = ", "))
    stop(error)
  }

  textColumns <- list()
  for(j in 1:length(xVarNames)){
    textColumns[[j]] <- .jarray(as.character(newData[,xVarNames[j]]), "S")
  }
  textData <- convertRListToJava(textColumns)
  
  rowList <- .jcall(.class_RUtils, makeResponse(.class_List), 
                    "importData", covariateList.java, textData)
  

  return(rowList)
}

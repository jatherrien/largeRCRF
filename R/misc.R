# Internal function
convertRListToJava <- function(lst){
  javaList <- .jnew(.class_ArrayList, as.integer(length(lst)))
  javaList <- .jcast(javaList, .class_List)

  for (item in lst){
    if (class(item) != "jobjRef" & class(item) != "jarrayRef"){
      stop("All items in the list must be rJava Java objects")
    }

    .jcall(javaList, "Z", "add", .jcast(item, .class_Object))
  }

  return(javaList)
}

#Internal function
convertJavaListToR <- function(javaList, class = .class_Object){
  lst <- list()
  
  javaList.length <- .jcall(javaList, "I", "size")
  
  for(i in 0:(javaList.length - 1)){
    object <- .jcall(javaList, makeResponse(.class_Object), "get", as.integer(i))
    object <- .jcast(object, class)
    
    lst[[i+1]] <- object
  }
  
  return(lst)
  
}

#' @export
print.SplitFinder = function(x, ...) print(x$call)

#' @export
print.ResponseCombiner = function(x, ...) print(x$call)

#' @export
print.JRandomForest <- function(x, ...){
  cat("Call:\n")
  print(x$call)
  cat("\nParameters:\n")
  cat("\tSplit Finder: "); print(x$params$splitFinder$call)
  cat("\tTerminal Node Response Combiner: "); print(x$params$nodeResponseCombiner$call)
  cat("\tForest Response Combiner: "); print(x$params$forestResponseCombiner$call)
  cat("\t# of trees: "); cat(x$params$ntree); cat("\n")
  cat("\t# of Splits: "); cat(x$params$numberOfSplits); cat("\n")
  cat("\t# of Covariates to try: "); cat(x$params$mtry); cat("\n")
  cat("\tNode Size: "); cat(x$params$nodeSize); cat("\n")
  cat("\tMax Node Depth: "); cat(x$params$maxNodeDepth); cat("\n")

  cat("Try using me with predict() or one of the relevant commands to determine error\n")
}

#' @export
print.CompetingRiskFunctions.List <- function(x, ...){
  cat("Number of predictions: ")
  cat(length(x))

  cat("\n\nSee the help page ?CompetingRiskPredictions for a list of relevant functions on how to use this object.\n")
}

#' @export
print.CompetingRiskFunctions <- function(x, ...){
  mx <- ncol(x$cif)
  cat(mx); cat(" CIFs available\n")
  cat(mx); cat(" CHFs available\n")
  cat("An overall survival curve available\n")
  cat("\nSee the help page ?CompetingRiskPredictions for a list of relevant functions on how to use this object.\n")

}



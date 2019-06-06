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

#' @export
print.SplitFinder = function(splitFinder) print(splitFinder$call)

#' @export
print.ResponseCombiner = function(combiner) print(combiner$call)

#' @export
print.JRandomForest <- function(forest){
  cat("Call:\n")
  print(forest$call)
  cat("\nParameters:\n")
  cat("\tSplit Finder: "); print(forest$params$splitFinder$call)
  cat("\tTerminal Node Response Combiner: "); print(forest$params$nodeResponseCombiner$call)
  cat("\tForest Response Combiner: "); print(forest$params$forestResponseCombiner$call)
  cat("\t# of trees: "); cat(forest$params$ntree); cat("\n")
  cat("\t# of Splits: "); cat(forest$params$numberOfSplits); cat("\n")
  cat("\t# of Covariates to try: "); cat(forest$params$mtry); cat("\n")
  cat("\tNode Size: "); cat(forest$params$nodeSize); cat("\n")
  cat("\tMax Node Depth: "); cat(forest$params$maxNodeDepth); cat("\n")

  cat("Try using me with predict() or one of the relevant commands to determine error\n")
}

#' @export
print.CompetingRiskFunctions.List <- function(lst){
  cat("Number of predictions: ")
  cat(length(lst))

  cat("\n\nSee the help page ?CompetingRiskPredictions for a list of relevant functions on how to use this object.\n")
}

#' @export
print.CompetingRiskFunctions <- function(functions){
  mx <- ncol(functions$cif)
  cat(mx); cat(" CIFs available\n")
  cat(mx); cat(" CHFs available\n")
  cat("An overall survival curve available\n")
  cat("\nSee the help page ?CompetingRiskPredictions for a list of relevant functions on how to use this object.\n")

}

#' @export
plot.JMatrixPlottable <- function(mat, add=FALSE, type="s", xlab="Time", ylab=NULL, col="black", ...){
  if(!add){
    if(is.null(ylab)){
      matType <- attr(mat, "type")
      event <- attr(mat, "event")

      if(matType == "cif"){
        ylab <- paste0("CIF-", event, "(t)")
      }
      else if(matType == "chf"){
        ylab <- paste0("CHF(t)-", event, "(t)")
      }
      else if(matType == "kaplanMeier"){
        ylab <- "S-hat(t)"
      }
      else{
        ylab <- "Y"
        warning("Unknown type attribute in plottable object")
      }

    }

    plot(mat[,2] ~ mat[,1], col=col, type=type, xlab=xlab, ylab=ylab, ...)
  }
  else{
    points(mat[,2] ~ mat[,1], col=col, type=type, xlab=xlab, ylab=ylab, ...)
  }

}

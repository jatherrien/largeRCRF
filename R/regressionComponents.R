
#' WeightedVarianceSplitFinder
#'
#' This split finder is used in regression random forests. When a split is made,
#' this finder computes the sample variance in each group (divided by n, not
#' n-1); it then minimizes the sum of these variances, each of them weighted by
#' their sample size divided by the total sample size of that node.
#'
#' @note There are other split finders that are used in regression random
#'   forests that are not included in this package. This package is oriented
#'   toward the competing risks side of survival analysis; the regression
#'   options are provided as an example of how extensible the back-end Java
#'   package is. If you are interested in using this package for regression (or
#'   other uses), feel free to write your own components. It's not too hard to
#'   write these components; the WeightedVarianceSplitFinder Java class is quite
#'   short; most of the code is to reuse calculations from previous considered
#'   splits. I (the author) am also willing to assist if you have any questions.
#' @export
#' @return A split finder object to be used in \code{\link{train}}; not useful
#'   on its own.
#' @examples
#' splitFinder <- WeightedVarianceSplitFinder()
#' # You would then use it in train()
#'
#' @references https://kogalur.github.io/randomForestSRC/theory.html#section8.3
WeightedVarianceSplitFinder <- function(){
  javaObject <- .jnew(.class_WeightedVarianceSplitFinder)
  javaObject <- .jcast(javaObject, .class_SplitFinder)

  splitFinder <- list(javaObject=javaObject, call=match.call())
  class(splitFinder) <- "SplitFinder"

  return(splitFinder)
}

#' MeanResponseCombiner
#'
#' This response combiner is used in regression random forests, where the
#' response in the data is a single number that needs to be averaged in each
#' terminal node, and then averaged across trees. This response combiner is
#' appropriate as an argument for both the \code{nodeResponseCombiner} and
#' \code{forestResponseCombiner} parameters in \code{\link{train}} when doing
#' regression.
#' @export
#' @examples
#' responseCombiner <- MeanResponseCombiner()
#' # You would then use it in train()
#'
MeanResponseCombiner <- function(){
  javaObject <- .jnew(.class_MeanResponseCombiner)
  javaObject <- .jcast(javaObject, .class_ForestResponseCombiner)

  combiner <- list(javaObject=javaObject, call=match.call(), outputClass="numeric")
  combiner$convertToRFunction <- function(javaObject, ...){
    return(.jcall(javaObject, "D", "doubleValue"))
  }


  class(combiner) <- "ResponseCombiner"

  return(combiner)
}


#' WeightedVarianceSplitFinder
#'
#' This split finder is used in regression random forests. When a split is made,
#' this finder computes the sample variance in each group (divided by n, not
#' n-1); it then minimizes the the sum of these variances, each of them weighted
#' by their sample size divided by the total sample size of that node.
#'
#' @note There are other split finders that are used in regression random
#'   forests that are not included in this package. This package is oriented
#'   toward the competing risk side of survival analysis; the regression options
#'   are provided as an example of how extensible the back-end Java package is.
#'   If you are interested in using this package for regression (or other uses),
#'   feel free to write your own components. It's really not hard to write these
#'   components; the WeightedVarianceSplitFinder Java class is quite short; most
#'   of the code is to reuse calculations from previous considered splits. 
#' @export
#' @return A split finder object to be used in \code{\link{train}}; not
#'   useful on its own.
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
#' @return A response combiner object to be used in \code{\link{train}}; not
#'   useful on its own. However, internally, a response combiner object is a
#'   list consisting of the following objects:
#'  \describe{
#'   \item{\code{javaObject}}{The java object used in the algorithm}
#'   \item{\code{call}}{The call (used in \code{print})}
#'   \item{\code{outputClass}}{The R class of the outputs; used in \code{\link{predict.JRandomForest}}}
#'   \item{\code{convertToRFunction}}{An R function that converts a Java prediction from the combiner into R output that is readable by a user.}
#' }
#'
#' @examples
#' responseCombiner <- MeanResponseCombiner()
#' # You would then use it in train()
#'
#' # However; I'll show an internal Java method to make it clear what it does
#' # Note that you should never have to do the following
#' x <- 1:3
#' x <- convertRListToJava(Numeric(x))
#'
#' # will output a Java object containing 2
#' output <- rJava::.jcall(responseCombiner$javaObject, "Ljava/lang/Double;", "combine", x)
#' responseCombiner$convertToRFunction(output)
#'
MeanResponseCombiner <- function(){
  javaObject <- .jnew(.class_MeanResponseCombiner)
  javaObject <- .jcast(javaObject, .class_ResponseCombiner)

  combiner <- list(javaObject=javaObject, call=match.call(), outputClass="numeric")
  combiner$convertToRFunction <- function(javaObject, ...){
    return(.jcall(javaObject, "D", "doubleValue"))
  }


  class(combiner) <- "ResponseCombiner"

  return(combiner)
}
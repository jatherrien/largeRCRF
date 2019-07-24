
#' Numeric
#'
#' An internal function that converts an R vector of numerics or integers into
#' an R list containing java.lang.Double objects. This method does not need to
#' be used directly by the user, as \code{\link{train}} will automatically
#' handle numeric responses if you're working in the regression settings.
#' @param y The R vector of numbers
#' @export
#' @return An R list containing rJava Doubles.
#' @keywords internal
#' @examples
#' x <- Numeric(1:5)
#' class(x[[1]])
Numeric <- function(y){
  y <- as.double(y)
  
  javaList <- .jcall(.class_RUtils, 
                     makeResponse(.class_List), 
                     "importNumericResponse", 
                     y)

  responses <- list(javaObject=javaList, y=y)

  class(responses) <- "JNumeric"

  return(responses)
}

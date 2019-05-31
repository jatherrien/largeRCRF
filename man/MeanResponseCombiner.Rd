% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/regressionComponents.R
\name{MeanResponseCombiner}
\alias{MeanResponseCombiner}
\title{MeanResponseCombiner}
\usage{
MeanResponseCombiner()
}
\value{
A response combiner object to be used in \code{\link{train}}; not
  useful on its own. However, internally, a response combiner object is a
  list consisting of the following objects:
 \describe{
  \item{\code{javaObject}}{The java object used in the algorithm}
  \item{\code{call}}{The call (used in \code{print})}
  \item{\code{outputClass}}{The R class of the outputs; used in \code{\link{predict.JRandomForest}}}
  \item{\code{convertToRFunction}}{An R function that converts a Java prediction from the combiner into R output that is readable by a user.}
}
}
\description{
This response combiner is used in regression random forests, where the
response in the data is a single number that needs to be averaged in each
terminal node, and then averaged across trees. This response combiner is
appropriate as an argument for both the \code{nodeResponseCombiner} and
\code{forestResponseCombiner} parameters in \code{\link{train}} when doing
regression.
}
\examples{
responseCombiner <- MeanResponseCombiner()
# You would then use it in train()

# However; I'll show an internal Java method to make it clear what it does
# Note that you should never have to do the following
x <- 1:3
x <- convertRListToJava(Numeric(x))

# will output a Java object containing 2
output <- rJava::.jcall(responseCombiner$javaObject, "Ljava/lang/Double;", "combine", x)
responseCombiner$convertToRFunction(output)

}
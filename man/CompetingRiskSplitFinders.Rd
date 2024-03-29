% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cr_components.R
\name{CompetingRiskSplitFinders}
\alias{CompetingRiskSplitFinders}
\alias{GrayLogRankSplitFinder}
\alias{LogRankSplitFinder}
\title{Competing Risk Split Finders}
\usage{
GrayLogRankSplitFinder(events, eventsOfFocus = NULL)

LogRankSplitFinder(events, eventsOfFocus = NULL)
}
\arguments{
\item{events}{A vector of integers specifying which competing risk events
should be focused on when determining differences. Currently, equal weights
will be assigned to all included groups.}

\item{eventsOfFocus}{The split finder will only maximize differences
between the two groups with respect to these specified events. Default is
\code{NULL}, which will cause the split finder to focus on all events
included in \code{events}.}
}
\value{
An internal rJava Java object used in \code{\link{train}}.
}
\description{
Creates a SplitFinder rJava Java object, which is then used internally when
training a competing risk random forest. The split finder is responsible for
finding the best split according to the logic of the split finder.
}
\details{
These split finders require that the response be \code{\link{CR_Response}}.

The user only needs to pass this object into \code{\link{train}} as the
\code{splitFinder} parameter.

Roughly speaking, the Gray log-rank split finder looks at
  differences between the cumulative incidence functions of the two groups,
  while the plain log-rank split finder look at differences between the
  cause-specific hazard functions. See the references for a more detailed
  discussion.
}
\note{
The Gray log-rank split finder \strong{requires} that the response
  include the censoring time.
}
\examples{
splitFinder <- GrayLogRankSplitFinder(1:2)
splitFinder <- LogRankSplitFinder(1:2)
}
\references{
Kogalur, U., Ishwaran, H. Random Forests for Survival,
  Regression, and Classification: A Parallel Package for a General
  Implemention of Breiman's Random Forests: Theory and Specifications. URL
  https://kogalur.github.io/randomForestSRC/theory.html#section8.2

  Ishwaran, H., et. al. (2014) Random survival forests for competing risks,
  Biostatistics (2014), 15, 4, pp. 757–773
}

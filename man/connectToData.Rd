% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/connectToData.R
\name{connectToData}
\alias{connectToData}
\title{Connect To Data}
\usage{
connectToData(forest, responses, covariateData)
}
\arguments{
\item{forest}{The forest to connect data too}

\item{responses}{The responses in the data; aka the left hand side of the formula}

\item{covariateData}{A data.frame containing all of the covariates used in the training dataset}
}
\value{
The same forest, but connected to the training data.
}
\description{
When a trained forest is saved, the training dataset is not saved alongside
it. When it's loaded back up, it can be more convenient (and in some cases
necessary) to import the training dataset back into the Java environment so
that it's readily accessible. There are only two functions that look for the
training dataset: \code{predict}, where you can easily just specify an
alternative dataset, or \code{\link{addTrees}}, which requires the training
dataset be connected.
}
\examples{
data <- data.frame(x1=rnorm(1000), x2=rnorm(1000), y=rnorm(1000))
forest <- train(y~x1+x2, data, ntree=100, numberOfSplits=0, nodeSize=1, mtry=1)
forest$dataset <- NULL # what the forest looks like after being loaded

forest <- connectToData(forest, data$y, data)
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/goldeneye_algorithm.R
\name{prune.singletons}
\alias{prune.singletons}
\title{An iterative algorithm to prune singletons.}
\usage{
prune.singletons(data, model, delta = (1/sqrt(nrow(data))), grouping = NULL,
  class.name = "PClass", goodness.function = fidelity)
}
\arguments{
\item{data}{The dataset.}

\item{model}{A model created from the data using a classifier.}

\item{delta}{The sensitivity parameter. Defaults to 1 / sqrn(nrow(data)).}

\item{grouping}{A grouping parametrizing the data.}

\item{class.name}{The name of the column in the dataset with the predicted classes.}

\item{goodness.function}{The function used to investigate the effect of randomizing attributes in the dataset. Optional. Default is \code{fidelity}.}
}
\value{
A list containing the optimal grouping, with singletons pruned.
}
\description{
This function iteratively prunes those singletons from a given grouping
that do not affect the fidelity more than the given sensitivity.
}
\examples{
data <- gen.data.single.xor()
train <- testsplit(data)
model <- classifier.single.xor(Class~., data = data[train, -ncol(data)])
data[,"PClass"]  <- predict(model, newdata = data)
res <- grouping(data = data, model = model)
S.pruned <- prune.singletons(data = data, model = model, grouping = res$S)

}


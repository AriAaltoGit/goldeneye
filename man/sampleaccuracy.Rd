\name{sampleaccuracy}
\alias{sampleaccuracy}
\title{Obtain a sample of classification accuracy of the model when the
data has been permuted as parametrized by tree. Notice pred makes
sense only if the true class variable has not been permuted!}
\usage{
sampleaccuracy(model, tree, data, class = "Class")
}
\arguments{
  \item{model}{A model of the data from a classifier}

  \item{tree}{A permutation tree}

  \item{data}{The data matrix}

  \item{class}{The name of the column with the class label}
}
\value{
The classification accuracy
}
\description{
Obtain a sample of classification accuracy of the model
when the data has been permuted as parametrized by tree.
Notice pred makes sense only if the true class variable has
not been permuted!
}


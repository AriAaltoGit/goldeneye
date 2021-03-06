% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utilities.R
\name{calculate.importance}
\alias{calculate.importance}
\title{Calculate the importance of attribute groups and individual attributes.}
\usage{
calculate.importance(res, pred.class.name = "PClass",
  pred.class.index = NULL)
}
\arguments{
\item{res}{The output from GoldenEye using the arguments return.model = TRUE and return.data = TRUE.}

\item{pred.class.name}{The name of the column with the predicted class label.}

\item{pred.class.index}{The index of the column with the predicted class label.}
}
\value{
The input structure res with the following fields added:
\describe{
\item{group.scores}{The fidelity of each group in the optimal grouping. A lower fidelity score implies a more important group. Returned if calculate.importance is TRUE.}
\item{attribute.scores}{The fidelity score for each individual attribute in the dataset. A lower fidelity score implies a more important attribute. Returned if calculate.importance is TRUE.}
\item{attribute.ranks}{The rank order of the attribute scors. A lower rank implies a more important attribute. Returned if calculate.importance is TRUE.}
}
}
\description{
Calculate the importance of attribute groups and individual attributes.
}
\examples{
library(RWeka)
set.seed(42)
data <- gen.data.single.xor()
set.seed(42)
res   <- goldeneye(data = data, classifier = classifier.single.xor, return.model = TRUE, return.data = TRUE)
res.i <- calculate.importance(res)

}


\name{maketree.prune.i}
\alias{maketree.prune.i}
\title{Randomize the ith attribute from X regardless of class label. This
is used to test the effect the pruning of a singleton has on the
fidelity.}
\usage{
maketree.prune.i(X, i, class.old, class.new = 0)
}
\arguments{
  \item{X}{A grouping with singletons.}

  \item{i}{The ith attribute of X to be pruned}

  \item{class.old}{The old class index of the singleton.}

  \item{class.new}{The new class index (typically 0 to
  prune the singleton)}
}
\value{
A grouping with the ith singleton pruned.
}
\description{
Randomize the ith attribute from X regardless of class
label. This is used to test the effect the pruning of a
singleton has on the fidelity.
}
\keyword{internal}


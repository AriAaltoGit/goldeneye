\name{gen.data.double.xor}
\alias{gen.data.double.xor}
\title{Generate random binary data (Double-XOR)}
\usage{
gen.data.double.xor(nrows = 1000, ncols = 7, noiselevel = 0.1,
  noise = TRUE)
}
\arguments{
  \item{nrows}{Number of rows in the data}

  \item{ncols}{Number of columns in the data}

  \item{noiselevel}{Fraction of class labels to randomly
  flip.}

  \item{noise}{Add noise or not (Boolean).}
}
\value{
Data matrix where Class = XOR(1, 2) | XOR(3, 4)
}
\description{
Generate random binary data (Double-XOR)
}


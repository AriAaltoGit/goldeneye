\name{readdataset}
\alias{readdataset}
\title{Read UCI data set stored in arff format and transform all columns
to factors.}
\usage{
readdataset(dataset, datapath, factorize = TRUE, na.omit = TRUE)
}
\arguments{
  \item{dataset}{The name of the dataset without file
  suffix.}

  \item{datapath}{The path to the directory containing the
  datasets, with trailing slash.}

  \item{factorize}{Should the columns be transformed to
  factors. Default is TRUE}

  \item{na.omit}{Omit NAs. Default is TRUE.}
}
\value{
The classification accuracy
}
\description{
Read UCI data set stored in arff format and transform all
columns to factors.
}


#' Basic helper function to generate random data.
#'
#' @param nrows Number of rows in the data 
#' @param ncols Number of columns in the data
#' 
#' @return A binary matrix with random data.
#' 
#' @keywords internal
gen.data.basic <- function(nrows = 1000, ncols = 4) {
    data <- matrix(sample(c(0, 1), nrows * ncols, replace = TRUE), nrow = nrows, ncol = ncols)
    data
}


#' Flip the class label in noiselevel fraction of the data
#'
#' @param classvector A vector giving the class of the data
#' @param noiselevel Fraction of class labels to randomly flip.
#' @return Class vector with randomly noiselevel amount of random noise.
#' 
#' @keywords internal
add.data.noise <- function(classvector, noiselevel) {
    ind <- sample(1:length(classvector), size = floor(noiselevel * length(classvector)), replace = FALSE)
    classvector[ind] <- as.numeric(xor(1, classvector[ind]))
    classvector
}


#' Generate random binary data (Single-XOR)
#'
#' @param nrows Number of rows in the data 
#' @param ncols Number of columns in the data
#' @param noiselevel Fraction of class labels to randomly flip.
#' @param noise Add noise or not (Boolean).
#' @return Data matrix where Class = XOR(1, 2) | 3
#'
#' @export
gen.data.single.xor <- function(nrows = 1000, ncols = 5, noiselevel = 0.1, noise = TRUE) {
    data <- gen.data.basic(nrows = nrows, ncols = ncols)

    ## Link columns
    data[,5] <- xor(data[,1], data[,2]) | data[,3]

    data[,4] <- rep(c(1,0), ceiling(nrows / 2))[1:nrows]
    
    ## Add noise: randomly flip the class bit
    if (noise)
        data[,5] <- add.data.noise(data[,5], noiselevel = noiselevel)

    data           <- data.frame(data)
    colnames(data) <- c(sapply(1:(ncols - 1), function(i) paste("A", i, sep = "")), "Class")
    data$Class     <- factor(data$Class)

    data
}


#' Generate random binary data (Double-XOR)
#'
#' @param nrows Number of rows in the data 
#' @param ncols Number of columns in the data
#' @param noiselevel Fraction of class labels to randomly flip.
#' @param noise Add noise or not (Boolean).
#' @return Data matrix where Class = XOR(1, 2) | XOR(3, 4)
#'
#' @export
gen.data.double.xor <- function(nrows = 1000, ncols = 7, noiselevel = 0.1, noise = TRUE) {
    ncols <- max(7, ncols)

    data <- gen.data.basic(nrows = nrows, ncols = ncols)

    X <- as.numeric(xor(data[,1], data[,2]))
    Y <- as.numeric(xor(data[,3], data[,4]))

    ## Set the class variable
    data[, 7]      <- X | Y

    ## Add noise: randomly flip the class bit
    if (noise)
        data[, 7]      <- add.data.noise(data[,7], noiselevel = noiselevel)

    data           <- data.frame(data)
    colnames(data) <- c(sapply(1:(ncols - 1), function(i) paste("A", i, sep = "")), "Class")
    data$Class     <- factor(data$Class)

    data
}


#' Exact classifier for the Single-XOR data
#'
#' @param formula Model formula
#' @param data The data
#'
#' @export
classifier.single.xor <- function(formula, data) {
    z         <- list()
    z$formula <- formula
    z$data    <- data
    class(z)  <- "classifier.single.xor"
    z
}


#' Exact classifier for the Double-XOR data
#'
#' @param formula Model formula
#' @param data The data
#' 
#' @export
classifier.double.xor <- function(formula, data) {
    z         <- list()
    z$formula <- formula
    z$data    <- data
    class(z)  <- "classifier.double.xor"
    z
}


#' Prediction function for the Single-XOR
#'
#' @param object Object
#' @param newdata The data to predict
#' @param ... Additional arguments
#'
#' @export
predict.classifier.single.xor <- function(object, newdata, ...) {
    as.numeric(xor(newdata[,1], newdata[,2]) | newdata[,3])
}


#' Prediction function for the Double-XOR data
#'
#' @param object Object
#' @param newdata The data to predict
#' @param ... Additional arguments
#'
#' @export
predict.classifier.double.xor <- function(object, newdata, ...) {
    as.numeric(xor(newdata[,1], newdata[,2]) | xor(newdata[,3], newdata[,4]))
}

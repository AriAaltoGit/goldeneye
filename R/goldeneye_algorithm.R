#' An iterative algorithm to the find the optimal grouping of attributes
#' for a dataset.
#'
#' This function iteratively finds the optimal grouping of attributes for a dataset
#' using a given classifier.
#'
#' @param data The dataset.
#' @param model A model created from the data using a classifier.
#' @param delta The sensitivity parameter. Defaults to 1 / sqrt(nrow(data)).
#' @param class.name The name of the column in the dataset with the predicted classes. Optional. Default is \code{PClass}.
#' @param class.name.2 The name of the column in the dataset with the real classes. Optional. Default is \code{Class}.
#' @param goodness.function The function used to investigate the effect of randomizing attributes in the dataset. Optional. Default is \code{fidelity}.
#' @param return.data Should the inflated dataset used in the grouping process be returned. Default is false.
#'
#' @return A list containing the following items:
#' \describe{
#' \item{S}{The optimal grouping}
#' \item{bayes.goodness}{The goodness (e.g., fidelity) when calculated using a naive Bayes (all-singleton) grouping.}
#' \item{delta}{The value of delta used.}
#' \item{data.inflated}{The dataset, created by sampling with replacement from the testing data, inflated based on the value of delta. Returned if return.data is TRUE.}
#' }
#'
#' @examples
#' data <- gen.data.single.xor()
#' train <- testsplit(data)
#' model <- classifier.single.xor(Class~., data = data[train, -ncol(data)])
#' data[,"PClass"]  <- predict(model, newdata = data)
#' res <- grouping(data = data, model = model)
#'
#' @export
grouping <- function(data, model, delta = (1 / sqrt(nrow(data))), class.name = "PClass", class.name.2 = "Class", goodness.function = fidelity, return.data = FALSE) {
    ## Determine the index of the column class.name
    ## and the indices of the attribute columns
    class.index <- which(names(data) == class.name)
    attrs       <- setdiff(seq.int(ncol(data)), which(names(data) %in% c(class.name, class.name.2)))

    ## Compute Naive Bayes fidelity using an inflated dataset, to ensure precision
    p     <- 0.5
    sigma <- delta / 5
    nr    <- ceiling((p * (1 - p)) / (sigma^2))
    nbfid <- goodness.function(model = model, tree = lapply(attrs, function(i) c(class.index, i)), data = data[sample(seq.int(nrow(data)), nr, replace = TRUE), ], class = class.name)

    ## Inflate the dataset to ensure that the desired variance level can be reached
    p    <- max(0.5, nbfid)
    nr   <- max(1000, ceiling((p * (1 - p)) / (sigma^2)))
    data <- data[sample(seq.int(nrow(data)), nr, replace = TRUE), ]

    ## --------------------------------------------------

    S     <- list()          # accumulated attribute groups
    R     <- as.list(attrs)  # groups to test for
    A     <- list()          # removed attributes
    Delta <- nbfid + delta   # grouping threshold

    ## --------------------------------------------------
    ## group the attributes
    ## --------------------------------------------------
    while ((length(R) > 0) | (length(A) > 0)) {
        if ((length(A) == 0) & (goodness.function(model = model, data = data, class = class.name, tree = maketree(R, S, class.index)) < Delta)) {
            ## Already below Delta before removing any attributes,
            ## so assign remaining attributes to singleton groups
            ##  S <- c(lapply(S, function(i) c(class.index, i)), singletonize(R, class.index))
            S <- c(lapply(S, function(i) c(NULL, i)), singletonize(R, NULL))
            R <- list()
            A <- list()
        } else {
            ## Find the attribute j removal of which from R decreases the fidelity the least
            fids <- sapply(seq.int(length(R)),
                           function(i)
                           goodness.function(model = model,
                                             data  = data,
                                             class = class.name,
                                             tree  = maketree.attr.i(R, c(A, S), i, class.index)))
            idx <- which.max(fids)
            if ((length(R) == 1) | (fids[idx] < Delta)) {
                ## If the fidelity drops below Delta add the group of attributes to the result
                ## and look for the next group of attributes
                S <- c(S, list(unlist(R)))
                R <- A
                A <- list()
            } else {
                ## If the fidelity stays above Delta, then continue by removing the grouping R
                A <- c(A, R[idx])
                R <- setdiff(R, R[idx])
            }
        }
    }

    S <- lapply(S, function(i) c(class.index, i))
    
    ## Return final grouping
    out <- list("S"              = S,
                "bayes.goodness" = nbfid,
                "delta"          = delta)

    if (return.data)
        out <- c(out, list("data.inflated" = data))

    out
}


#' An iterative algorithm to prune singletons.
#'
#' This function iteratively prunes those singletons from a given grouping
#' that do not affect the fidelity more than the given sensitivity.
#'
#' @param data The dataset.
#' @param model A model created from the data using a classifier.
#' @param delta The sensitivity parameter. Defaults to 1 / sqrn(nrow(data)).
#' @param grouping A grouping parametrizing the data.
#' @param class.name The name of the column in the dataset with the predicted classes.
#' @param goodness.function The function used to investigate the effect of randomizing attributes in the dataset. Optional. Default is \code{fidelity}.
#'
#' @return A list containing the optimal grouping, with singletons pruned.
#'
#' @examples
#' data <- gen.data.single.xor()
#' train <- testsplit(data)
#' model <- classifier.single.xor(Class~., data = data[train, -ncol(data)])
#' data[,"PClass"]  <- predict(model, newdata = data)
#' res <- grouping(data = data, model = model)
#' S.pruned <- prune.singletons(data = data, model = model, grouping = res$S)
#'
#' @export
prune.singletons <- function(data, model, delta = (1 / sqrt(nrow(data))), grouping = NULL, class.name = "PClass", goodness.function = fidelity) {
    ## Sanity check
    if (is.null(grouping))
        stop("No grouping provided!")

    ## Set shorthand for the grouping
    S <- grouping

    ## Index of the column with the predicted class
    class.index <- which(names(data) == class.name)

    ## Original fidelity
    Delta <- goodness.function(model = model, data = data, class = class.name, tree = S) - delta

    ## Singleton attributes
    ind.singleton <- which(sapply(S, function(i) length(i)) == 2)

    ## Prune singletons
    while (length(ind.singleton) > 0) {
        ## Find the singleton X removal of which from S decreases the fidelity the least
        fid <- sapply(seq.int(length(ind.singleton)),
                      function(i)
                          goodness.function(model = model,
                                            data  = data,
                                            class = class.name,
                                            tree  = maketree.prune.i(S, ind.singleton[i], class.old = class.index, class.new = 0)))
        idx <- which.max(fid)

        if (fid[idx] >= Delta) {
            ## Prune the singleton and continue
            S <- maketree.prune.i(S, ind.singleton[idx], class.old = class.index, class.new = 0)
            ind.singleton <- setdiff(ind.singleton, ind.singleton[idx])
        } else {
            ## Pruning the singleton would reduce the fidelity too much
            ind.singleton <- c()
        }
    }
    ## Return final grouping
    S
}


#' An iterative algorithm to the find the optimal grouping of attributes
#' for a dataset, also pruning singletons.
#'
#' This function iteratively finds the optimal grouping of attributes for a dataset
#' using a given classifier, after which singletons are pruned.
#'
#' @param data The dataset.
#' @param model A model created from the data using a classifier.
#' @param classifier A classifier (e.g. \code{svm}) to use
#' @param delta The sensitivity parameter. Defaults to 1 / sqrn(nrow(data)).
#' @param real.class.name The name of the column in the dataset with the true classes.
#' @param goodness.function The function used to investigate the effect of randomizing attributes in the dataset. Optional. Default is \code{fidelity}.
#' @param predict.function The function used to predict data using the classifier model. Optional. Default is the generic \code{predict} method with no additional arguments except \code{model} and \code{newdata}. Pass a custom \code{predict.function} if you, e.g., need to set some additional arguments to the \code{predict} function. A custom \code{predict.function} may only take the arguments \code{model} and \code{newdata}.
#' @param pred.class.name The name of the column in the dataset with the predicted classes. Optional. Default is \code{PClass}. If a column named \code{PClass} is not found in the dataset, it is automatically created.
#' @param return.model Should the model created from a classifier be returned. Default is FALSE.
#' @param return.data Should the data with predictions from the classifier be returned. Default is FALSE.
#'
#' @return A list containing the following items:
#' \describe{
#' \item{S}{The optimal grouping}
#' \item{S.goodness}{The goodness (e.g, fidelity) for the optimal grouping}
#' \item{S.pruned}{The optimal grouping with singletons pruned}
#' \item{S.goodness.pruned}{The goodness (e.g., fidelity) for the optimal grouping with singletons pruned}
#' \item{bayes.goodness}{The  goodness (e.g., fidelity) when calculated using a naive Bayes (all-singleton) grouping.}
#' \item{acc.original}{The original classification accuracy.}
#' \item{acc.final}{The classification accuracy using the final grouping, with singletons pruned.}
#' \item{delta}{The value of delta used.}
#' }
#' In addition, depending on the input arguments, the following fields may also be returned:
#' \describe{
#' \item{model}{The classifier model. Returned if return.model is TRUE.}
#' \item{data}{The testing data separated from the input data. Returned if return.data is TRUE.}
#' \item{data.inflated}{The dataset, created by sampling with replacement from the testing data, inflated based on the value of delta. Returned if return.data is TRUE.}
#' }
#'
#' @examples
#' ## Example 1
#' library(RWeka)
#' set.seed(42)
#' data <- gen.data.single.xor()
#' set.seed(42)
#' goldeneye(data = data, classifier = classifier.single.xor)
#'
#' ## Example 2
#' ## C4.5 Decision tree
#' set.seed(42)
#' goldeneye(data = data, classifier = J48)
#'
#' ## Example 3
#' ## SVM with linear kernel
#' goldeneye(data = data, classifier = SMO)
#'
#' ## Example 4
#' ## Using classifiers with custom parameters is possible by first creating the model
#' ## SVM with radial kernel
#' set.seed(42)
#' train <- testsplit(data)
#' set.seed(42)
#' model <- SMO(Class~., data[train, -(ncol(data))], control =
#'           Weka_control(K = "weka.classifiers.functions.supportVector.RBFKernel -G 3"))
#' data[,"PClass"] <- predict(model, newdata = data)
#' goldeneye(data = data[-train,], model = model)
#'
#' ## Example 5
#' ## Using a custom goodness function and the randomForest classifier from the randomForest package.
#' library(randomForest)
#' res <- goldeneye(data = data, classifier = randomForest, goodness.function = class_probability_ranking, predict.function = predict_randomforest_probability)
#' 
#' @export
goldeneye <- function(data, model = NULL, classifier = NULL, delta = (1 / sqrt(nrow(data))), real.class.name = "Class", pred.class.name = "PClass", goodness.function = fidelity, predict.function = predict, return.model = FALSE, return.data = FALSE) {
    if (is.null(model) & is.null(classifier)) {
        stop("No model or classifier provided! Either must be provided.")
    }

    ## Construct a model from the given classifier
    if (is.null(model) & ! is.null(classifier)) {
        cat("Creating classifier model.\n")

        ## Check that the real class name and the predicted class are found.
        ## Predicted class name is created if the default is null.
        if (! real.class.name %in% names(data))
            stop("Real class name not found in the dataset")

        if ((pred.class.name == "PClass") & ! (pred.class.name %in% names(data)))
            data$PClass <- NA
        
        if (! (pred.class.name %in% names(data)))
            stop(paste("Predicted class '", pred.class.name, "' not found in the data.", sep = ""))

        if (! is.factor(data[real.class.name]))
            data[[real.class.name]] <- as.factor(data[[real.class.name]])

        ## if (! is.factor(data[pred.class.name]))
        ##     data[[pred.class.name]] <- factor(data[[pred.class.name]], levels = levels(data[[real.class.name]]))

        ## Index of the column with the predicted class
        real.class.index <- which(names(data) == real.class.name)
        pred.class.index <- which(names(data) == pred.class.name)

        ## Split data into training and testing
        train <- testsplit(data, stratify = real.class.name)

        ## Create model for the data
        model <- eval(parse(text = paste("classifier(", real.class.name, "~., data = data[train, -pred.class.index])", sep = "")))

        ## Add the original accuracy to the dataset
        data[, pred.class.name]  <- predict.function(model, newdata = data[, -pred.class.index])

        ## Remove the training set from the data
        data <- data[-train, ]

    }

    ## Find the optimal grouping of the dataset
    res        <- grouping(data = data, model = model, class.name = pred.class.name, class.name.2 = real.class.name, delta = delta, goodness.function = goodness.function, return.data = TRUE)
    S.goodness <- goodness.function(model = model, data = res$data.inflated, tree = res$S, class = pred.class.name)

    ## Prune singletons
    S.pruned          <- prune.singletons(data = res$data.inflated, model = model, grouping = res$S, class.name = pred.class.name, goodness.function = goodness.function)
    S.goodness.pruned <- goodness.function(model = model, data = res$data.inflated, tree = S.pruned, class = pred.class.name)

    ## Original accuracy and final accuracy
    acc.original <- sampleaccuracy(model = model, data = res$data.inflated, tree = list(), class = real.class.name)
    acc.final    <- sampleaccuracy(model = model, data = res$data.inflated, tree = S.pruned, class = real.class.name)

    ## Return results
    out <- list("S"                 = res$S,
                "S.goodness"        = S.goodness,
                "S.pruned"          = S.pruned,
                "S.goodness.pruned" = S.goodness.pruned,
                "bayes.goodness"    = res$bayes.goodness,
                "acc.original"      = acc.original,
                "acc.final"         = acc.final,
                "delta"             = res$delta)

    if (return.model)
        out <- c(out, "model" = list(model))

    if (return.data)
        out <- c(out, "data" = list(data), "data.inflated" = list(res$data.inflated))

    out
}

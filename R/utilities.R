#' Produces a permutation of integers 1:length(vec) such that the
#' permutation leaves vec unchanged, i.e., the permutations are within
#' class labes defined by vec.
#'
#' @param vec Vector defining the permuations
#' @param replace Should the sampling be with or without
#'                replacement. Default is FALSE.
#' @return A permutation.
#'
#' @keywords internal
cpermute <- function(vec,replace=FALSE) {
    res <- integer(length(vec))
    for(idx in tapply(1:length(vec),vec,function(x) x)) {
        res[idx] <- if(length(idx)>1) sample(idx,replace=replace) else idx
    }
    res
}


#' Permute the data matrix using the permutation tree.
#'
#' @param tree The permutation tree
#' @param data the data matrix
#' @return A permutation of the indices of the rows in the data matrix
#'
#' @keywords internal
permutation <- function(tree,data) {
    res <- matrix(1:dim(data)[1],dim(data)[1],dim(data)[2])
    for(vec in tree) {
        if(vec[1]==0) {
            res[,vec[-1]] <- sample.int(dim(res)[1])
        }
        else {
            res[,vec[-1]] <- cpermute(data[,vec[1]])[res[,vec[1]]]
        }
    }
    res
}


#' Use the permutation to obtain a new data matrix.
#'
#' @param tree The permutation tree
#' @param perm The permutation
#' @return The permuted data matrix
#'
#' @keywords internal
permutedata <- function(data,perm) {
    for(idx in 1:dim(data)[2]) {
        data[,idx] <- data[perm[,idx],idx]
    }
    data
}


#' Compute classification accuracy.
#'
#' @param estimate The estimated class
#' @param class The true class
#' @return The classification accuracy
#'
#' @keywords internal
accuracy <- function(estimate,class) {
    sum(estimate==class)/length(class)
}


#' Obtain a sample of classification accuracy of the model when the
#' data has been permuted as parametrized by tree. Notice pred makes
#' sense only if the true class variable has not been permuted!
#'
#' @param model A model of the data from a classifier
#' @param tree A permutation tree
#' @param data The data matrix
#' @param class The name of the column with the class label
#' @return The classification accuracy
#'
#' @export
sampleaccuracy <- function(model,tree,data,class="Class") {
    newdata <- permutedata(data,permutation(tree,data))
    newpred <- predict(model,newdata=newdata)
    accuracy(newpred,newdata[,class])
}


#' Obtain a sample of the fidelity when the data the data has been
#' permuted as parametrized by the tree. Notice that pred makes sense
#' only if the true class variable has not been permuted!
#'
#' @param model A model of the data from a classifier
#' @param tree A permutation tree
#' @param data The data matrix
#' @param class The name of the column with the predicted class label
#' @return The classification accuracy
#'
#' @export
fidelity <- function(model,tree,data,class="PClass") {
    sampleaccuracy(model, tree, data, class = class)
}


#' Compute the accuracies of the permutation tree.
#'
#' @param model A model of the data from a classifier
#' @param tree A permutation tree
#' @param data The data matrix
#' @param R The number of samples to use. Default is 99.
#' @param class The name of the column with the class label
#' @return The classification accuracy
#'
#' @keywords internal
sampleaccuracies <- function(model, tree, data, R=99, class="Class") {
    replicate(R,sampleaccuracy(model,tree,data,class=class))
}


#' Read UCI data set stored in arff format and transform all columns
#' to factors.
#'
#' @param dataset The name of the dataset without file suffix.
#' @param datapath The path to the directory containing the datasets,
#'                 with trailing slash.
#' @param factorize Should the columns be transformed to
#'                  factors. Default is TRUE
#' @param na.omit Omit NAs. Default is TRUE.
#' @return The classification accuracy
#'
#' @export
readdataset <- function(dataset,datapath,factorize=TRUE, na.omit = TRUE) {
    data <- read.arff(paste(datapath, dataset, '.arff', sep = ""))

    if (na.omit)
        data <- na.omit(data)

    colnames(data)[dim(data)[2]] <- "Class"
    if(factorize) {
        for(i in colnames(data)) {
            data[,i] <- factor(data[,i])
        }
    }
    names(data) <- gsub("-", "_", names(data))
    names(data) <- gsub(" ", "_", names(data))
    names(data) <- gsub("[?]", "", names(data))
    names(data) <- gsub("/", "", names(data))
    cbind(data,PClass=NA)
}


#' Split data set into training and validation sets, stratifying by
#' groups. Of each class of n items, floor(fraction*n) go to training
#' set and ceiling((1-fraction)*n) go to validation set.
#'
#' @param data The data matrix
#' @param fraction The fraction of data to place in the training and
#'                 testing set.
#' @param size The amount of data to place in the training and testing
#'             set
#' @param replace Should the splitting be with replacement. Default is
#'                 FALSE.
#' @param stratify Variable according to which stratification should
#'                 be performed.
#' @return Indices of the training set.
#'
#' @export
testsplit <- function(data,fraction=.5,size=NULL,replace=FALSE, stratify="Class") {
    if(!is.null(size)) {
        fraction <- size/dim(data)[1]
    }
    if(!is.null(stratify)) {
        res <- c()
        for(idx in tapply(1:dim(data)[1],data[,stratify],function(x) x)) {
            res <- c(res,sample(idx,
                                size=ceiling(fraction*length(idx)),
                                replace=replace))
        }
    }
    else {
        res <- sample.int(dim(data)[1],
                          size=ceiling(fraction*dim(data)[1]),
                          replace=replace)
    }
    res
}


#' Create singletons of all attributes in the grouping X
#'
#' @param X The grouping
#' @param class.index The index of the class attribute column in the
#'                     data matrix that the grouping applies to.
#' @return A grouping with all-singleton attributes.
#'
#' @keywords internal
singletonize <- function(X, class.index) {
    if (length(X) > 0)
        lapply(sort(unlist(X)), function(i) c(class.index, i))
    else
        c()
}


#' Create a grouping from two lists of attributes. The attributes
#' in R are permuted within class as one group, whereas all attributes
#' in S are singletonized.
#'
#' @param R Unpartitioned groups, e.g. list(c(9, 1, 2, 4, 5, 7)
#' @param S Accumulated attribute groups, e.g. list(c(3, 6), c(8))
#' @param class.index The index of the class attribute column in the
#'                     data matrix that the grouping applies to.
#' @return A new grouping
#'
#' @keywords internal
maketree <- function(R, S, class.index) {
    sl <- singletonize(S, class.index)
    c(list(c(class.index, unlist(R))), sl)
}


#' Create a grouping from two lists of attributes, but the ith
#' attribute from R a singleton attribute. This is used to test the
#' effect on the fidelity that singletonizing the ith attribute of R
#' has.
#'
#' @param R Unpartitioned groups, e.g. list(c(9, 1, 2, 4, 5, 7)
#' @param S Accumulated attribute groups, e.g. list(c(3, 6), c(8))
#' @param i The ith attribute will be removed from R and made a
#'          singleton.
#' @param class.index The index of the class attribute column in the
#'                     data matrix that the grouping applies to.
#' @return A new grouping
#'
#' @keywords internal
maketree.attr.i <- function(R, S, i, class.index) {
    R <- unlist(R)
    if (length(S) > 0)
        c(list(c(class.index, R[-i]),
               c(class.index, R[i])),
          singletonize(S, class.index))
    else
        c(list(c(class.index, R[-i]),
               c(class.index, R[i])))
}


#' Randomize the ith attribute from X regardless of class label. This
#' is used to test the effect the pruning of a singleton has on the
#' fidelity.
#'
#' @param X A grouping with singletons.
#' @param i The ith attribute of X to be pruned
#' @param class.old The old class index of the singleton.
#' @param class.new The new class index (typically 0 to prune the
#'                  singleton)
#' @return A grouping with the ith singleton pruned.
#'
#' @keywords internal
maketree.prune.i <- function(X, i, class.old, class.new = 0) {
    X[[i]][which(X[[i]] == class.old)] <- class.new
    X
}


#' Determine the relative importance of each group in the grouping
#'
#' @param model A model of the data from a classifier
#' @param data The data matrix
#' @param class.name The name of the column with the class label
#' @param class.index The name of the column with the class label
#' @param tree A permutation tree
#'
#' @return A vector with the relative fidelity of each group.
#'
#' @keywords internal
group.importance <- function(model = model, data = data, class.name = class.name, class.index = class.index, tree = tree) {

    sapply(1:length(tree),
           function(i)
           sampleaccuracy(model = model,
                          tree  = maketree(tree, i, class.index),
                          data  = data,
                          class = class.name))
}

#' Determine the relative importance of each attribute in the dataset
#' using the method of Breiman et al.
#'
#' @param model A model of the data from a classifier
#' @param data The data matrix
#' @param class.name The name of the column with the class label
#' @param class.index The name of the column with the class label
#' @param tree A permutation tree
#'
#' @return A vector with the relative fidelity of each attribute in
#' the dataset.
#'
#' @keywords internal
attribute.importance <- function(model = model, data = data, class.name = class.name, class.index = class.index, tree = tree) {
    all.attrs <- sort(setdiff(unlist(tree), c(0, class.index)))
    sapply(1:length(all.attrs),
           function(i)
           sampleaccuracy(model = model,
                          tree  = list(c(0, all.attrs[i]), c(class.index, all.attrs[-i])),
                          data  = data,
                          class = class.name))
}


#' Calculate the importance of attribute groups and individual attributes.
#'
#' @param res The output from GoldenEye using the arguments return.model = TRUE and return.data = TRUE.
#' @param pred.class.name The name of the column with the predicted class label.
#' @param pred.class.index The index of the column with the predicted class label.
#' 
#' @return The input structure res with the following fields added:
#' \describe{
#' \item{group.scores}{The fidelity of each group in the optimal grouping. A lower fidelity score implies a more important group. Returned if calculate.importance is TRUE.}
#' \item{attribute.scores}{The fidelity score for each individual attribute in the dataset. A lower fidelity score implies a more important attribute. Returned if calculate.importance is TRUE.}
#' \item{attribute.ranks}{The rank order of the attribute scors. A lower rank implies a more important attribute. Returned if calculate.importance is TRUE.}
#' }
#' 
#' @examples
#' library(RWeka)
#' set.seed(42)
#' data <- gen.data.single.xor()
#' set.seed(42)
#' res   <- goldeneye(data = data, classifier = classifier.single.xor, return.model = TRUE, return.data = TRUE)
#' res.i <- calculate.importance(res)
#' 
#' @export
calculate.importance <- function(res, pred.class.name = "PClass", pred.class.index = NULL) {
    ## Set pred.class.index
    if (is.null(pred.class.index))
        pred.class.index <- which(names(res$data.inflated) == pred.class.name)

    ## Importance of groupings and individual attributes
    ## Calculate the relative importance of each group
    imp.g <- group.importance(model       = res$model,
                              data        = res$data.inflated,
                              class.name  = pred.class.name,
                              class.index = pred.class.index,
                              tree        = res$S.pruned)


    ## The most important attribute in the breiman method
    ## is the attribute for which the score is lowest, i.e.
    ## randomizing it causes the greatest drop in fidelity.
    imp.a <- attribute.importance(model       = res$model,
                                  data        = res$data.inflated,
                                  class.name  = pred.class.name,
                                  class.index = pred.class.index,
                                  tree        = res$S.pruned)

    ## Return result
    c(res, 
    list("group.scores"     = imp.g,
         "attribute.scores" = imp.a,
         "attribute.ranks"  = order(imp.a, decreasing = FALSE)))
}

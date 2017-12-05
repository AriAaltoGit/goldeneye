# 0.2.1 (2017-12-05)
* Fixed bug in the `GoldenEye++` variant of the algorithm, i.e., when a custom goodness function is used. The data was not permuted within-class as it should be.
* In the `goldeneye`-function, the argument `predict.function` has been removed. The custom prediction using class probabilities instead of class labels is now instead incorporated directly into the custom goodness functions `class_probability_ranking_randomforest` and `class_probability_correlation_randomforest`.
* Renamed the function `class_probability_ranking` to `class_probability_ranking_randomforest`.
* Renamed the function `class_probability_ranking_pearson` to `class_probability_correlation_randomforest`.
* Added README file.
* Added CITATION file.

# 0.2.0 (2017-01-31)
* Fixed bug in the grouping algorithm.

# 0.1.3 (2014-11-19)
* Fixed bug in which the function used to prune singletons always used fidelity instead of the supplied goodness function.
* Fixed wrong correlation type in the function `class_probability_ranking`. The correct correlation is Spearman.

# 0.1.3 (2014-11-07)
* The class column in the input dataset automatically made into a factor.
* Added support for generic goodness functions, to be used instead of fidelity.
* Added support for custom predict functions, to support more advanced usage.

# 0.1.2 (2014-11-05)
* Predicted class automatically added to the data frame if not present.
* Improved check of input arguments.
* Fixed a bug that prevented use of the `svm` classifier from the `e1071` package with `GoldenEye`.
* Updated citation information.

# 0.1.0 (2014-04-25)
* First release.

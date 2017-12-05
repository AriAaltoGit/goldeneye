# goldeneye

## Overview
The `goldeneye` package provides an R-implementation of the `GoldenEye` algorithm described in _Henelius, Puolamäki, Boström, Asker and Papapetrou. 2014. A peek into the black box: exploring classifiers by randomization_, [available here](https://doi.org/10.1007/s10618-014-0368-8). 

The package also implements the `GoldenEye++` algorithm described in the paper _Henelius, Puolamäki, Karlsson, Zhao, Asker, Boström and Papapetrou. 2015. GoldenEye++: A Closer Look into the Black Box_, [available here](https://doi.org/10.1007/978-3-319-17091-6_5). Please note that a bug was found in the `GoldenEye++` variant impacting the groupings reported in the paper; refer to the erratum below and to the [changelog](changelog.md).

Using the `goldeneye` method it is possible to uncover which attributes in a dataset that a classifier uses jointly for making predictions. A *dataset* is of the form

item | A1 | A2 | A3 | A4 | Class
-----|----|----|----|----|-------
1    | 1  | 1  | 1  | 1  |  1
2    | 1  | 0  | 0  | 0  |  1
3    | 0  | 1  | 1  | 1  |  0
4    | 1  | 1  | 1  | 0  |  1
5    | 1  | 0  | 1  | 1  |  1
6    | 1  | 0  | 1  | 0  |  1

where each row of the dataset is a data item of different attrbutes *A1*, ..., *A4* (in this example) and is associated with a *class label*. Given such a dataset and a classifier (e.g., the *randomForest* classifier or a *support vector machine (SVM)*), the `goldeneye` algorithm finds a grouping of attributes of the form *((A1, A2), (A3))*. In this case, the grouping means that attributes *A1* and *A2* are used jointly by the classifier in making predictions, attribute *A3* is not used in conjunction with other attributes, and attribute *A4* is uninformative for the classification task and is not part of the grouping.

In R, the grouping of attributes is done using the `goldeneye` function in the package, see the examples below.


## Citing
To get a BibTex entry in R type `citation("goldeneye")` when the package is installed.


## Installation
The development version of the `goldeneye` package can be installed as follows.

First install the `devtools`-package and load it:
```
install.packages("devtools")
library(devtools)
```

after which the `goldeneye` package cam be installed:
```
install_bitbucket("aheneliu/goldeneye")
```

## Usage
### Example 1
Investigate which attributes the `randomForest` classifier is using in a synthetic dataset.

```R
## Load libraries
library(goldeneye)
library(randomForest)

## Generate synthetic data
set.seed(42)
data <- gen.data.single.xor()

## Find the attribute grouping
set.seed(42)
res <- goldeneye(data = data, classifier = randomForest)

## View the results
> str(res)
List of 8
 $ S                :List of 3
  ..$ : int [1:3] 6 1 2
  ..$ : int [1:2] 6 3
  ..$ : int [1:2] 6 4
 $ S.goodness       : num 0.903
 $ S.pruned         :List of 3
  ..$ : int [1:3] 6 1 2
  ..$ : int [1:2] 6 3
  ..$ : num [1:2] 0 4
 $ S.goodness.pruned: num 0.905
 $ bayes.goodness   : num 0.755
 $ acc.original     : num 0.915
 $ acc.final        : num 0.827
 $ delta            : num 0.0448
```
The grouping is found in `res$S` which in this case corresponds to the grouping `((A1, A2), (A3), (A4))`. The pruned grouping, in which uninformative singleton attributes are removed, is found in `res$S.pruned`. In this case it corresponds to `((A1, A2), (A3))`, i.e., attribute _A4_ is not used by the classifier.

### Example 2
Using classifiers with custom parameters is possible by first creating the model and passing it to the `goldeneye` function.
```R
# Load libraries
library(goldeneye)
library(RWeka)

# Create synthetic data
set.seed(42)
data <- gen.data.single.xor()

# Split data into training and testing
set.seed(42)
train <- testsplit(data)

# Create classifier model: SVM with radial kernel from Weka
set.seed(42)
model <- SMO(Class~., data[train, -(ncol(data))], control = Weka_control(K = "weka.classifiers.functions.supportVector.RBFKernel -G 3"))
data[,"PClass"] <- predict(model, newdata = data)

## Find the attribute grouping
res <- goldeneye(data = data[-train,], model = model)
```

### Example 3
Using a custom goodness function and the randomForest classifier from the randomForest package as in the `GoldenEye++`-algorithm.
```R
# Load libraries
library(goldeneye)
library(randomForest)

## Find the attribute grouping
res <- goldeneye(data = data, classifier = randomForest, goodness.function = class_probability_correlation_randomforest)
```

## License
The `goldeneye` R-package is licensed under the [MIT-license](http://opensource.org/licenses/MIT).

## Erratum
### 2017-12-05
Due to a bug in the code for the `GoldenEye++` algorithm the data was not permuted within-class. Please note that this affects the `GoldenEye++`-groupings reported in the [GoldenEye++ -paper](https://doi.org/10.1007/978-3-319-17091-6_5) which hence are incorrect. Please also refer to the [changelog](CHANGELOG.md).

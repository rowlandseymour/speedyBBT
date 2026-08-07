# Construct the comparison specific Bradley–Terry design matrix

Construct the comparison specific Bradley–Terry design matrix

## Usage

``` r
construct.design.matrix.by.comparison(object1, object2)
```

## Arguments

- object1:

  vector containing the labels of object1

- object2:

  vector containing the labels of object2

## Value

design matrix X

## Examples

``` r


# Generate comparisons of three items
object1 <- c(1, 3, 2, 1)
object2 <- c(3, 1, 1, 2)
X <- construct.design.matrix.by.comparison(object1, object2)
```

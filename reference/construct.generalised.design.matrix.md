# Construct the generalised Bradley–Terry design matrix

Construct the generalised Bradley–Terry design matrix

## Usage

``` r
construct.generalised.design.matrix(item1, item2, formula, data)
```

## Arguments

- item1:

  vector containing the labels of item1

- item2:

  vector containing the labels of item2

- formula:

  right-hand-side only formula for the item specific covariates

- data:

  dataframe containing the values of the item specific covariates

## Value

design matrix X

## Examples

``` r

# Generate data.frame
example.df <- data.frame(
  "a" = stats::runif(100, 0, 3),
  "b" = sample(1:2, 100, TRUE)
)

# Generate formula
example.formula <- ~ a + b + I(a^2)

# Generate comparisons of three items
item1 <- c(1, 3, 2, 1)
item2 <- c(3, 1, 1, 2)
X <- construct.generalised.design.matrix(item1, item2, example.formula, example.df)
#> Error in construct.generalised.design.matrix(item1, item2, example.formula,     example.df): could not find function "construct.generalised.design.matrix"
```

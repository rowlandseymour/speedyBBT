# Construct the generalised Bradley–Terry design matrix

Construct the generalised Bradley–Terry design matrix

## Usage

``` r
construct.generalised.design.matrix(player1, player2, formula, data)
```

## Arguments

- player1:

  vector containing the labels of player1

- player2:

  vector containing the labels of player2

- formula:

  right-hand-side only formula for the player specific covariates

- data:

  dataframe containing the values of the player specific covariates

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

# Generate comparisons of three players
player1 <- c(1, 3, 2, 1)
player2 <- c(3, 1, 1, 2)
X <- construct.generalised.design.matrix(player1, player2, example.formula, example.df)
```

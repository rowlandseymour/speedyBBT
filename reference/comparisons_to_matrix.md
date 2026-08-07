# Construct Win Matrix from Comparisons

This function constructs a win matrix from a data frame of comparisons.
It is needed for the MCMC functions.

## Usage

``` r
comparisons_to_matrix(n.objects, comparisons)
```

## Arguments

- n.objects:

  The number of areas in the study.

- comparisons:

  An N x 2 data frame, where N is the number of comparisons. Each row
  should correspond to a judgment. The first column is the winning
  object, the second column is the more losing object. The areas should
  be labeled from 1 to n.objects.

## Value

A matrix where the i, j th element is the number of times object i beat
object j.

## Examples

``` r

# Generate some sample comparisons
comparisons <- data.frame("winner" = c(1, 3, 2, 2), "loser" = c(3, 1, 1, 3))

# Create matrix from comparisons
win.matrix <- comparisons_to_matrix(3, comparisons)
```

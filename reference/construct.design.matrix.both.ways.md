# Construct the Bradley–Terry design matrix for ties

This is the design matrix for tied comparisons. Each permutation (rather
than combination) is featured, i.e. both (i, j) and (j, i).

## Usage

``` r
construct.design.matrix.both.ways(n.objects)
```

## Arguments

- n.objects:

  the number of objects

## Value

design matrix X

## Examples

``` r

# design matrix with 3 objects

X <- construct.design.matrix.both.ways(3)
```

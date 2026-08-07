# Honour Based Abuse in Oxfordshire

A comparative judgment data set for risk of honour based abuse in Oxford
and Banbury

## Usage

``` r
oxon.comparisons
```

## Format

A data frame with 1,167 comparisons.

- comparison_id:

  The ID of the comparison.

- user_id:

  The ID of the user who made the comparisons.

- item_1_id:

  The ID of the first area involved in the comparison.

- item_2_id:

  The ID of the second area involved in the comparison.

- selected_item_id:

  The ID of the selected area. If the comparison was tied, the
  `selected_item_id` is NA.

- state:

  The state of the outcome. `selected` indicates that a judgment was
  made, `skipped` indicates that the comparison was skipped, and `tied`
  indicates that the comparison was tied.

## Source

The data was collected following ethical approval the University of
Birmingham's Science, Engineering and Maths Ethics Committee.

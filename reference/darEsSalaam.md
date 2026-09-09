# Comparative Judgment on Deprivation in Dar es Salaam, Tanzania

A comparative judgment data set on deprivation in subwards in Dar es
Salaam, Tanzania.Citizens were shown pairs of subwards at random and
asked which was more deprived.If they said they were equal, one of the
pair was chosen at random to be more deprived.The data was collected in
August 2018. The sex of each judge is also included.

## Usage

``` r
darEsSalaam
```

## Format

A list with three elements.

- comparisons:

  A dataframe containing the comparisons. Each row corresponds to a
  judgement made by a single judge. This differs from the data in the
  `BBT` package as it explicitly includes ties rather than randomly
  allocating a winner. The columns are:

  outcome

  :   The outcome of the comparison. 1 if item 2 is the winner, 0 if
      item 1 is the winner, and 2 if it was a tie (although there are no
      instances of this happening).

  item1

  :   The first item in the comparison.

  item2

  :   The second item in the comparison.

  sex

  :   The sex of the judge making the comparison.

- wards:

  A dataframe containing the names and shapefiles of the subwards with
  columns:

  subwardName

  :   The name of the subward.

  geometry

  :   A list containing the shapefile of the subward.

- adjacencyMatrix:

  An adjacency matrix of the subwards formed from the shapefiles. This
  considers subwards as nodes and places edges between adjacent
  subwards. Two additional edges have been manually included to allow
  for crossings of the Kurasini creek.

## Source

This data set was collected by Madeleine Ellis, James Goulding, Bertrand
Perrat, Gavin Smith and Gregor Engelmann. We gratefully acknowledge the
Rights Lab at the University of Nottingham for supporting funding for
the comprehensive ground truth survey. We also acknowledge
HumanitarianStreet Mapping Team (HOT) for providing a team of experts in
data collection to facilitate the surveys. This work was also supported
by the EPSRC Horizon Centre for Doctoral Training - My Life in Data
(EP/L015463/1) and EPSRC grant Neodemographics (EP/L021080/1).

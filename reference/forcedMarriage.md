# Forced Marriage in Nottinghamshire

A comparative judgment data set for risk of forced marriage at ward
level in Nottinghamshire. There are 12 judges and 76 wards.

## Usage

``` r
forcedMarriage
```

## Format

A list with three elements.

- comparisons:

  A dataframe containing the comparisons. Each row corresponds to a
  judgement made by a single judge. The columns are:

  user

  :   The ID of the judge.

  time

  :   The time the comparison was made.

  win

  :   The ID of the ward that was judged to have a higher risk of forced
      marriage.

  lost

  :   The ID of the ward that was judged to have a lower risk of forced
      marriage.

- wards:

  A dataframe containing information about each of the wards. The
  columns are:

  NAME

  :   The name of the ward.

  AREA_CODE

  :   The code of the ward.

  DESCRIPTIO

  :   The electoral division of the ward.

  FILE_NAME

  :   The name of the file containing the shapefile for the ward.

- adjacencyMatrix:

  The final element is an adjacency matrix, where the wards are nodes
  and edges are placed between adjacent wards.

## Source

The data was collected using support from the Engineering and Physical
Sciences Research Council (grant reference EP/R513283/1), the Economic
and Social Sciences Research Council (ES/V015370/1) and the Research
England Policy Support Fund. The data was collected following ethical
approval from the University of Nottingham School of Politics and
International Relations ethics committee.

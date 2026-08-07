# Wimbledon Men's Singles Championship 2019

The outcomes of all 127 men's singles matches in the 2019 Wimbledon
champtionship.

## Usage

``` r
wimbledon
```

## Format

A list containing a dataframe with the outcomes of the matches and a
dataframe describing the players.

- matches:

  Each row of the matchs dataframe corresponds to a match. The columns
  are:

  winner

  :   The
      ``` id`` of the winner of the match.} \item{loser}{The  ```id\` of
      the loser of the match.

  secondWeek

  :   A boolean indicating if the match was played in the second week of
      the tournament.

  outcome

  :   The outcome of the match. 1 if the winner is item 2, 0 if the
      winner is item 1.

- players:

  Data frame describing the players. The columns are:

  name

  :   The name of the player.

  rank

  :   The rank of the player in the ATP league table.

  points

  :   The number of points received so far in the ATP 2019 tour prior to
      Wimbledon starting.

  id

  :   The ID of the player.

## Source

<http://tennis-data.co.uk/alldata.php>

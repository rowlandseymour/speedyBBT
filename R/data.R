#' Comparative Judgment on Deprivation in Dar es Salaam, Tanzania
#'
#' A comparative judgment data set on deprivation in subwards in Dar es Salaam,
#' Tanzania.Citizens were shown pairs of subwards at random and asked which
#' was more deprived.If they said they were equal, one of the pair was chosen at
#' random to be more deprived.The data was collected in August 2018.
#' The sex of each judge is also included.
#' @docType data
#'
#'
#' @format A list with three elements.
#' \describe{
#'   \item{comparisons}{A dataframe containing the comparisons. Each row corresponds
#'   to a judgement made by a single judge. This differs from the data
#'   in the `BBT` package as it explicitly includes ties rather than randomly
#'   allocating a winner. The columns are:
#'   \describe{
#'      \item{outcome}{The outcome of the comparison. 1 if item 2 is the winner, 0 if item 1 is the winner, and 2 if it was a tie (although there are no instances of this happening).}
#'      \item{item1}{The first item in the comparison.}
#'      \item{item2}{The second item in the comparison.}
#'      \item{sex}{The sex of the judge making the comparison.}
#'    }
#' }
#'   \item{wards}{A dataframe containing the names and shapefiles of the subwards with columns:
#'     \describe{
#'       \item{subwardName}{The name of the subward.}
#'       \item{geometry}{A list containing the shapefile of the subward.}
#'       \item{adjacencyMatrix}{An adjacency matrix of the subwards formed from the shapefiles.This considers subwards as nodes and places edges between adjacent subwards.
#' Two additional edges have been manually included to allow for crossings of the Kurasini creek.}
#'     }
#'   }
#' }
#'
#' @keywords datasets
#'
#' @source
#' This data set was collected by Madeleine Ellis, James Goulding, Bertrand Perrat,
#' Gavin Smith and Gregor Engelmann. We gratefully acknowledge the Rights Lab at the
#' University of Nottingham for supporting funding for the comprehensive ground truth survey.
#' We also acknowledge HumanitarianStreet Mapping Team (HOT) for providing a team of experts in
#' data collection to facilitate the surveys. This work was also supported by the EPSRC Horizon
#' Centre for Doctoral Training - My Life in Data (EP/L015463/1) and EPSRC grant
#' Neodemographics (EP/L021080/1).
#'
"darEsSalaam"


#' Wimbledon Men's Singles Championship 2019
#'
#'
#' The outcomes of all 127 men's singles matches in the 2019
#' Wimbledon champtionship.
#'
#'
#' @docType data
#'
#'
#' @format A list containing a dataframe with the  outcomes of the matches and a dataframe
#' describing the players.
#' \describe{
#'   \item{matches}{Each row of the matchs dataframe corresponds to a match. The columns are:
#'     \describe{
#'       \item{winner}{The `id`` of the winner of the match.}
#'       \item{loser}{The `id` of the loser of the match.}
#'       \item{secondWeek}{A boolean indicating if the match was played in the second week of the tournament.}
#'       \item{outcome}{The outcome of the match. 1 if the winner is item 2, 0 if the winner is item 1.}
#'   }
#' }
#' \item{players}{Data frame describing the players. The columns are:
#'   \describe{
#'     \item{name}{The name of the player.}
#'     \item{rank}{The rank of the player in the ATP league table.}
#'     \item{points}{The number of points received so far in the ATP 2019 tour prior to Wimbledon starting.}
#'     \item{id}{The ID of the player.}
#'    }
#' }
#' }
#' @keywords datasets
#'
#' @source \url{http://tennis-data.co.uk/alldata.php}
#'
"wimbledon"


#' Forced Marriage in Nottinghamshire
#'
#' A comparative judgment data set for risk of forced marriage at ward level in Nottinghamshire.
#' There are 12 judges and 76 wards.
#'
#' @docType data
#'
#' @format A list with three elements.
#' \describe{
#'   \item{comparisons}{A dataframe containing the comparisons. Each row corresponds to a judgement made by a single judge. The columns are:
#'     \describe{
#'       \item{user}{The ID of the judge.}
#'       \item{time}{The time the comparison was made.}
#'       \item{win}{The ID of the ward that was judged to have a higher risk of forced marriage.}
#'       \item{lost}{The ID of the ward that was judged to have a lower risk of forced marriage.}
#'     }
#'   }
#' \item{wards}{A dataframe containing information about each of the wards. The columns are:
#'   \describe{
#'     \item{NAME}{The name of the ward.}
#'     \item{AREA_CODE}{The code of the ward.}
#'     \item{DESCRIPTIO}{The electoral division of the ward.}
#'     \item{FILE_NAME}{The name of the file containing the shapefile for the ward.}
#'   }
#'  }
#' \item{adjacencyMatrix}{The final element is an adjacency matrix, where the wards are nodes and edges are placed between
#'  adjacent wards.}
#' }
#'
#' @keywords datasets
#'
#' @source
#' The data was collected using support from the Engineering and Physical Sciences Research
#' Council (grant reference EP/R513283/1), the Economic and Social Sciences Research Council (ES/V015370/1)
#' and the Research England Policy Support Fund. The data was collected following ethical approval
#' from the University of Nottingham School of Politics and International Relations ethics committee.
#'
#'
#'
#'
"forcedMarriage"


#' FGM in South Yorkshire
#'
#' A comparative judgment data set for risk of female genital mutilation at ward level in South Yorkshire.
#'
#' @docType data
#'
#' @format A data frame with 877 comparisons.
#' \describe{
#' \item{comparison_id}{The ID of each comparison.}
#' \item{user_id}{The ID
#' of the user who made the comparisons.}
#' \item{item_1_id}{The ID of the first area involved in the
#' comparison.}
#' \item{item_2_id}{The ID of the second area involved in the comparison.}
#' \item{selected_item_id}{The ID of the selected area. If the
#' comparison was tied, the `selected_item_id` is NA.}
#' \item{state}{The state of the outcome. `selected` indicates that a judgment was made, `skipped` indicates that the comparison was skipped, and `tied` indicates that the comparison was tied.}
#' }
#' @keywords datasets
#'
#' @source
#' The data was collected following ethical approval the University
#' of Birmingham's Science, Engineering and Maths Ethics Committee.
#'
#'
#'
"sy.comparisons"


#' Honour Based Abuse in Oxfordshire
#'
#' A comparative judgment data set for risk of honour based abuse in Oxford and Banbury
#'
#' @docType data
#'
#' @format A data frame with 1,167 comparisons.
#' \describe{
#' \item{comparison_id}{The ID of the comparison.}
#' \item{user_id}{The ID
#' of the user who made the comparisons.}
#' \item{item_1_id}{The ID of the first area involved in the
#' comparison.}
#' \item{item_2_id}{The ID of the second area involved in the comparison.}
#' \item{selected_item_id}{The ID of the selected area. If the
#' comparison was tied, the `selected_item_id` is NA.}
#' \item{state}{The state of the outcome. `selected` indicates that a judgment was made, `skipped` indicates that the comparison was skipped, and `tied` indicates that the comparison was tied.}
#' }
#' @keywords datasets
#'
#' @source
#' The data was collected following ethical approval the University
#' of Birmingham's Science, Engineering and Maths Ethics Committee.
#'
"oxon.comparisons"

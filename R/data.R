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
#' The first is a dataframe containing the comparison. Each row corresponds
#' to a judgement made by a single judge. Columns 2 and 3 contain the pair of s
#' ubwards being compared. The first column shows the outcome
#' of the comparison: 1 if player 2 won, 2 if it was a tie and 0 if player 1 won
#' (although there a no instances of this happening). This differs from the data
#' in the `BSBT` package as it explicitly includes ties rather than randomly
#' allocating a winner.
#'
#' The second is a dataframe containing the names and shapefiles of the subwards
#'
#' The third is an adjacency matrix of the subwards formed from the shapefiles.
#' This considers subwards as nodes and places edges between adjacent subwards.
#' Two additional edges have been manually included to allow for crossings of the
#' Kurasini creek.
#'
#' @keywords datasets
#'
#' @source This data set was collected by Madeleine Ellis, James Goulding, Bertrand Perrat,
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
#'The outcomes of all 127 men's singles matches in the 2019
#'Wimbledon champtionship.
#'
#'
#' @docType data
#'
#'
#' @format A list containing a dataframe with the  outcomes of the matches and a dataframe
#' describing the players. Each row of the matchs dataframe corresponds to a match. The players
#' dataframw has the name and id fo the player as weel as their rank in the ATP league table
#' and the number of points received so far in the ATP 2019 tour prior to Wimbledon starting.
#'
#'
#' @keywords datasets
#'
#' @source \url{http://tennis-data.co.uk/alldata.php}
#'
"wimbledon"



#' Forced Marriage in Nottinghamshire
#'
#'A comparative judgment data set for risk of forced marriage at ward level in Nottinghamshire.
#'There are 12 judges and 76 wards.
#'
#' @docType data
#'
#' @format A list with three elements. The first is c dataframe containing 1846 rows and 4 columns.
#'  Each row corresponds to a judgement made by a single judge. Columns 3 and 4 shows which of the
#'  pair of wards was judged to have relatively higher and low forced marriage risk level, column 1
#'  shows which judge the comparison belong to, and column 2 shows what time they made the decision.
#'
#'  The second is the a dataframe describing each ward and its geometry.
#'
#'  The final element is an adjacency matrix, where the wards are nodes and edges are placed between
#'  adjacent wards.
#'
#'  @keywords datasets
#'
#'  @source The data was collected using support from the Engineering and Physical Sciences Research
#'  Council (grant reference EP/R513283/1), the Economic and Social Sciences Research Council (ES/V015370/1)
#'  and the Research England Policy Support Fund. The data was collected following ethical approval
#'  from the University of Nottingham School of Politics and International Relations ethics committee.
#'
#'
#'
#'
"forcedMarriage"


#' FGM in South Yorkshire
#'
#'A comparative judgment data set for risk of female genital mutilation at ward level in South Yorkshire.
#'
#' @docType data
#'
#' @format A data frame with 877 comparisons. Each comparison has an ID, the ID
#' of the user who made the comparisons, the IDs of the two areas involved in the
#' comparisons, the ID of the selected area, and the state of the outcome. If the
#' comparison was tied, the ID of the selected area is NA
#'
#'  @keywords datasets
#'
#'  @source  The data was collected following ethical approval the University
#'  of Birmingham's Science, Engineering and Maths Ethics Committee.
#'
#'
#'
"sy.comparisons"



#' Honour Based Abuse in Oxfordshire
#'
#'A comparative judgment data set for risk of honour based abuse in Oxford and Banbury
#'
#' @docType data
#'
#' @format A data frame with 1,167 comparisons. Each comparison has an ID, the ID
#' of the user who made the comparisons, the IDs of the two areas involved in the
#' comparisons, the ID of the selected area, and the state of the outcome. If the
#' comparison was tied, the ID of the selected area is NA
#'
#'  @keywords datasets
#'
#'  @source  The data was collected following ethical approval the University
#'  of Birmingham's Science, Engineering and Maths Ethics Committee.
#'
#'
#'
#'
"oxon.comparisons"




#' Simulated data about morality and politics in Denmark
#'
#' This is a simulated counterpart of data presented
#' by Van Leeuwen and colleagues (2022) concerning
#' associations between moral dispositions (measured using the Morality As
#' Cooperation questionnaire, MAC; see Curry et al., 2019) and political
#' orientation.
#'
# \tabular{lll}{
#   \strong{sex} \tab \code{integer} \tab Sex of the child; 1 = boy, 2 = girl\cr
#   \strong{site} \tab \code{integer} \tab Site of the child's origin; 1 = disadvantaged inner city, 2 =
#    advantaged suburban , 3 = advantaged rural,
#   4 = disadvantaged rural, 5 = disadvantaged Spanish speaking\cr
#   \strong{setting} \tab \code{integer} \tab Setting in which the child watches Sesame Street; 1 = at
#   home, 2 = at school\cr
#   \strong{age} \tab \code{integer} \tab Age of the child in months\cr
#   \strong{viewenc} \tab \code{integer} \tab Whether or not the child is encouraged to watch Sesame
#   Street; 0 = no, 1 = yes\cr
#   \strong{peabody} \tab \code{integer} \tab Peabody mental age score of the child; the higher the
#   score the higher the mental age\cr
#   \strong{prenumb} \tab \code{integer} \tab score on a numbers test before watching Sesame Street for
#   a year\cr
#   \strong{postnumb} \tab \code{integer} \tab score on a numbers test after watching Sesame Street for
#   a year\cr
#   \strong{funumb} \tab \code{integer} \tab follow up numbers test score measured one year after
#   postnumb\cr
#   \strong{Bb} \tab \code{integer} \tab Knowledge of body parts before\cr
#   \strong{Bl} \tab \code{integer} \tab Knowledge of letters before\cr
#   \strong{Bf} \tab \code{integer} \tab Knowledge of forms before\cr
#   \strong{Bn} \tab \code{integer} \tab Knowledge of numbers before\cr
#   \strong{Br} \tab \code{integer} \tab Knowledge of relations before\cr
#   \strong{Bc} \tab \code{integer} \tab Knowledge of classifications before\cr
#   \strong{Ab} \tab \code{integer} \tab Knowledge of body parts after\cr
#   \strong{Al} \tab \code{integer} \tab Knowledge of letters after\cr
#   \strong{Af} \tab \code{integer} \tab Knowledge of forms after\cr
#   \strong{An} \tab \code{integer} \tab Knowledge of numbers after\cr
#   \strong{Ar} \tab \code{integer} \tab Knowledge of relations after\cr
#   \strong{Ac} \tab \code{integer} \tab Knowledge of classifications after
# }
#' @docType data
#' @keywords datasets
#' @name synthetic_dk
#' @usage data(synthetic_dk)
#' @references Van Leeuwen, F., Van Lissa, C. J., Papakonstantinou, T.,
#' Petersen, M., & Curry, O. S. (2022, May 25). Morality as Cooperation,
#' Politics as Conflict. <doi:10.31234/osf.io/wm6rk>
#'
#' Curry, O. S., Chesters, M. J., & Van Lissa, C. J. (2019). Mapping morality
#' with a compass: Testing the theory of ‘morality-as-cooperation’with a new
#' questionnaire. Journal of Research in Personality, 78, 106-124.
#' @format A data frame with 518 rows and 33 variables.
NULL

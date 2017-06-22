#' Zentrales Haltestellenverzeichnis
#'
#' A dataset containing all public stations in Germany. Currently the dataset only includes stations in Baden-Württemberg, Bayern, Berlin, Brandenburg and Hessen. Accordingly to \href{http://www.delfi.de/}{Delfi} the other states will follow step by step.
#'
#' @format A data frame with 356828 rows and 14 variables:
#' \describe{
#'   \item{Type}{The type of station.}
#'   \item{DHID}{The German DHID code.}
#'   \item{Parent}{The DHID code of the parent station.}
#'   \item{Name}{The name of the station.}
#'   \item{Latitude}{The latitude of the station.}
#'   \item{Longitude}{The longitude of the station.}
#'   \item{MunicipalityCode}{The code of the city also known as 'AGS'.}
#'   \item{Municipality}{The name of the city.}
#'   \item{DistrictCode}{The code of the district.}
#'   \item{District}{The name of the district.}
#'   \item{Condition}{The condition of the station.}
#'   \item{State}{The state of the station.}
#'   \item{Description}{Additional infromation.}
#'   \item{Authority}{The authority maintaining the station.}
#'   }
#' @source \url{https://zhv.wvigmbh.de/}
"zHV"
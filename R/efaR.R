#'
#'
#'
#'
#'
#'
#'

efaR <- function (origin,
                  destination,
                  auto_station = TRUE,
                  date = Sys.Date(),
                  time = Sys.time(),
                  simplify = TRUE
                  ) {
  
  if (missing(origin)) {
    stop(
      sprintf(
        "Please provide an origin."
      )
    )
  }
  
  if (missing(destination)) {
    stop(
      sprintf(
        "Please provide an destination."
      )
    )
  }
  
  
  
}
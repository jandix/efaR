#' Wrapper function for EFA XML API.
#'
#' @param origin The origin of the journey. Either a name or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'.
#' @param destination The destination of the journey. Either a name or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'.
#' @param time The date and time of the journey.
#' @param simplify Simplify determines whether the original xml result is returned or a simpler S3 object.
#'
#' @return Either the original xml result or an S3 object based on the simplify input. 
#'
#' @export

efaR <- function (origin,
                  destination,
                  time = Sys.time(),
                  simplify = TRUE
) {
  
  if (missing(origin)) {
    stop(
      sprintf(
        "Please provide an origin."
      ),
      call. = FALSE
    )
  }
  
  if (missing(destination)) {
    stop(
      sprintf(
        "Please provide an destination."
      ),
      call. = FALSE
    )
  }
  
  data("zHV")
  
  if (!stringr::str_detect(origin, "[a-z]{2}:[:digit:]{5}")) {
    coordinates <- ggmap::geocode(origin)
    zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                             cbind(zHV$Longitude, zHV$Latitude))
    zHV <- dplyr::arrange(zHV, distance)
    origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
  } else {
    if(any(stringr::str_detect(zHV$DHID, origin))){
      origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    } else {
      stop(
        sprintf(
          "Origin DHID code is not in data set."
        ),
        call. = FALSE
      )
    }
  }
  
  if (!stringr::str_detect(destination, "[a-z]{2}:[:digit:]{5}")) {
    coordinates <- ggmap::geocode(destination)
    data("zHV")
    zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                             cbind(zHV$Longitude, zHV$Latitude))
    zHV <- dplyr::arrange(zHV, distance)
    destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
  } else {
    if(any(stringr::str_detect(zHV$DHID, destination))){
      destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    } else {
      stop(
        sprintf(
          "Origin DHID code is not in data set."
        ),
        call. = FALSE
      )
    }
  }
  
  if (is(time, "POSIXt")) {
    date <- format(time, "%Y%m%d")
    time <- format(time, "%H%M")  
  } else {
    stop(
      sprintf(
        "Date and time are not in the correct format."
      ),
      call. = FALSE
    )
  }
  
  
}
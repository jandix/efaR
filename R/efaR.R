#' Wrapper function for EFA XML API.
#'
#' @param origin The origin of the journey. Either a name, coordinates or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'. Coordinates should be a vector with lon/lat.
#' @param destination The destination of the journey. Either a name, coordinates or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'. Coordinates should be a vector with lon/lat.
#' @param time The date and time of the journey.
#' @param simplify Simplify determines whether the original xml result is returned or a simpler S3 object.
#'
#' @return Either the original xml result or an S3 object based on the simplify input. 
#'
#' @export

efaR <- function (origin,
                  destination,
                  datetime = Sys.time(),
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
  
  
  if (is.numeric(origin)) {
    data("zHV")
    zHV$distance <- geosphere::distHaversine(c(origin[1], origin[2]),
                                             cbind(zHV$Longitude, zHV$Latitude))
    zHV <- dplyr::arrange(zHV, distance)
    origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    rm(zHV)
  } else {
    if (!stringr::str_detect(origin, "[a-z]{2}:[:digit:]{5}")) {
      coordinates <- ggmap::geocode(origin, messaging = FALSE)
      data("zHV")
      zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                               cbind(zHV$Longitude, zHV$Latitude))
      zHV <- dplyr::arrange(zHV, distance)
      origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
      rm(zHV)
    } else {
      if(any(stringr::str_detect(zHV$DHID, origin))){
        origin <- list(name = zHV$Name[zHV$DHID == origin], dhid = origin)
      } else {
        stop(
          sprintf(
            "Destination DHID code is unknown."
          ),
          call. = FALSE
        )
      }
    }
  }
  
  if (is.numeric(destination)) {
    data("zHV")
    zHV$distance <- geosphere::distHaversine(c(destination[1], destination[2]),
                                             cbind(zHV$Longitude, zHV$Latitude))
    zHV <- dplyr::arrange(zHV, distance)
    destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    rm(zHV)
  } else {
    if (!stringr::str_detect(destination, "[a-z]{2}:[:digit:]{5}")) {
      coordinates <- ggmap::geocode(destination, messaging = FALSE)
      data("zHV")
      zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                               cbind(zHV$Longitude, zHV$Latitude))
      zHV <- dplyr::arrange(zHV, distance)
      destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
      rm(zHV)
    } else {
      if(any(stringr::str_detect(zHV$DHID, destination))){
        destination <- list(name = zHV$Name[zHV$DHID == destination], dhid = destination)
      } else {
        stop(
          sprintf(
            "Destination DHID code is unknown."
          ),
          call. = FALSE
        )
      }
    }
  }
  
  if (is(datetime, "POSIXt")) {
    date_string <- format(datetime, "%Y%m%d")
    time_string <- format(datetime, "%H%M")  
  } else {
    stop(
      sprintf(
        "Date and time are not in the correct format."
      ),
      call. = FALSE
    )
  }
  
  result <- efaR_get_xml(origin = origin$name, 
                      destination = destination$name,
                      date = date_string,
                      time = time_string)
  
  if(!simplify) {
    return(result$result)
  } 
  
  
  routes <- xml2::xml_find_all(result$result, ".//itdRoute")
  
  
  
  
  meta <- list(origin = origin,
               destination = destination,
               url = result$url,
               datetime = datetime)
  
 
  
}
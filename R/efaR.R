#' Wrapper function for EFA XML API.
#'
#' @param origin The origin of the journey. Either a name, coordinates or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'. Coordinates should be a vector with lon/lat.
#' @param destination The destination of the journey. Either a name, coordinates or a DHID Code. If a name is provided the next station is calculated based on the geocode function and the 'Haversine distance'. Coordinates should be a vector with lon/lat.
#' @param time The date and time of the journey.
#' @param simplify Simplify determines whether the original xml result is returned or a simpler S3 object.
#'
#' @return Either the original xml result or an S3 object based on the simplify input. 
#'
#' @examples
#' # name example
#' efaR(origin = "Konstanz", destination = "Stuttgart")
#' 
#' # coordinates example
#' efaR(origin = c(9.173238, 47.67795), destination = "Stuttgart")
#' 
#' # dhid example
#' efaR(origin = "de:08335:6554", destination = "Stuttgart")
#'
#' @export

efaR <- function (origin,
                  destination,
                  datetime = Sys.time(),
                  simplify = TRUE
) {
# if the origin value is missing break
  if (missing(origin)) {
    stop(
      sprintf(
        "Please provide an origin."
      ),
      call. = FALSE
    )
  }
  
  # if the destination value is missing break
  if (missing(destination)) {
    stop(
      sprintf(
        "Please provide an destination."
      ),
      call. = FALSE
    )
  }
  
  # if coordinates are provided calculate next station directly
  if (is.numeric(origin)) {
    
    # load zhv data set
    data("zHV")
    
    # calculate distance between origin and stations
    zHV$distance <- geosphere::distHaversine(c(origin[1], origin[2]),
                                             cbind(zHV$Longitude, zHV$Latitude))
    # sort by distance
    zHV <- dplyr::arrange(zHV, distance)
    
    # extract closest station
    origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    
    # detach zhv data set
    rm(zHV)
    
  } else {
    # if is not dhid number get coordinates
    
    if (!stringr::str_detect(origin, "[a-z]{2}:[:digit:]{5}")) {
      
      # get coordinates with google geocode api
      coordinates <- ggmap::geocode(origin, messaging = FALSE)
      
      # load zhv data set
      data("zHV")
      
      # calculate distance between origin and stations
      zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                               cbind(zHV$Longitude, zHV$Latitude))
      
      # sort by distance
      zHV <- dplyr::arrange(zHV, distance)
      
      # extract closest station
      origin <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
      
      # detach zhv data set
      rm(zHV)
      
    } else {
      
      # check if dhid code in dataset else break
      if(any(stringr::str_detect(zHV$DHID, origin))){
        # extract name
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
  
  # if coordinates are provided calculate next station directly
  if (is.numeric(destination)) {
    
    # load zhv data set
    data("zHV")
    
    # calculate distance between destination and stations
    zHV$distance <- geosphere::distHaversine(c(destination[1], destination[2]),
                                             cbind(zHV$Longitude, zHV$Latitude))
    # sort by distance
    zHV <- dplyr::arrange(zHV, distance)
    
    # extract closest station
    destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
    
    # detach zhv data set
    rm(zHV)
    
  } else {
    # if is not dhid number get coordinates
    
    if (!stringr::str_detect(destination, "[a-z]{2}:[:digit:]{5}")) {
      
      # get coordinates with google geocode api
      coordinates <- ggmap::geocode(destination, messaging = FALSE)
      
      # load zhv data set
      data("zHV")
      
      # calculate distance between destination and stations
      zHV$distance <- geosphere::distHaversine(c(coordinates$lon, coordinates$lat),
                                               cbind(zHV$Longitude, zHV$Latitude))
      
      # sort by distance
      zHV <- dplyr::arrange(zHV, distance)
      
      # extract closest station
      destination <- list(name = zHV$Name[1], dhid = zHV$DHID[1])
      
      # detach zhv data set
      rm(zHV)
      
    } else {
      
      # check if dhid code in dataset else break
      if(any(stringr::str_detect(zHV$DHID, destination))){
        # extract name
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
  
  # check if datetime has the correct format else break
  if (is(datetime, "POSIXt")) {
    # extract date string
    date_string <- format(datetime, "%Y%m%d")
    # extract time string
    time_string <- format(datetime, "%H%M")  
  } else {
    stop(
      sprintf(
        "Date and time are not in the correct format."
      ),
      call. = FALSE
    )
  }
  
  # get xml result
  result <- efaR_get_xml(origin = origin$dhid, 
                         destination = destination$dhid,
                         date = date_string,
                         time = time_string)
  
  # if not simplify return raw xml result
  if(!simplify) {
    return(result$result)
  } 
  
  # extract routes
  nodes <- xml2::xml_find_all(result$result, ".//itdRoute")
  
  # define varibales for loop
  routes <- NULL
  mean_duration <- 0
  mean_changes <- 0
  
  # extract route information
  for (i in 1:length(nodes)) {
    # extract single route
    route <- xml2::xml_attrs(nodes[[i]])
    
    # get duration and format as minute
    duration <- lubridate::period_to_seconds(lubridate::hm(route["publicDuration"])) / 60
    
    # get depature time
    depature <- as.POSIXct(route["cTime"], format="%Y%m%d%H%M%OS")
    
    # calculate the arrival time
    arrival <- depature + duration * 60
    
    # glue everything together
    routes <- append(routes,
                     list(
                       list(
                         depature = depature,
                         arrival = arrival,
                         duration = duration,
                         changes = as.numeric(route["changes"])))
    )
    
    # add duration
    mean_duration <- mean_duration + duration
    
    # add changes
    mean_changes <- mean_changes + as.numeric(route["changes"])
  }
  
  # calculate mean duration
  mean_duration <- mean_duration / i
  
  # calculate mean changes
  mean_changes <- mean_changes / i
  
  # define meta details
  meta <- list(origin = origin,
               destination = destination,
               url = result$url,
               datetime = datetime,
               mean_duration = mean_duration,
               mean_changes = mean_changes)
  
  # return meta and routes
  list(meta = meta,
       routes = routes)
}
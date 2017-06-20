efaR_get_xml <- function(origin,
                     destination,
                     date,
                     time) {
  
  # paste url
  url <- "www.efa-bw.de/nvbw/XML_TRIP_REQUEST2?language=de&itdLPxx_frames=false&tripMacro=nvbwLink&type_origin=any&type_destination=any&lineRestriction=400&routeType=LEASTTIME&useProxFootSearch=1&excludedMeans=12&excludedMeans=17&excludedMeans=18&excludedMeans=19&trITDepMOT=100&trITArrMOT=100&trITDepMOTvalue100=15&trITArrMOTvalue100=15"
  
  parameters <- list(name_origin = origin, name_destination, itdDate = date, idtTime = time)
  
  url <- httr::modify_url(url, query = parameters)
  
  # get the document
  request_result <- httr::GET(url)
  
  #define return value
  return_value <- NA
  
  if (request_result$status_code == 200) {
    
    # get content out of doucment
    result_raw <- content(request_result, as = "text", encoding = "Latin1")
    
    # parse document
    result_parsed <- read_xml(result_raw)
    
    # get the route
    route <- xml_attrs(xml_find_all(result_parsed, ".//itdRoute"))
    stations <- xml_attrs(xml_find_all(xml_find_first(result_parsed, ".//itdPartialRoute"), ".//itdPoint"))
    
    
    if (length(route) > 0) {
      duration <- 0
      changes <- 0
      for (j in 1:length(route)) {
        duration <- duration + period_to_seconds(hm(route[[j]][['publicDuration']])) / 60
        changes <- changes + as.numeric(route[[j]][['changes']])
      }
      stops <- NULL
      for (k in 1:length(stations)) {
        if (!is.na(stations[[k]]['name'])) {
          station <- paste(stations[[k]]['name'], stations[[k]]['nameWO'], stations[[k]]['gid'], sep=",")
          stops <- paste(stops, station, sep = ";")
        }
      }
      stops <- str_replace(stops, ";", "")
      
      return_value <- list(duration = duration / j,
                           changes = changes /j,
                           stops = stops)
      
    }
    
    
    
  }
  
  
}
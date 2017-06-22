efaR_get_xml <- function(origin,
                         destination,
                         date,
                         time) {
  
  # paste url
  url <- "https://www.efa-bw.de/nvbw/XML_TRIP_REQUEST2?language=de&itdLPxx_frames=false&tripMacro=nvbwLink&type_origin=any&type_destination=any&lineRestriction=400&routeType=LEASTTIME&useProxFootSearch=1&excludedMeans=12&excludedMeans=17&excludedMeans=18&excludedMeans=19&trITDepMOT=100&trITArrMOT=100&trITDepMOTvalue100=15&trITArrMOTvalue100=15"
  
  parameters <- list(name_origin = origin, name_destination = destination, itdDate = date, idtTime = time)
  
  url <- httr::modify_url(url, query = parameters)
  
  # get the document
  response <- httr::GET(url)
  
  # error message
  if (httr::http_error(response)) {
    stop(
      sprintf(
        "ZEIT Online API request failed [%s]\n%s\nURL: %s",
        httr::status_code(response),
        url
      ),
      call. = FALSE
    )
  }
  
  # get content out of doucment
  result_raw <- httr::content(response, as = "text", encoding = "Latin1")
  
  # parse document
  list(url = url,
       result = xml2::read_xml(result_raw))
  
}
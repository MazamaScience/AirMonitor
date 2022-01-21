# Testing reliability of types in OpenCage returns

library(MazamaLocationUtils)

setLocationDataDir("~/Projects/MazamaScience/known-locations/AirNow")

locationTbl <- table_load("airnow_PM2.5_sites")

openCageList <- list()





for ( i in c(96,100,118)) { ###seq_len(nrow(locationTbl)) ) {
  
  if ( (i %% 10) == 0 ) message("Working on ", i, " ...")
  
  openCageList[[i]] <-
    location_getOpenCageInfo(
      longitude = locationTbl$longitude[i],
      latitude = locationTbl$latitude[i],
      verbose = FALSE
    )
  
  
  
}



for ( i in seq_len(length(openCageList)) ) {
  
  if ( "components.road_reference" %in% names(openCageList[[i]]) ) {
    
    print(sprintf(
      "%d: %f, %f",
      i,
      openCageList[[i]]$long,
      openCageList[[i]]$lat
    ))
    
  }
  
}


a <- 
  dplyr::tibble(
    long = c(-101.428100, -122.440201),
    lat = c(47.185800, 48.762699)
  )

openCageTbl <-
  tidygeocoder::reverse_geocode(
    .tbl = dplyr::tibble(
      lat = c(47.185800, 48.762699),
      long = c(-101.428100, -122.440201)
    ),
    lat = "lat",
    long = "long",
    full_results = TRUE,
    method = "opencage"
  )



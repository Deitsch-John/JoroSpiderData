
CountyMap <- function(county){
  
  Counties <- UScounties %>%
    dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))%>%
    filter(CountyState==county)
  
  obs_county <- st_join(Joro.sf, Counties)%>%
    dplyr::filter(CountyState==county)
  
  ggplot()+
    geom_sf(data = filter(Counties, CountyState==county),
            fill = "white",
            color = "grey22",
            size = 1)+
    geom_sf(data = obs_county,
            color = "firebrick2",
            alpha = 0.6,
            size = 5)+
    theme(
      panel.grid.major = element_blank(), 
      panel.background = element_rect(fill = "White"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
}

CountyLocation <- function(county){
  
  County <- UScounties %>%
    dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))%>%
    dplyr::filter(CountyState==county)
  
  Box <- sf::st_buffer(County, 40000)%>%
    sf::st_bbox(County)%>%
    sf::st_as_sfc(., crs = 4269)
  
  States <- USstates %>%
    dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))
  
  ggplot()+
    geom_sf(data = County,
            fill = "Red",
            color = "grey21",
            size = 0.20)+
    geom_sf(data = States,
            fill = NA,
            color = "grey10",
            size = 1)+
    geom_sf(data = Box,
            color = "Red",
            size = 1,
            fill = NA)+
    theme(
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "White"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
}

CountyMapPage <- function(county){
  map <- CountyMap(county)
  location <- CountyLocation(county)
  
  page <- ggpubr::ggarrange(map, location, 
                            heights = c(3, 1),
                            widths = c(3, 1))
  
  return(page)
}

print_statements <- function(county){
  Counties <- UScounties %>%
    dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))
  
  obs <- obs_county <- st_join(Joro.sf, Counties)%>%
    filter(CountyState==county)
  
  number <- nrow(obs)
  
  year <- if(number>0){
    min(obs$observed_on_details.year)
  } else{
    str_c("blank", sep = "")
  }
  
  string <- if(number>0){
    stringr::str_c("Joro Spiders have been reported ", number, " time(s) to iNaturalist in ", county, ". They were first reported here in ", year, ".", sep = "")
  } else{
    stringr::str_c("As of ", CurrentDate, ", Joro Spiders have not been reported to iNaturalist in ", county, ".", sep = "")
  }
  
  return(string)
}


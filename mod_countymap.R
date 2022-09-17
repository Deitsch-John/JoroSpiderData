
CountyMap <- function(county){
  
  County <- Counties%>%
    dplyr::filter(CountyState==county)%>%
    rmapshaper::ms_simplify(., keep = 0.2)
  
  obs_county <- sf::st_join(Species_observations, Counties)%>%
    dplyr::filter(CountyState==county)
  
  ggplot()+
    ggplot2::geom_sf(data = County,
            fill = "white",
            color = "grey22",
            size = 1)+
    ggplot2::geom_sf(data = obs_county,
            color = "firebrick2",
            alpha = 0.6,
            size = 5)+
    ggplot2::theme(
      panel.grid.major = element_blank(), 
      panel.background = element_rect(fill = "White"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
}

CountyLocation <- function(county){
  
  County <- Counties%>%
    dplyr::filter(CountyState==county)%>%
    rmapshaper::ms_simplify(., keep = 0.2)
  
  Box <- sf::st_buffer(County, 40000)%>%
    sf::st_bbox(County)%>%
    sf::st_as_sfc(., crs = 4269)
  
  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = County,
            fill = "Red",
            color = "grey21",
            size = 0.20)+
    ggplot2::geom_sf(data = States,
            fill = NA,
            color = "grey10",
            size = 1)+
    ggplot2::geom_sf(data = Box,
            color = "Red",
            size = 1,
            fill = NA)+
    ggplot2::theme(
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "White"),
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank())
  
  return(Map)
}

CountyMapPage <- function(county){
  map <- CountyMap(county)
  location <- CountyLocation(county)
  
  page <- ggpubr::ggarrange(map, location, 
                            heights = c(2, 1),
                            widths = c(2, 1))
  
  return(page)
}

print_statements <- function(county){
  
  obs <- sf::st_join(Species_observations, Counties)%>%
    filter(CountyState==county)
  
  number <- nrow(obs)
  
  year <- if(number>0){
    min(obs$observed_on_details.year)
  } else{
    str_c("blank", sep = "")
  }
  
  string <- if(number>1){
    stringr::str_c("Joro Spiders have been reported ", number, " times to iNaturalist in ", county, ". They were first reported here in ", year, ".", sep = "")
  } else if(number==1){
    stringr::str_c("Joro Spiders have been reported 1 time to iNaturalist in ", county, ". They were first reported here in ", year, ".", sep = "")
  } else{
    stringr::str_c("As of ", CurrentDate, ", Joro Spiders have not been reported to iNaturalist in ", county, ".", sep = "")
  }
  
  return(string)
}


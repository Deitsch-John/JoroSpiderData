# range map module
range_map_lf <- function(dataframe, type){
  
  Presence <- . <- NULL
  
  popt <- Species_presence %>%
    sf::st_drop_geometry()%>%
    dplyr::select(CountyState,
                  First_Seen)%>%
    dplyr::rename("County, State" = "CountyState",
                  "Year First Seen" = "First_Seen")%>%
    leafpop::popupTable(feature.id = FALSE,
               row.numbers = FALSE)
  
  Species_presence$Presence <- factor(Species_presence$Presence,
                                      levels = c("No", "Yes"))
  
  colors <- RColorBrewer::brewer.pal(9, "Reds")[3:9]
  
  pal2 =
    colorFactor(palette = c("White", colors), domain = Species_presence$First_Seen, reverse = TRUE)
  
  pal = 
    colorFactor(palette = c("White", 
                            "Green"), domain = Species_presence$Presence)
  
  Map <- if(type=="Presence"){
    leaflet()%>%
      addPolygons(data = Species_presence,
                  fillColor = ~pal(Presence),
                  color = "Black",
                  weight = 1,
                  fillOpacity = 0.7,
                  popup = popt)%>%
      addPolylines(data = States,
                   color = "Black",
                   weight = 2)%>%
      setMapWidgetStyle(list(background="white"))%>%
      addScaleBar(position = c("topright"))
  } else if(type == "years"){
    leaflet()%>%
      addPolygons(data = Species_presence,
                  fillColor = ~pal2(First_Seen),
                  color = "Black",
                  weight = 1,
                  fillOpacity = 1,
                  popup = popt)%>%
      addPolylines(data = States,
                   color = "Black",
                   weight = 2)%>%
      addLegend(pal = pal2, 
                values = Species_presence$First_Seen,
                title = "Year First Seen")%>%
      setMapWidgetStyle(list(background="white"))%>%
      addScaleBar(position = c("topright"))
  } else{
    print("oops")
  }
  return(Map)
  
}

observation_map_lf <- function(dataframe){
  
  popt <- Species_observations %>%
    sf::st_drop_geometry()%>%
    dplyr::select(quality_grade,
                  date_observed)%>%
    dplyr::mutate(quality_grade2 = dplyr::case_when(
      quality_grade=="research"~"Research Grade",
      quality_grade=="needs_id"~"Unconfirmed"
    ))%>%
    dplyr::select(-quality_grade)%>%
    dplyr::rename("Observation Grade" = "quality_grade2",
                  "Date" = "date_observed")%>%
    leafpop::popupTable(feature.id = FALSE,
               row.numbers = FALSE)
  
  Map <- leaflet()%>%
    leaflet::addPolylines(data = States,
                 color = "Black",
                 weight = 2)%>%
    leaflet::addCircles(data = Species_observations,
               fillColor = "#A50F15",
               fillOpacity = 0.7,
               color = "#A50F15",
               radius = 800,
               popup = popt)%>%
    leaflet.extras::setMapWidgetStyle(list(background="white"))%>%
    leaflet::addScaleBar(position = c("topright"))
    
  return(Map)
  
}

# range_map <- function(dataframe, type){
#   
#   Presence <- . <- NULL
#   
#   sf::st_crs(dataframe)=4269
#   sf::st_crs(UScounties)=4269
#   sf::st_crs(USstates)=4269
#   
#   Counties <- UScounties %>%
#     dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))%>%
#     dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))
#   
#   Counties.t <- sf::st_transform(Counties, crs = 5070)
#   Species_observations.t <- sf::st_transform(dataframe, crs = 5070)
#   States.t <- USstates %>%
#     dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))%>%
#     sf::st_transform(., crs = 5070)
#   
#   Species_Counties <- sf::st_join(Species_observations, Counties.t)%>%
#     sf::st_drop_geometry()%>%
#     dplyr::group_by(CountyState)%>%
#     dplyr::summarize(Observations = dplyr::n(),
#                      firstseen = min(observed_on_details.year))%>%
#     dplyr::filter(!is.na(CountyState))
#   
#   Species_presence <- Counties.t %>%
#     dplyr::left_join(Species_Counties)%>%
#     dplyr::mutate(Presence = ifelse(is.na(Observations), "No", "Yes"),
#                   First_Seen = ifelse(is.na(firstseen)==TRUE, "Not Seen", firstseen))%>%
#     dplyr::select(-firstseen)
#   
#   Map <- if(type=="Presence"){
#     ggplot2::ggplot()+
#       ggplot2::geom_sf(data = dplyr::filter(Species_presence, Presence=="Yes"),
#                        fill = "seagreen3",
#                        color = "grey41",
#                        size = 0.10)+
#       ggplot2::geom_sf(data = dplyr::filter(Species_presence, Presence=="No"),
#                        fill = "ivory1",
#                        color = "grey41",
#                        size = 0.10)+
#       ggplot2::geom_sf(data = States.t,
#                        fill = NA,
#                        color = "grey10",
#                        size = 0.5)+
#       ggplot2::theme(
#         panel.grid.major = ggplot2::element_blank(),
#         panel.background = ggplot2::element_rect(fill = "White"),
#         panel.border = ggplot2::element_rect(fill = NA, size = 2))
#   } else if(type == "years"){
#     ggplot2::ggplot()+
#       ggplot2::geom_sf(data = dplyr::filter(Species_presence, First_Seen!="Not Seen"),
#                        color = "grey41",
#                        size = 0.10,
#                        aes(fill = First_Seen))+
#       ggplot2::geom_sf(data = dplyr::filter(Species_presence, First_Seen=="Not Seen"),
#                        fill = "White",
#                        color = "grey41",
#                        size = 0.10)+
#       ggplot2::geom_sf(data = State.t,
#                        fill = NA,
#                        color = "grey10",
#                        size = 0.5)+
#       scale_fill_viridis(option = "viridis", direction = 1, discrete = TRUE)+
#       ggplot2::theme(
#         panel.grid.major = ggplot2::element_blank(),
#         panel.background = ggplot2::element_rect(fill = "White"),
#         panel.border = ggplot2::element_rect(fill = NA, size = 2))
#   } else{
#     print("oops")
#   }
#   return(Map)
#   
# }
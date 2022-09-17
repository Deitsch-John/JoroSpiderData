# phenology functions

construct_pheno <- function(regionx){
  year.tca <- tibble(
    day = c(1:365))
    
  GeorgiaObs <- Species_observations[Georgia,]
  
  pheno_georgia <- if(regionx == "All"){
    GeorgiaObs%>%
      dplyr::filter(observed_on_details.year!="2022")%>%
      sf::st_join(., regions)%>%
      sf::st_drop_geometry()%>%
      dplyr::group_by(yearday)%>%
      dplyr::summarise(obs = n())%>%
      dplyr::right_join(year.tca, by = c("yearday" = "day"))%>%
      dplyr::mutate(obs=ifelse(is.na(obs)==TRUE,0,obs))%>%
      dplyr::arrange(yearday)%>%
      dplyr::mutate(weeklyavg = rollmean(x = obs, k = 7, fill = NA, align = c("center")))%>%
      dplyr::mutate(Date = as.Date(yearday, origin = "2021-12-31"))%>%
      dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = F),
                    Day = lubridate::day(Date))%>%
      tidyr::unite(Datex, Month:Day, sep = " ")%>%
      dplyr::select(yearday, obs, weeklyavg, Datex)%>%
      dplyr::rename("Day of Year" = "yearday",
                    "Observations" = "obs",
                    "Weekly Average Observations" = "weeklyavg",
                    "Date" = "Datex")
  } else {pheno_georgia <- 
  GeorgiaObs%>%
    dplyr::filter(observed_on_details.year!="2022")%>%
    sf::st_join(., regions)%>%
    sf::st_drop_geometry()%>%
    dplyr::filter(region==regionx)%>%
    dplyr::group_by(yearday)%>%
    dplyr::summarise(obs = n())%>%
    dplyr::right_join(year.tca, by = c("yearday" = "day"))%>%
    dplyr::mutate(obs=ifelse(is.na(obs)==TRUE,0,obs))%>%
    dplyr::arrange(yearday)%>%
    dplyr::mutate(weeklyavg = rollmean(x = obs, k = 7, fill = NA, align = c("center")))%>%
    dplyr::mutate(Date = as.Date(yearday, origin = "2021-12-31"))%>%
    dplyr::mutate(Month = lubridate::month(Date, label = TRUE, abbr = F),
                  Day = lubridate::day(Date))%>%
    tidyr::unite(Datex, Month:Day, sep = " ")%>%
    dplyr::select(yearday, obs, weeklyavg, Datex)%>%
    dplyr::rename("Day of Year" = "yearday",
                  "Observations" = "obs",
                  "Weekly Average Observations" = "weeklyavg",
                  "Date" = "Datex")}

  return(pheno_georgia)
}

pheno_plot <- function(regionx){
  pheno_georgia <- construct_pheno(regionx)
  
  plot <- ggplot(pheno_georgia)+
    geom_point(aes(`Day of Year`, `Weekly Average Observations`), 
               alpha = 0.7, size = 2, color = "#A50F15")+
    labs(y = "Weekly Average Observations", x = "Day of the Year")+
    scale_x_continuous(limits = c(0,365), breaks = c(15, 74, 135, 196, 
                                                     258, 319),
                       labels = c("Jan 15", "Mar 15", "May 15",
                                  "Jul 15", "Sep 15",
                                  "Nov 15"))+
    theme(
      panel.grid.major = element_blank(),
      panel.background = element_rect(fill = "White"),
      panel.border = element_blank(),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16))
  
  return(plot)
}

pheno_table <- function(regionx){
  pheno_georgia <- construct_pheno(regionx)
  
  first_sight <- pheno_georgia %>%
    dplyr::filter(Observations > 0)%>%
    dplyr::slice_min(order_by = `Day of Year`, n = 1)%>%
    dplyr::pull(Date)
  
  last_sight <- pheno_georgia %>%
    dplyr::filter(Observations > 0)%>%
    dplyr::slice_max(order_by = `Day of Year`, n = 1)%>%
    dplyr::pull(Date)
  
  peak <- pheno_georgia %>%
    dplyr::filter(Observations > 0)%>%
    dplyr::slice_max(order_by = `Weekly Average Observations`, n = 1)%>%
    dplyr::slice_head(n = 1)%>%
    dplyr::pull(Date)
  
  Information <- tibble(
    Category = c("Earliest Sighting", 
                 "Peak in Sightings", 
                 "Latest Sighting"),
    Date = c(first_sight, 
             peak, 
             last_sight)
  )
  
  table <- Information %>%
    gt::gt()%>%
    gt::cols_label(Category = md("**Category**"),
               Date = gt::md("**Date**"))
  
  return(table)
}

phys_map <- function(){
  
  regions$region <- factor(regions$region,
                           levels = c("Coastal Plain",
                                      "Piedmont",
                                      "Mountains"))
  
  pal =
    colorFactor(palette = "Spectral", domain = regions$region)
  
  Map <- ggplot2::ggplot()+
    ggplot2::geom_sf(data = regions, 
            aes(fill = region), 
            color = NA,
            alpha = 0.9)+
    ggplot2::geom_sf(data = Georgia,
            fill = NA,
            size = 1.3)+
    ggplot2::scale_fill_brewer(type = "qual", palette = "Paired", 
                      direction = 1)+
    ggplot2::theme_void()+
    ggplot2::labs(fill = "Physiographic Region")+
    ggplot2::theme(legend.position = "left")
  
  return(Map)
  
}



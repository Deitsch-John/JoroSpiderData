
url = "https://github.com/Deitsch-John/JoroSpiderData/raw/main/appfiles.zip"
download.file(url, "appfiles.zip")
unzip("appfiles.zip")

UScounties = sf::read_sf("US_counties", dsn = ".")
USstates = sf::read_sf("s_22mr22", dsn = ".")
phys <- read_sf("ga_eco_l3", dsn = ".")

States <- USstates %>%
  dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))

Counties <- UScounties %>%
  dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))%>%
  dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))

JoroData <- readr::read_csv("https://raw.githubusercontent.com/Deitsch-John/JoroSpiderData/main/Joro_Obs.csv")%>%
  dplyr::mutate(Species = "Trichonephila clavata")%>%
  dplyr::filter(quality_grade != "casual")

Joro.sf <- st_as_sf(JoroData,
                    coords = c("longitude",
                               "latitude"),
                    crs = 4269)

Counties_List <- sf::st_join(Joro.sf, Counties)%>%
  sf::st_drop_geometry()%>%
  dplyr::group_by(CountyState)%>%
  dplyr::summarize(Observations = dplyr::n(),
                   firstseen = min(observed_on_details.year))%>%
  dplyr::filter(!is.na(CountyState))%>%
  dplyr::pull(CountyState)

CurrentDate <- Sys.Date()
TotalObservations <- nrow(Joro.sf)
TotalCounties <- length(Counties_List)

# load data ---------------------------------------------------------------

url = "https://github.com/Deitsch-John/JoroSpiderData/raw/main/appfiles.zip"
download.file(url, "appfiles.zip")
unzip("appfiles.zip")

UScounties = sf::read_sf("US_counties", dsn = ".")
USstates = sf::read_sf("s_22mr22", dsn = ".")
phys <- read_sf("ga_eco_l3", dsn = ".")

JoroData <- readr::read_csv("https://raw.githubusercontent.com/Deitsch-John/JoroSpiderData/main/Joro_Obs.csv")%>%
  dplyr::mutate(Species = "Trichonephila clavata")%>%
  dplyr::filter(quality_grade != "casual")

Joro.sf <- sf::st_as_sf(JoroData,
                    coords = c("longitude",
                               "latitude"),
                    crs = 4269)

regions <- phys %>%
  dplyr::select(US_L3NAME)%>%
  dplyr::filter(US_L3NAME%in%c("Southeastern Plains", "Southern Coastal Plain", "Middle Atlantic Coastal Plain",
                               "Blue Ridge", "Southwestern Appalachians", "Ridge and Valley", "Piedmont"))%>%
  sf::st_transform(crs = 4326)%>%
  dplyr::mutate(region = case_when(
    US_L3NAME%in%c("Southeastern Plains", "Southern Coastal Plain", "Middle Atlantic Coastal Plain")~"Coastal Plain",
    US_L3NAME%in%c("Blue Ridge", "Southwestern Appalachians", "Ridge and Valley")~"Mountains",
    US_L3NAME=="Piedmont"~"Piedmont",
  ))%>%
  dplyr::group_by(region)%>%
  dplyr::summarize()

# create objects ----------------------------------------------------------

States <- USstates %>%
  dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))%>%
  sf::st_transform(., crs = 4326)

Georgia <- States %>%
  dplyr::filter(STATE%in%c("GA"))

Counties <- UScounties %>%
  dplyr::filter(STATE%in%c("NC", "SC", "TN", "AL", "FL", "MS", "GA"))%>%
  dplyr::mutate(CountyState = stringr::str_c(COUNTYNAME, STATE, sep = ", "))%>%
  sf::st_transform(., crs = 4326)

Species_observations <- sf::st_transform(Joro.sf, crs = 4326)

Species_Counties <- sf::st_join(Species_observations, Counties)%>%
  sf::st_drop_geometry()%>%
  dplyr::group_by(CountyState)%>%
  dplyr::summarize(Observations = dplyr::n(),
                   firstseen = min(observed_on_details.year))%>%
  dplyr::filter(!is.na(CountyState))

Species_presence <- Counties %>%
  dplyr::left_join(Species_Counties)%>%
  dplyr::mutate(Presence = ifelse(is.na(Observations), "No", "Yes"),
                First_Seen = ifelse(is.na(firstseen)==TRUE, "Not Seen", firstseen))%>%
  dplyr::select(-firstseen)

Counties_List <- Species_Counties%>%
  dplyr::pull(CountyState)

CurrentDate <- Sys.Date()
TotalObservations <- nrow(Joro.sf)
TotalCounties <- length(Counties_List)
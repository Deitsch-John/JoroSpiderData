library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(git2r)

setwd("C:/Users/jfdei/OneDrive/Desktop/JoroData_gh/JoroSpiderData")

Old_Data <- readr::read_csv("https://raw.githubusercontent.com/Deitsch-John/JoroSpiderData/main/Joro_Obs.csv")

gert::git_rm(files = "Joro_Obs.csv",
             repo = getwd())

gert::git_commit(message = stringr::str_c("New data batch:", CurrentDate, sep = " "),
                 repo = getwd())

gert::git_push(repo = getwd(),
               ssh_key = "C:\\Users\\jfdei\\.ssh\\id_rsa")

gert::git_pull(repo = getwd(),
               ssh_key = "C:\\Users\\jfdei\\.ssh\\id_rsa")

unlink("Joro_Obs.csv")

Get_Last_Two_Weeks <- function(){
  options(stringsAsFactors = FALSE)
  
  CurrentDate <- Sys.Date()
  TwoWeeksAgo <- CurrentDate-14
  
  api_path <- "https://api.inaturalist.org/v1/observations"
  
  res <- httr::GET(api_path, query=c(
    list(
      per_page = 200,
      order = "desc",
      order_by = "observed_on",
      taxon_id=904334,
      place_id=1,
      d1 = TwoWeeksAgo),
    list(page=1)
  )
  )
  
  resDF <- jsonlite::fromJSON(httr::content(res, as = "text"),
                              flatten=TRUE)
  
  for (i in 2:(ceiling(resDF$total_results/resDF$per_page))) {
    res.t <- httr::GET(api_path, query=c(list(
      per_page = 200,
      order = "desc",
      order_by = "observed_on",
      taxon_id=904334,
      place_id=1,
      d1 = TwoWeeksAgo),
      list(page=i)))
    resDF.t <- fromJSON(httr::content(res.t, as = "text"),flatten=TRUE)
    resDF$results <- bind_rows(resDF$results,resDF.t$results)
    Sys.sleep(1)
    
    iNatDF <- resDF$results
  }
  
  JoroObs <- iNatDF %>%
    dplyr::select(quality_grade,
                  id,
                  public_positional_accuracy,
                  observed_on,
                  obscured,
                  location,
                  observed_on_details.week,
                  observed_on_details.month,
                  observed_on_details.year,
    ) %>%
    dplyr::filter(public_positional_accuracy <= 1000 & obscured=="FALSE")%>%
    dplyr::mutate(date_observed = as.Date(observed_on, format = "%Y-%m-%d"))%>%
    dplyr::mutate(yearday = lubridate::yday(date_observed))%>%
    dplyr::select(-observed_on)%>%
    tidyr::separate(col = location, into = c("latitude", "longitude"), sep = ",")%>%
    dplyr::mutate_at(.vars = c("latitude", "longitude"), as.numeric)%>%
    dplyr::distinct()
  
  return(JoroObs)
}

New_Data <- Get_Last_Two_Weeks() #grab new data from iNaturalist

Updated_Data <- Old_Data %>% #merge new with old data
  dplyr::bind_rows(New_Data)%>%
  dplyr::distinct()%>%
  dplyr::arrange(-id)

readr::write_csv(Updated_Data, "Joro_Obs.csv", append = FALSE)

CurrentDate <- Sys.Date()

setwd("C:/Users/jfdei/OneDrive/Desktop/JoroShiny")
setwd("C:/Users/jfdei/OneDrive/Desktop/JoroData_gh/JoroSpiderData")

gert::git_add(files = "Joro_Obs.csv",
             repo = getwd())

gert::git_commit(message = stringr::str_c("New data batch:", CurrentDate, sep = " "),
                 repo = getwd())

gert::git_push(repo = getwd(),
               ssh_key = "C:\\Users\\jfdei\\.ssh\\id_rsa")

setwd("C:/Users/jfdei/OneDrive/Desktop/JoroShiny")

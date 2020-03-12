library(tidyverse)
library(foreach)

Team_ID <- data.frame(
  Team = 
    c("N.J", "NYI", "NYR", "PHI", "PIT", "BOS", "BUF", "MTL", "OTT", "TOR", "ATL", "CAR", "FLA", "T.B", 
      "WSH", "CHI", "DET", "NSH", "STL", "CGY", "COL", "EDM", "VAN", "ANA", "DAL", "L.A", "ARI", "S.J", 
      "CBJ", "MIN", "WPG", "ARI", "VGK"
    ), 
  ID = c(seq(1:33))
  ) %>% 
  mutate(ID = ifelse(ID == 31, 52, ifelse(ID == 32, 53, ifelse(ID == 33, 54, ID))))

## For identifying event_team in HTM events

sc.scrape_schedule <- function(start_date = Sys.Date(), end_date = Sys.Date(), print_sched = TRUE) { 
  
  ## getURL for schedule data
  url_schedule <- NULL
  try_count <- 3
  
  while (!is.character(url_schedule) & try_count > 0) { 
    
    url_schedule <- try(
      RCurl::getURL(
        paste0(
          "https://statsapi.web.nhl.com/api/v1/schedule?startDate=",
          as.character(start_date),
          "&endDate=",
          as.character(end_date)
          )
        )
      )
    
    try_count <- try_count - 1
    
    }
  
  ## Parse JSON data
  if (is.character(url_schedule)) {
    schedule_list <- jsonlite::fromJSON(url_schedule)
    
    }
  
  ## Return from function if scrape returned no data
  if (length(schedule_list$dates) == 0) {
    
    warning("NHL Schedule API Returned No Data")
    
    ## Return empty data.frame in same format if error occurred
    return(
      data.frame(
        game_id = character(), 
        game_date = character(), 
        season = character(), 
        session = character(), 
        game_status = character(), 
        away_team = character(), 
        home_team = character(), 
        game_venue = character(), 
        game_datetime = character(), 
        EST_time_convert = character(), 
        EST_date = character(), 
        stringsAsFactors = FALSE
        )
      )
    
    }
  
  
  ## Bind games from schedule list
  bind_schedule <- foreach::foreach(i = 1:length(schedule_list$dates$games), .combine = rbind) %do% {
    
    schedule_current <- data.frame(
      game_id =       as.character(schedule_list$dates$games[[i]]$gamePk), 
      game_date =     as.Date(schedule_list$dates$games[[i]]$gameDate),
      season =        schedule_list$dates$games[[i]]$season, 
      session =       schedule_list$dates$games[[i]]$gameType, 
      game_status =   schedule_list$dates$games[[i]]$status$detailedState, 
      away_team_id =  schedule_list$dates$games[[i]]$teams$away$team$id, 
      home_team_id =  schedule_list$dates$games[[i]]$teams$home$team$id, 
      game_venue =    schedule_list$dates$games[[i]]$venue$name, 
      game_datetime = schedule_list$dates$games[[i]]$gameDate, 
      
      stringsAsFactors = FALSE
      )
    
    }
  
  
  ## Modify bound schedule data
  schedule_current <- bind_schedule %>% 
    arrange(game_id) %>% 
    # filter(session != "PR") %>%   ## filter out preseason games
    filter(session %in% c("R", "P")) %>%  ## only scrape regular season and playoff games
    mutate(
      home_team_id = Team_ID$Team[match(home_team_id, Team_ID$ID)], 
      away_team_id = Team_ID$Team[match(away_team_id, Team_ID$ID)], 
      
      EST_time_convert = format(
        as.POSIXct(
          gsub("T", " ", game_datetime) %>% gsub("Z", "", .), 
          tz = "UTC", 
          format = "%Y-%m-%d %H:%M:%S"
          ), 
        tz = "Canada/Eastern"
        ), 
      
      EST_date = as.Date(
        ifelse(is.na(EST_time_convert), as.Date(game_datetime) - 1, EST_time_convert), 
        origin = "1970-01-01"
        ), 
      
      game_date = EST_date
      ) %>% 
    arrange(game_id) %>% 
    rename(
      home_team = home_team_id, 
      away_team = away_team_id
      ) %>% 
    data.frame()
  
  ## Arrange if playoff games
  if ("P" %in% unique(schedule_current$session)) { 
    schedule_current <- arrange(schedule_current, game_date, EST_time_convert)
    
    }
  
  ## print schedule
  if (print_sched == TRUE) print(head(schedule_current, 20))
  
  ## return schedule
  return(schedule_current)
  
}

today <- sc.scrape_schedule()
yesterday <- sc.scrape_schedule(Sys.Date()-1, Sys.Date()-1)

# nhlapi data driven

# FUNCTION DEFINITIONS  ----

setenv <- function(){

library(magrittr)
library(tidyverse)
library(lubridate)
options(tibble.width = Inf)

source(file = "plot_ice_rink.R")

rink <<- nhl_rink_plot()

teamcolors <<- c("ANA" = "#111111","ARI" = "#b43143","BOS" = "#c98e03","BUF" = "#00224d","CGY" = "#ce1126",
                "CAR" = "#76232F","CHI" = "#ec132c","COL" = "#6f263d",
                "CBJ" = "#041e42","DAL" = "#008053","DET" = "#a50d27","EDM" = "#fc4c02","FLA" = "#b9975b",
                "LAK" = "#a2aaad","MIN" = "#154734","MTL" = "#851425",
                "NSH" = "#ffb81c","NJD" = "#a50d27","NYI" = "#003087","NYR" = "#0049e6","OTT" = "#c69214",
                "PHI" = "#fa4616","PIT" = "#ffb81c","SJS" = "#008599",
                "STL" = "#FCB514","TBL" = "#002d80","TOR" = "#002d80","VAN" = "#008852","VGK" = "#3f4e5a",
                "WSH" = "#ec1337","WPG" = "#858f93", "SEA" = "#99d9d9")

datehere <- force_tz(Sys.time(), tzone = "America/Sao_Paulo")
datetor <- with_tz(datehere, tzone = "America/Toronto")
datetoday <<- date(datetor)


# geoms for hdsc

hda_coords <<- tibble(
  x = c(89, 89, 69, 69),
  y = c(10, -10, -10, 10)
)

da_coords <<- tibble(
  x = c(89, 69,  69,  89,  69,  54,  54,  44, 44, 54, 54, 69),
  y = c(10, 10, -10, -10, -22, -22, -10, -10, 10, 10, 22, 22)
)


}

# retrives team schedule games since 01-13-2021 to today
getschedule <- function(season, team, date, type){

schedule <- nhlapi:::nhl_url_schedule(season, team, "2021-01-13", date, type)

game_ids_list <- nhlapi::nhl_get_data(schedule)

game_list <- game_ids_list %>% first() %>% magrittr::extract2("dates") %>% extract2("games")
game_address <<- plyr::rbind.fill(game_list)

#game_df <- game_list %>%  as_tibble(.name_repair = "unique")
#game_ids <- t(janitor::clean_names(as_tibble(map(game_list, ~ .x[["gamePk"]]), .name_repair = "unique")))
#game_link <- t(janitor::clean_names(as_tibble(map(game_list, ~ .x[["link"]]), .name_repair = "unique")))

#game_address <<- as_tibble(cbind(game_ids, game_link))

}

# retrieves data from the game which gameid if set
getgamefeed <- function(gameid){

  gamefeed <- nhlapi::nhl_games_feed(gameid)


  game_live_feed <- gamefeed %>% first() %>%
   magrittr::extract2("liveData") %>%
   magrittr::extract2("plays") %>%
   magrittr::extract2("allPlays") %>%
   as_tibble()

gamedatetime <- gamefeed %>% first() %>%
   magrittr::extract2("gameData") %>%
  magrittr::extract2("datetime") %>%
  magrittr::extract2("dateTime")

gamedatetimeTz <- force_tz(as_datetime(gamedatetime), tz = "UTC")
gamedatetimeTor <- with_tz(gamedatetimeTz, tz = "America/Toronto")

gameid <- gamefeed %>% first() %>%
   magrittr::extract2("gamePk")

statusCode <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("status") %>%
  extract2("statusCode")


home_team <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("name")

away_team <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("name")

away_team_id <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("id")

home_team_id <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("id")

home_team_tricode <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("triCode")

away_team_tricode <- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("triCode")

gameType <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("game") %>%
  extract2("type")

gameId <<- gameid
homeTeam <<- home_team
awayTeam <<- away_team
homeTeamId <<- home_team_id
awayTeamId <<- away_team_id
homeTeamTricode <<- home_team_tricode
awayTeamTricode <<- away_team_tricode
gameDate <<- lubridate::date(gamedatetimeTor)
gameStartTime <<- format(gamedatetimeTor, format="%H:%M %Z")


game_live_feed$gameId <- gameid
game_live_feed$homeTeam <- home_team
game_live_feed$awayTeam <- away_team
game_live_feed$homeTeamId <- home_team_id
game_live_feed$awayTeamId <- away_team_id
game_live_feed$homeTeamTricode <- home_team_tricode
game_live_feed$awayTeamTricode <- away_team_tricode

if (statusCode != "1"){
game_live_feed <- tidyr::unnest_wider(game_live_feed, players)
game_live_feed <- tidyr::unnest_wider(game_live_feed, player.fullName, names_sep = ".")
game_live_feed <- tidyr::unnest_wider(game_live_feed, playerType, names_sep = ".")
game_live_feed <<- tidyr::unnest_wider(game_live_feed, player.id, names_sep = ".")
}else{
  game_live_feed <<- game_live_feed
}


}

# Boxscore function

getboxscore <- function(gameid){

  boxScore <- nhlapi::nhl_games_boxscore(gameid)

# away ----
  awayTeamStatSkaters <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("away") %>%
    extract2("teamStats") %>%
    extract2("teamSkaterStats") %>%
    as_tibble() %>%
    mutate(teamTricode = awayTeamTricode)

  awayPlayers <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("away") %>%
    extract2("players")

  awayPlayersIds <- names(awayPlayers)

  awayGoaliesIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("away") %>%
    extract2("goalies")

   awaySkatersIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("away") %>%
    extract2("skaters")

   awayHealthyScratchesIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("away") %>%
    extract2("scratches")

  awayPlayersIds_int <- as.integer(str_remove(awayPlayersIds, "^[A-Z]{2}"))
  awayPlayers_df <- tibble(players = awayPlayers)

  awayPlayers_df1 <- awayPlayers_df %>%
    unnest_wider(col = players) %>%
    unnest_wider(col = person) %>%
    unnest_wider(col = primaryPosition) %>%
    unnest_wider(col = currentTeam, names_sep = "." ) %>%
    unnest_wider(col = position, names_sep = ".") %>%
    unnest_wider(col = stats) %>%
    unnest_wider(col = skaterStats, names_sep = ".") %>%
    unnest_wider(col = goalieStats, names_sep = ".")

  awayRoster <- awayPlayersIds_int[! awayPlayersIds_int %in% awayHealthyScratchesIds]
  awayRosterSkaters <- awayRoster[! awayRoster %in% awayGoaliesIds]

# home ----

   homeTeamStatSkaters <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("home") %>%
    extract2("teamStats") %>%
    extract2("teamSkaterStats") %>%
    as_tibble() %>%
    mutate(teamTricode = homeTeamTricode)

  homePlayers <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("home") %>%
    extract2("players")

  homePlayersIds <- names(homePlayers)

  homeGoaliesIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("home") %>%
    extract2("goalies")

   homeSkatersIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("home") %>%
    extract2("skaters")

   homeHealthyScratchesIds <- boxScore %>%
    first() %>%
    extract2("teams") %>%
    extract2("home") %>%
    extract2("scratches")

  homePlayersIds_int <- as.integer(str_remove(homePlayersIds, "^[A-Z]{2}"))
  homePlayers_df <- tibble(players = homePlayers)

  homePlayers_df1 <- homePlayers_df %>%
    unnest_wider(col = players) %>%
    unnest_wider(col = person) %>%
    unnest_wider(col = primaryPosition) %>%
    unnest_wider(col = currentTeam, names_sep = "." ) %>%
    unnest_wider(col = position, names_sep = ".") %>%
    unnest_wider(col = stats) %>%
    unnest_wider(col = skaterStats, names_sep = ".") %>%
    unnest_wider(col = goalieStats, names_sep = ".")

  homeRoster <- homePlayersIds_int[! homePlayersIds_int %in% homeHealthyScratchesIds]
  homeRosterSkaters <- homeRoster[! homeRoster %in% homeGoaliesIds]

  teamStats <<- bind_rows(awayTeamStatSkaters, homeTeamStatSkaters)
  teamPlayersStats <<- bind_rows(awayPlayers_df1, homePlayers_df1)

  gameroster <- c(awayRoster, homeRoster)

  gameRoster <<- tibble(id = gameroster)

}

gethdsc <- function(){

  fenwick_events <- c("Shot", "Missed Shot", "Goal")

high_danger <- game_live_feed %>%
    select(-player.link) %>%
    filter(result.event != "Stoppage") %>%
    #filter(about.ordinalNum %in% prd)
    #filter(result.eventTypeId == "SHOT" | result.eventTypeId == "GOAL") %>%
    #select(coordinates.x, coordinates.y, team.name, result.description, result.secondaryType, result.eventTypeId,
    #       about.period, about.ordinalNum, team.triCode, about.eventIdx, about.periodTime) %>%
    mutate(coordinates.x = ifelse(about.period %in% c(2,4,6),  coordinates.x * -1, coordinates.x),
           coordinates.y = ifelse(about.period %in% c(2,4,6),  coordinates.y * -1, coordinates.y)) %>%
    mutate(period_time = as.double(lubridate::ms(about.periodTime))) %>%
    mutate(time_since_last_event = period_time - lag(period_time)) %>%
    mutate(is_rebound = if_else(time_since_last_event <=3 &
                                result.event %in% fenwick_events &
                                team.triCode == lag(team.triCode),
                                TRUE, FALSE)) %>%
    mutate(tor_zone = cut(coordinates.x,breaks = c( -100, -25, 25, 100), labels = c("Off", "Neu", "Def"))) %>%
    mutate(is_goal = if_else(result.event == "Goal", TRUE, FALSE)) %>%
    mutate(is_Tor = if_else(team.triCode == "TOR", TRUE, FALSE)) %>%
    mutate(is_rush = if_else(time_since_last_event < 4 &
                             lag(abs(coordinates.x)) < 25 &
                             result.event %in% fenwick_events, TRUE, FALSE))

high_danger_fenwick <- high_danger %>%
  filter(result.event %in% fenwick_events)

high_danger_fenwick1 <- high_danger_fenwick %>%
  mutate(coord_y = if_else(coordinates.x < 0, -1*coordinates.y, coordinates.y)) %>%
  mutate(coord_x = abs(coordinates.x)) %>%
  mutate(angle_shot = (asin(abs(coord_y)/sqrt((87.95
                                    - abs(coord_x))^2
                                    + coord_y^2))*180)/ 3.14)


da_finder <- function(x,y){

  y <- abs(y)
  y_line <- ((-3/5)*x + 63.4)


  if(x < 44 | x > 89 ){
    da = FALSE
  }else if(x >= 44 & x < 54 & y <= 10){
    da = TRUE
  }else if(x >=54 & x < 69 & y <= 22){
    da = TRUE
  }else if(x >= 69 & x < 89 & y <= y_line){
    da = TRUE
  }else{
    da = FALSE
  }
  return(da)
}

hda_finder <- function(x,y){

  y <- abs(y)
  if(x >=69 & x<= 89 & y <= 10){
    hda = TRUE
  }else{
    hda = FALSE
  }

  return(hda)
}

# base R to the rescue
high_danger_fenwick1$is_danger <- mapply(da_finder, high_danger_fenwick1$coord_x,
                                         high_danger_fenwick1$coord_y)


high_danger_fenwick1$is_high_danger <- mapply(hda_finder, high_danger_fenwick1$coord_x,
                                         high_danger_fenwick1$coord_y)

high_danger_fenwick1 <- high_danger_fenwick1 %>%
  mutate(is_rebound = ifelse(is.na(is_rebound), FALSE, is_rebound)) %>%
  mutate(is_rush = ifelse(is.na(is_rush), FALSE, is_rush)) %>%
   # this is some faulty logic
  mutate(loc_score = ifelse(is_high_danger == is_danger, ifelse(is_danger == TRUE, 3, 1) , ifelse(is_danger == TRUE, 2, 1))) %>%
  mutate(shot_score = ifelse(is_rebound, loc_score + 1, loc_score)) %>%
  mutate(shot_score = ifelse(is_rush, shot_score + 1, shot_score))


high_danger_fenwick3 <- high_danger_fenwick1 %>%
  filter(shot_score >= 3)

high_danger_fenwick1 <<- high_danger_fenwick1
high_danger_fenwick3 <<- high_danger_fenwick3

}

getshiftsdouble <- function(g_id){

 full_url <- url(paste0("https://api.nhle.com/stats/rest/en/shiftcharts?cayenneExp=gameId=", g_id,""))

 #{"GET":{"scheme":"https","host":"api.nhle.com","filename":"/stats/rest/en/shiftcharts","query":{"cayenneExp":"gameId=2020020350 and ((duration != '00:00' and typeCode = 517) or typeCode != 517 )","exclude":["detailCode","duration","eventDetails","teamAbbrev","teamName"]},"remote":{"Address":"[2606:4700::6813:982a]:443"}}}
#{"GET":{"scheme":"https","host":"api.nhle.com","filename":"/stats/rest/en/shiftcharts","query":{"cayenneExp":"gameId=2020020350 and ((duration != '00:00' and typeCode = 517) or typeCode != 517 )","exclude":["detailCode","duration","eventDetails","teamAbbrev","teamName"]},"remote":{"Address":"[2606:4700::6813:982a]:443"}}}
 raw_data <- jsonlite::fromJSON(full_url)

 shifts_data <- tibble(raw_data[["data"]])


 shifts <<- shifts_data %>%
  select(playerId, id, eventNumber, startTime, endTime, duration, period, firstName, lastName, shiftNumber, teamAbbrev, teamName , everything()) %>%
  type_convert(col_types =  cols(.default = col_integer(),
                                  duration = col_time(format = "%M:%S"),
                                  startTime = col_time(format = "%M:%S"),
                                  endTime = col_time(format = "%M:%S"),
                                  firstName = col_character(),
                                  lastName = col_character(),
                                  teamAbbrev = col_character(),
                                  eventDescription = col_character(),
                                  eventDetails = col_character(),
                                  hexValue = col_character(),
                                  teamName = col_character())) %>%
  mutate(startTime = as.double.POSIXlt(startTime),
         endTime = as.double.POSIXlt(endTime),
         name = str_c(firstName, lastName, sep = " "))
}

# today's games
gettodaygames<- function() {

  schedule2 <- nhlapi::nhl_schedule_today()

  schedule1 <- schedule2 %>%
    first() %>%
    extract2("dates") %>% extract2("games") %>% first() %>% tibble()

  scheduleToday <<- schedule1 %>%
    select(gamePk, teams.home.team.name,teams.home.score, teams.away.team.name, teams.away.score,
           gameDate, status.abstractGameState, gameType, content.link) %>%
    mutate(gameDate = force_tz(as_datetime(gameDate), tz = "UTC")) %>%
    mutate(gameDate = with_tz(gameDate, tzone = "America/Toronto"))
}



# FUNCTION CALLS ----

setenv()

# return a data frame with the games played/scheduled to this day
getschedule(2021, 10, datetoday - 1, "R")

# filter postponed games

game_address <- game_address %>%
  filter(status.codedGameState == 7)

# this retrives the data from the most recent played/scheduled game
getgamefeed(game_address$gamePk[length(game_address$gamePk)])
#getgamefeed(gameId)

# get boxscore stats
getboxscore(game_address$gamePk[length(game_address$gamePk)])
#getboxscore(gameId)

# create HDSC data frame from game_live_feed df
gethdsc()

getshiftsdouble(gameId)


# All games analyses ------------------------------------------------------



allLeafsGames <- bind_rows(purrr::map(game_address$gamePk, ~getgamefeed(.x)))

playoffs <- nhlapi::nhl_tournaments_playoffs(seasons = 2020)

teamsStats <- nhlapi::nhl_teams_stats(10, 2020)

standings <- nhlapi::nhl_standings(2020)

splitgid <- game_address %>%
  mutate(season1 = str_extract(gamePk, "^[0-9]{4}")) %>%
  mutate(teamGamesId = str_extract(gamePk, "[0-9]{4}$")) %>%
  mutate(seriesID = str_extract(teamGamesId, "^[0-9]{3}"))

# PLOTS
# Team Stats  -------------------------------------------------------------

colnames(teamStats) <- c("Goals", "PIM", "Shots", "PP%", "Power Play Goals", "Power Play Opportunities",
                "FO%", "Shots Blocked", "Takeaways", "Giveaways", "Hits", "teamTricode")

teamStats %>%
  reshape2::melt(id = "teamTricode") %>%
  as_tibble() %>%
  mutate(value_inv = ifelse(teamTricode == awayTeamTricode, as.double(value)*-1, as.double(value))) %>%
  mutate(variable = as_factor(variable)) %>%
  mutate(levels = case_when(variable == "Goals" ~ 11,
                            variable == "Power Play Goals" ~ 10,
                            variable == "Shots" ~ 9,
                            variable == "Shots Blocked" ~ 8,
                            variable == "Power Play Opportunities" ~ 7,
                            variable == "PP%" ~ 6,
                            variable == "PIM" ~ 5,
                            variable == "FO%" ~ 4,
                            variable == "Takeaways" ~ 3,
                            variable == "Giveaways" ~ 2,
                            variable == "Hits" ~ 1)) %>%
  #mutate(variable = fct_reorder(variable, levels)) %>%
  arrange(levels) %>%
  ggplot()+
  geom_col(aes(value_inv, fct_reorder(variable, levels), fill = teamTricode))+
  geom_text(aes(value_inv, variable, label = value, hjust = ifelse(teamTricode == awayTeamTricode, 1.1 , -0.2)))+
  hrbrthemes:: theme_ipsum_rc(grid = "Y")+
  theme(axis.text.x = element_blank())+
  scale_color_manual(name = "Team", values = teamcolors, aesthetics = c("color", "fill"))+
  xlab("")+
  ylab("")+
  labs(title = paste0("Game Statistics"),
       subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
       caption ="Chart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/game_cmp_boxscore_", gameId, "_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)



# Goalies Comp ------------------------------------------------------------

teamPlayersStats %>%
  filter(!is.na(goalieStats.timeOnIce)) %>%
  select(id, fullName, starts_with("goalieStats")) %>%
  mutate(goalieStats.timeOnIce = lubridate::ms(goalieStats.timeOnIce), goalieStats.timeOnIce = as.double(goalieStats.timeOnIce)) %>%
  mutate(decision = goalieStats.decision) %>%
  select( - goalieStats.decision) %>%
  pivot_longer(cols = starts_with("goalieStats"), names_to = "stats", values_to = "value") %>%
  mutate(stats = str_remove(stats, "goalieStats.")) %>%
  mutate(stats = str_replace(stats, "powerPlay", "Power Play ")) %>%
  mutate(stats = str_replace(stats, "shortHanded", "Short Handed ")) %>%
  mutate(stats = str_replace(stats, "even", "Even ")) %>%
  mutate(stats = str_replace(stats, "SavePercentage", "Save % ")) %>%
  mutate(stats = str_replace(stats, "ShotsAgainst", "Shots Against")) %>%
  mutate(stats = str_replace(stats, "timeOnIce", "Time On Ice")) %>%
  mutate(stats = str_replace(stats, "saves", "Saves")) %>%
  mutate(stats = str_replace(stats, "shots", "Shots")) %>%
  mutate(stats = str_replace(stats, "savePercentage", "Save %")) %>%
  filter(stats != "pim") %>%
  filter(stats != "goals") %>%
  filter(stats != "assists") %>%
  filter(stats != "Time On Ice") %>%
  mutate(value_inv = ifelse(decision == "L", value*-1, value)) %>%

  #mutate(value_inv = ifelse(stats == "Time On Ice", hms::as_hms(abs(as.stats)), stats))
  ggplot()+
  geom_col(aes(value_inv, stats, fill = fullName))+
  geom_text(aes(value_inv, stats, label = round(value, 2), hjust = ifelse(decision == "L", 1.1 , -0.2)))+
  hrbrthemes:: theme_ipsum_rc(grid = "Y")+
  theme(axis.text.x = element_blank())+
  scale_fill_brewer(palette = "Dark2", name = "Name")+
  #scale_color_manual(name = "Name", aesthetics = c("color", "fill"))+
  xlab("")+
  ylab("")+
  labs(title = paste0("Goalie Head-to-Head"),
       subtitle = paste0("Date: ", lubridate::date(datetoday), " | ", awayTeamTricode, " @ ", homeTeamTricode),
       caption ="Chart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/goalie_hth_", gameId, "_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)



# Corsi  ------------------------------------------------------------------

corsi <- game_live_feed %>%
  filter(result.event %in% c("Shot", "Missed Shot", "Blocked Shot", "Goal")) %>%
  select(result.event, result.description, result.secondaryType, about.period, about.periodType,
         about.periodTime, team.triCode, about.dateTime, homeTeamTricode, awayTeamTricode) %>%
  # mutate(home_team = if_else(team.triCode == homeTeamTricode, homeTeamTricode, awayTeamTricode))
  mutate(is_block = case_when(result.event == "Blocked Shot" ~ TRUE,
                                   TRUE ~ FALSE)) %>%
  #select(is_block, result.event, everything()) %>%
  mutate(event_tricode = if_else(is_block, ifelse(team.triCode == homeTeamTricode, awayTeamTricode, homeTeamTricode), team.triCode)) %>%

  type_convert(col_types = cols(.default = col_character(),
                                about.period = col_integer(),
                                about.periodTime = col_time(format = "%M:%S"))) %>%
  group_by(event_tricode) %>%
  mutate(gametime = about.periodTime + (about.period-1)* lubridate::dminutes(20),
         event = 1, corsi = cumsum(event), Team = event_tricode) %>%
  #filter(about.period ) %>%
  ungroup()

corsi %>%
  ggplot(aes(gametime, corsi, fill = Team, group = Team)) +
  geom_line(size = 1.2, aes(col = Team))+
  geom_point(shape = 21, size = ifelse(corsi$result.event == "Goal", 6, 0), alpha = ifelse(corsi$result.event == "Goal", 1, 0), col = "snow")+
  hrbrthemes::theme_ipsum_rc()+
  #theme(legend.title = element_text("Team"))+
  scale_x_time(labels = scales::time_format("%H:%M:%S"), limits = c(0,NA), breaks = c(0, 1200, 2400, 3600, 4800, 6000))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  scale_color_manual(values = teamcolors)+
  scale_fill_manual(values = teamcolors)+
  xlab("Time")+
  labs(title = paste0("All Situations Corsi"),
       #title = paste0("5v5 Corsi Events"),
       subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
       caption ="Goals are marked with dots\n\nChart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/corsi_5v5_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)

# corsi relative

  corsi %>%
  mutate(c_rel = if_else(Team == homeTeamTricode, event, event*-1)) %>%
  mutate(c_tot = cumsum(c_rel)) %>%
  ggplot(aes(gametime, c_tot))+
  geom_hline(yintercept = 0, color = "magenta", size = 0.7)+
  geom_step()+
  geom_point(aes(fill = Team), shape = 21, size = ifelse(corsi$result.event == "Goal", 4, 0), alpha = ifelse(corsi$result.event == "Goal", 1, 0), col = "snow")+
  geom_text(aes(x = 250, y = max(c_tot), label = homeTeam, family = "Roboto Condensed"))+
  geom_text(aes(x = 250, y = min(c_tot), label = awayTeam, family = "Roboto Condensed"))+
  hrbrthemes::theme_ipsum_rc()+
  theme(legend.title = element_text("Team"))+
  scale_x_time(labels = scales::time_format("%H:%M:%S"), limits = c(0,NA))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  guides(fill = guide_legend(override.aes = list(size = 4)))+
  scale_color_manual(values = teamcolors)+
  scale_fill_manual(values = teamcolors)+
  xlab("Time")+
  ylab("Relative Corsi")+
  labs(#title = paste0("All Situations Relative Corsi"),
       title = paste0("Relative Corsi Events"),
       subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
       caption ="Goals are marked with dots\n\nChart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/corsi_rel_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)

# Shots On Goal Location --------------------------------------------------

shots <- game_live_feed %>%
    #filter(about.ordinalNum %in% prd)
    filter(result.eventTypeId == "SHOT" | result.eventTypeId == "GOAL") %>%
    select(coordinates.x, coordinates.y, team.name, result.description, result.secondaryType, result.eventTypeId,
           about.period, about.ordinalNum, team.triCode, about.eventIdx, about.periodTime) %>%
    mutate(coordinates.x = ifelse(about.period %in% c(2,4,6),  coordinates.x * -1, coordinates.x),
           coordinates.y = ifelse(about.period %in% c(2,4,6),  coordinates.y * -1, coordinates.y)) %>%
    mutate(period_time = as.double(lubridate::ms(about.periodTime)))

# this is to plot the shots on goal on the rink ( yeah i know its a mess)
rink + geom_point(data = shots, aes(coordinates.x, coordinates.y, color = team.triCode),
                        shape = ifelse(shots$result.eventTypeId %in% c("GOAL"), 19, 18),
                        size = ifelse(shots$result.eventTypeId %in% c("GOAL"), 6, 5),
                        alpha = ifelse(shots$result.eventTypeId %in% c("GOAL"), 0.8, 0.7))+
      scale_color_manual(name = "", values = teamcolors, aesthetics = c("color"))+
      ggforce::theme_no_axes()+
      hrbrthemes::theme_ipsum_rc(grid = F)+
      #theme(legend.title = element_text(), legend.position = "top", axis.text.x = element_blank(),
      #      axis.text.y = element_blank(), plot.background = element_rect(fill = "grey12", colour = "grey23"),
      #      panel.background = element_rect(fill = "grey95", colour = "grey95"),
      #      text = element_text(family = "Roboto Condensed", size = 16)) +
      theme(legend.title = element_text(), legend.position = "top",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()
            )+
      guides(color = guide_legend(override.aes = list(size = 5)))+
      ylab("")+
      xlab("")+
      labs(title = paste0("Location of Shots on Goal"),
      subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
          caption ="Chart by: u/only-locals | Data: NHLAPI")
      #labs(title = paste0("Shots on Goal of the 3 Games Span"),
      #     subtitle = paste0("Date: 2021-02-27 - 2021-03-03  | Opponent: ", opponent),
      #     caption ="Chart by: u/only-locals | Data: NHLAPI")5

ggsave(
  paste0("plots/shotsongoal_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)


# HDSC --------------------------------------------------------------------

high_danger_fenwick1 %>%
    filter(shot_score >= 2) %>%
    mutate(shot = cut(shot_score, breaks = c(0,4.5), labels = c("Scoring Chances"))) %>%
    mutate(shot_hd = cut(shot_score, breaks = c(2.5,4.5), labels = c("High Danger Scoring Chances"))) %>%
    #group_by(team.triCode, shot_score) %>%
    #summarise(n())
    ggplot()+
    geom_bar(aes(shot, fill = team.triCode), color = "snow", position = "dodge")+
    geom_bar(aes(shot_hd, fill = team.triCode), color = "snow", position = "dodge")+
    hrbrthemes::theme_ipsum_rc(grid = "Y,y")+
    scale_color_manual(name = "", values = teamcolors, aesthetics = c("fill"))+
    xlab("Shot Danger")+
    ylab("Opportunities")+
    labs(title = paste0("How dangerous were the shots"),
      subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode, " | All Situations"),
          caption ="Chart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/danger_chances_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)

# ---

rink +  geom_polygon(data = da_coords, aes(x = x, y = y), color = "orange", alpha = 0.5, fill = "transparent")+
        geom_polygon(data = hda_coords, aes(x = x, y = y), color = "firebrick", alpha = 0.5, fill = "transparent")+
        geom_point(data = high_danger_fenwick3, aes(coord_x, coord_y, color = team.triCode),
                        shape = ifelse(high_danger_fenwick3$result.eventTypeId %in% c("GOAL"), 19, 18),
                        size = ifelse(high_danger_fenwick3$result.eventTypeId %in% c("GOAL"), 6, 5),
                        alpha = ifelse(high_danger_fenwick3$result.eventTypeId %in% c("GOAL"), 0.7, 0.8))+

      scale_color_manual(name = "", values = teamcolors, aesthetics = c("color"))+
      ggforce::theme_no_axes()+
      hrbrthemes::theme_ipsum_rc(grid = F)+
      coord_cartesian(xlim = c(0,100))+
      #theme(legend.title = element_text(), legend.position = "top", axis.text.x = element_blank(),
      #      axis.text.y = element_blank(), plot.background = element_rect(fill = "grey12", colour = "grey23"),
      #      panel.background = element_rect(fill = "grey95", colour = "grey95"),
      #      text = element_text(family = "Roboto Condensed", size = 16)) +
      theme(legend.title = element_text(), legend.position = "top",
            axis.text.x = element_blank(),
            axis.text.y = element_blank()
            )+
      guides(color = guide_legend(override.aes = list(size = 5)))+
      ylab("")+
      xlab("")+
      labs(title = paste0("Location of High Danger Scoring Chances"),
      subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
          caption ="Chart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/high_danger_location_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)



# Shifts data wrangling ---------------------------------------------------



# period to filter - 1st - 2nd - 3rd - OT
# take care to match this with the filter
teamsRoster <- teamPlayersStats %>%
  select(id, fullName, jerseyNumber, type, currentTeam.name)

gameRoster <- left_join(gameRoster, teamsRoster, by = "id")


shiftRoster <- left_join(shifts, teamsRoster, by = c("playerId" = "id", "name" = "fullName"))

# verify if there's duplicate rows
shiftRoster <- shiftRoster %>%
  distinct()


lines <- rev(c("11", "34", "16",
               "12", "71", "88",
               "65", "15", "24",
               "97", "19", "47",
               "44", "78",
               "8", "3",
               "23", "22",
               "31", "36", "30", "33"))

# game duration ---

gameDuration <- game_live_feed %>%
  mutate(about.periodTime = parse_time(about.periodTime, format = "%M:%S"),
         about.periodTimeRemaining = parse_time(about.periodTimeRemaining, format = "%M:%S")) %>%
  mutate(periodTime = as.double(about.periodTime)) %>%
  group_by(about.period) %>%
  filter(periodTime == max(periodTime) & result.eventTypeId == "PERIOD_OFFICIAL") %>%
  ungroup() %>%
  mutate(gameDuration = cumsum(periodTime)) %>%
  select(gameDuration) %>%
  filter(gameDuration == max(gameDuration)) %>%
  as_vector()


gameSeconds <- tibble(seconds = seq(1:game_duration))

# player id data ---


idData <- teamPlayersStats %>%
  select(id, fullName, primaryNumber, currentTeam.id, currentTeam.name, type, abbreviation) %>%
  rename(jerseyNumber = primaryNumber,
         teamId = currentTeam.id,
         teamFullName = currentTeam.name,
         positionName = type,
         positionCode = abbreviation)


# ice time ---
# there's and error on ice time on even strength and total
iceTime <- teamPlayersStats %>%
  select(id, fullName, primaryNumber, currentTeam.id, currentTeam.name, type, abbreviation, skaterStats.timeOnIce, skaterStats.evenTimeOnIce, goalieStats.timeOnIce) %>%
  mutate(skaterStats.evenTimeOnIce = if_else(abbreviation == "G", goalieStats.timeOnIce, skaterStats.evenTimeOnIce)) %>%
  mutate(skaterStats.evenTimeOnIce = str_pad(skaterStats.evenTimeOnIce, width = 5, side = "left", pad = "0")) %>%
  mutate(skaterStats.evenTimeOnIce = if_else(is.na(skaterStats.evenTimeOnIce), "00:00", skaterStats.evenTimeOnIce)) %>%
  mutate(skaterStats.evenTimeOnIce = ms(skaterStats.evenTimeOnIce)) %>%
  mutate(toi_even = as.double(skaterStats.evenTimeOnIce)) %>%
  filter(currentTeam.name == "Toronto Maple Leafs") %>%
  arrange(type, -toi)

# ---


playersDf <- crossing(gameRoster$id, gameRoster$id, .name_repair = "universal") %>%
             rename(id = gameRoster.id...1, id2 = gameRoster.id...2)



playersOnIce <- shiftRoster %>%
  #selecting some columns
  select(playerId, eventNumber, shiftNumber, period, startTime, endTime,
         name, jerseyNumber, teamAbbrev, type, currentTeam.name) %>%
  # renaming to keep it consistent
  rename(teamFullName = currentTeam.name,
         positionName = type) %>%
  # new columns for the time to be continuous all game
  mutate(gameStartTime = startTime + (period-1)* 1200,
           gameEndTime = endTime + (period-1)* 1200) %>%
  # creating a list of the shifts taken for each player
  group_by(r = row_number()) %>%
  mutate(custom = list((gameStartTime+1):gameEndTime)) %>%
  ungroup() %>%
  select(-r) %>%
  unnest(cols = c(custom)) %>%
  select(playerId, custom) %>%
  group_by(playerId) %>%
  nest()


playersDf1 <- left_join(playersDf, playersOnIce, by = c("id" = "playerId"))
playersDf2 <- left_join(playersDf1, playersOnIce, by = c("id" = "playerId"))

# ====
df1 <- playersDf2$data[[1]]
df2 <- playersDf2$data2[[2]]

length(df1[[1]])
length(df2[[1]])

inter <- dplyr::intersect(df1, df2)

length(inter[[1]])
# ====

comp_fun <- function(df1, df2){
  inter <- dplyr::intersect(df1, df2)
}

playersDf3 <- playersDf2 %>%
  mutate(comp = map2(data.x, data.y, comp_fun)) %>%
  mutate(time = map(comp, dim)) %>%
  mutate(s = map(time, 1, 1)) %>%
  select(id, id2, s)


playerDf4 <- left_join(playersDf3, gameRoster)
playerDf5 <- left_join(playerDf4, gameRoster, by = c("id2" = "id"))


playerDf5 %>%
  filter(currentTeam.name.x == "Toronto Maple Leafs", currentTeam.name.y == "Toronto Maple Leafs") %>%
  unnest(cols = c(s)) %>%
  ggplot(aes( fct_reorder2(jerseyNumber.x, s, type.x), fct_reorder2(jerseyNumber.y, s, type.y), fill = s))+
    geom_tile()

 playerDf5 %>%
  filter(currentTeam.name.x == "Toronto Maple Leafs", currentTeam.name.y == "Toronto Maple Leafs") %>%
  unnest(cols = c(s)) %>%
  #filter(id == id2) %>%
  filter(type.x != "Goalie", type.y != "Goalie") %>%
  filter(s >= 1) %>%
  mutate(toi = hms::as_hms(s)) %>%
  mutate(pos_1 = factor(type.x, levels = c("Forward", "Defenseman")),
         pos_2 = factor(type.y, levels = c("Forward", "Defenseman"))) %>%
  #filter(id != id2) %>%
  #mutate(sum = as.integer(id) + as.integer(id2), dup = duplicated(sum)) %>%
  #filter(dup != T) %>%

  arrange(pos_1, pos_2, desc(toi)) %>%
  mutate(jerseyNumber.x = as.integer(jerseyNumber.x), jerseyNumber.y = as.integer(jerseyNumber.y)) %>%
  mutate(r_id = row_number(), jerseyNumber.x = factor(jerseyNumber.x, levels = jerseyNumber.x),  jerseyNumber.y = factor(jerseyNumber.y)) %>%
 # group_by(id) %>%
  mutate(p_toi = s/60) %>%

  ggplot(aes(jerseyNumber.x, jerseyNumber.y, fill = p_toi))+
    geom_tile()

playerDf5 %>%
  unnest(cols = c(s)) %>%
  mutate(type.x = as_factor(type.x), toi = s/60) %>%
  filter(currentTeam.name.x == "Toronto Maple Leafs", currentTeam.name.y == "Toronto Maple Leafs") %>%
  filter(type.x == "Forward" , type.y == "Forward") %>%
  filter(id != id2) %>%

ggplot(aes(x = factor(jerseyNumber.x),
  y = toi, color = jerseyNumber.y)) +
  geom_jitter(width = .075, alpha = .9) +
  facet_wrap(~type.x, scales = "free_x")




prd <- c(1)

lineChanges <- shifts_roster %>%
  #filter(period == prd) %>%
  filter(teamAbbrev == "TOR") %>%
  filter(eventNumber <10 | eventNumber > 100) %>%
  #filter(type == "Forward") %>%
  #mutate(name = factor(name)) %>%
  select(eventNumber, jerseyNumber) %>%
  group_by(eventNumber) %>%
  mutate(players = paste0("player.", row_number())) %>%
  ungroup() %>%
  pivot_wider(names_from = "players", values_from = "jerseyNumber") %>%
  arrange(eventNumber)

  filter(!is.na(player.3))
  select(-player.4) %>%
  mutate(line = str_c(player.1, player.2, player.3, sep = "-")) %>%
  mutate(line_f = as_factor(line))

  fct_count(test$line_f) %>%
    arrange(desc(n)) %>%
    filter(n > 1)

  #mutate(name = fct_reorder(name, unclass(position.type))) %>%
  #mutate(name = fct_reorder(name, as.double(jerseyNumber))) %>%
  mutate(name = fct_relevel(name, linesCombination))

test <- shifts_roster %>%
    #filter(period == prd) %>%
    mutate(gameStartTime = startTime + (period-1)* 1200,
           gameEndTime = endTime + (period-1)* 1200) %>%
    group_by(name) %>%
    mutate(duration1  = as.double(duration), toi = cumsum(duration1), t_toi = max(toi)) %>%
    ungroup() %>%
    mutate(name = as.factor(name), name = fct_reorder(name, t_toi)) %>%
    select(name, toi, t_toi, gameStartTime, gameEndTime, everything()) %>%
    arrange(eventNumber)

test %>%
ggplot() +
   geom_segment(aes(x=name ,xend=name, y=gameStartTime, yend=gameEndTime, col = teamAbbrev), lwd = 0.9)+
   #geom_point(aes(name, startTime, color = hexValue), shape = 21, size = 3)+
   #geom_point(aes(name, endTime, color = hexValue), shape = 20, size = 3)+

   geom_hline(data = shifts_roster %>% filter(typeCode == 505, period == prd),
              aes(yintercept = startTime), lty = 2, color = c("#e7298a"))+

  # ggrepel::geom_label_repel(data = shifts_roster %>% filter(typeCode == 505, period == prd),
  #              aes(label = paste0(teamAbbrev, " - ",eventDescription, "\n", name),
  #                  x = 22, y = startTime),  lineheight = 0.8, box.padding = 0.2)+

   coord_flip()+
   hrbrthemes::theme_ipsum_rc(grid = "Y, X")+
   #theme(legend.position = "none")+
   scale_y_time(labels = scales::time_format("%H:%M:%S"))+
   #guides( color = guide_legend(title = "Position", override.aes = list(size = 4)))+
   #scale_color_manual(values = c("Defenseman" = "#1b9e77", "Forward" = "#d95f02", "Goalie" = "#7570b3"))+
   scale_color_manual(values = teamcolors)+
   ylab("Shift Duration")+
   xlab("")+
   labs(title = paste0("Shifts in the ", nombre::nom_ord(prd, cardinal = F), " Period"),
        #title = paste0("Shifts in the OT"),
        #subtitle = paste0("Date: ", lubridate::date(gameday), " | Opponent: ", opponent),
        caption ="Chart by: u/only-locals | Data: NHLAPI")


ggsave(
  paste0("plots/shifts_on_",prd,"_",gameid,"_", lubridate::date(gameday),"_",oppo_tricode,".png"),
  width = 9,
  height = 8,
  units = "in",
  dpi = 600)






# Time on Ice chart -------------------------------------------------------


teamPlayersStats %>%
  filter(currentTeam.id == 10) %>%
  filter(!is.na(skaterStats.evenTimeOnIce)) %>%
  select(fullName, skaterStats.timeOnIce, currentTeam.id, type,
         skaterStats.evenTimeOnIce, skaterStats.powerPlayTimeOnIce, skaterStats.shortHandedTimeOnIce) %>%
  mutate(skaterStats.evenTimeOnIce = lubridate::ms(skaterStats.evenTimeOnIce)) %>%
  mutate(skaterStats.powerPlayTimeOnIce = lubridate::ms(skaterStats.powerPlayTimeOnIce)) %>%
  mutate(skaterStats.shortHandedTimeOnIce = lubridate::ms(skaterStats.shortHandedTimeOnIce)) %>%
  mutate(skaterStats.timeOnIce = lubridate::ms(skaterStats.timeOnIce)) %>%
  pivot_longer(cols = c("skaterStats.evenTimeOnIce", "skaterStats.powerPlayTimeOnIce",
                        "skaterStats.shortHandedTimeOnIce")) %>%
  mutate(name = str_remove(name, "skaterStats.")) %>%
  mutate(dvalue = as.double(value), toi = as.double(skaterStats.timeOnIce), fullName = as.factor(fullName)) %>%
  ggplot()+
  geom_bar(stat = "identity", position = position_stack(reverse = T), na.rm = T,
           aes(dvalue, fct_reorder(fullName, toi), fill = name))+
  scale_x_time(labels = scales::time_format("%M:%S"))+
  hrbrthemes::theme_ipsum_rc()+
  scale_fill_brewer(palette = "Dark2", name = "Strength")+
  ylab("Name")+
  xlab("Time on Ice")+
  labs(title = paste0("Time on Ice"),
        subtitle = paste0("Date: ", lubridate::date(gameDate), " | ", awayTeamTricode, " @ ", homeTeamTricode),
        caption ="Only skaters\nChart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/toi_p_strength_",gameId,"_", lubridate::date(gameDate),"_.png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)




# Time on Ice old chart ---------------------------------------------------


shifts_roster %>%
  filter(teamAbbrev == "TOR", ! position.type == "Goalie") %>%
  #mutate(name = factor(name)) %>%
  #mutate(name = fct_reorder(name, unclass(position.code))) %>%
  group_by(name) %>%
  summarise(toi = sum(duration, na.rm = T), position.type = position.type, .groups = "drop_last") %>%
  unique() %>%
  arrange(position.type, desc(toi)) %>%
  mutate(rk = row_number(),   name = fct_rev(fct_reorder(name, rk)),
         toi_str = hms::as_hms(toi), toi_str2 = as.character(toi_str),
         toi_str3 = str_replace(toi_str2, "^[0-9]{2}:", ""),
         #toi_str3 = toi_str2
         ) %>%

ggplot(aes(aes(x=name, y=duration))) +
   geom_segment(aes(x=name ,xend=name, y=0, yend=toi, col = position.type), lwd = 1)+
   geom_point(aes(name, toi, col = position.type), shape = 20, size = 6)+
   geom_text(aes(label = toi_str3, x = name, y = toi), col = "grey9", nudge_y = 80)+
   coord_flip()+
   hrbrthemes::theme_ipsum_rc()+
   theme(axis.text.x = element_blank())+
   #scale_y_time(labels = scales::time_format("%H:%M:%S"))+
   guides( color = guide_legend(title = "Position", override.aes = list(size = 4)))+
   #scale_color_viridis_d(begin = 0.3, end = 0.8)+
   scale_color_manual(values = c("Defenseman" = "#1b9e77", "Forward" = "#d95f02"))+
   ylab("Time on Ice")+
   xlab("")+
   labs(title = paste0("Time on Ice"), subtitle = paste0("Date: ", lubridate::date(gameday),
                                                                           " | Opponent: ", opponent),
        caption ="Only skaters \n\nChart by: u/only-locals | Data: NHLAPI")

ggsave(
  paste0("plots/toi_",gameid,"_", lubridate::date(gameday),"_",oppo_tricode,".png"),
  width = 11,
  height = 8,
  units = "in",
  dpi = 600)
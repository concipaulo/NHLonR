library(magrittr)
library(tidyverse)
library(lubridate)
library(reticulate)


setenv <- function(){

teamcolors <<- c("ANA" = "#111111","ARI" = "#b43143","BOS" = "#c98e03","BUF" = "#00224d","CGY" = "#ce1126",
                "CAR" = "#76232F","CHI" = "#ec132c","COL" = "#6f263d",
                "CBJ" = "#041e42","DAL" = "#008053","DET" = "#a50d27","EDM" = "#fc4c02","FLA" = "#b9975b",
                "LAK" = "#a2aaad","MIN" = "#154734","MTL" = "#851425",
                "NSH" = "#ffb81c","NJD" = "#a50d27","NYI" = "#003087","NYR" = "#0049e6","OTT" = "#c69214",
                "PHI" = "#fa4616","PIT" = "#ffb81c","SJS" = "#008599",
                "STL" = "#002f87","TBL" = "#002d80","TOR" = "#002d80","VAN" = "#008852","VGK" = "#3f4e5a",
                "WSH" = "#ec1337","WPG" = "#858f93")

datehere <- force_tz(Sys.time(), tzone = "America/Sao_Paulo")
datetor <- with_tz(datehere, tzone = "America/Toronto")
datetoday <<- date(datetor)

season <<- 2020L

}

gettodaygames<- function() {

  schedule2 <- nhlapi::nhl_schedule_today()

  schedule1 <- schedule2 %>%
    first() %>%
    extract2("dates") %>% extract2("games") %>% first() %>% tibble()

  schedule <<- schedule1 %>%
    select(gamePk, teams.home.team.name,teams.home.score, teams.away.team.name, teams.away.score,
           gameDate, status.abstractGameState, gameType, content.link) %>%
    mutate(gameDate = force_tz(as_datetime(gameDate), tz = "UTC")) %>%
    mutate(gameDate = with_tz(gameDate, tzone = "America/Toronto"))
}

getschedule <- function(season, team, date){

schedule <- nhlapi:::nhl_url_schedule(season, team, "2021-01-13", date, "R")

game_ids_list <- nhlapi::nhl_get_data(schedule)

game_list <- game_ids_list %>% first() %>% magrittr::extract2("dates") %>% extract2("games")
game_address <- plyr::rbind.fill(game_list)

game_address <<- game_address %>%
     mutate(gameDate = force_tz(as_datetime(gameDate), tz = "UTC")) %>%
     mutate(gameDate = with_tz(gameDate, tzone = "America/Toronto"))

}

getroster<- function(teamid, season){

  roster <- nhlapi::nhl_teams_rosters(teamid, season)
  roster_df <- roster %>%
  magrittr::extract2("roster.roster") %>%
  first() %>%
  tibble()

# ----
# add player manually, there's some cases where nhlapi dont have the right roster
# need to find the correct player id
statnames <- colnames(roster_df)

# player1 <- nhlapi::nhl_players("Alex Galchenyuk")
#
# player <- player1 %>%
#   select(primaryNumber, id, fullName, link, primaryPosition.code, primaryPosition.name, primaryPosition.type, primaryPosition.abbreviation)
#
# colnames(player) <- statnames
#
# roster_df <- add_row(roster_df, player)

# ----
roster <<- roster_df %>%
  select(person.id, person.fullName, jerseyNumber, position.code, position.abbreviation, position.name, position.type) %>%
  mutate(position.code = factor(position.code , levels = c("G", "D", "R", "L", "C")),
         position.type = factor(position.type , levels = c("Goalie", "Defenseman", "Forward")))
}

getgamefeed <- function(gameid){

  gamefeed <<- nhlapi::nhl_games_feed(gameid)

  game_live_feed <- gamefeed %>% first() %>%
   magrittr::extract2("liveData") %>%
   magrittr::extract2("plays") %>%
   magrittr::extract2("allPlays") %>%
   as_tibble()

  game_live_feed <- tidyr::unnest_wider(game_live_feed, players)
  game_live_feed <- tidyr::unnest_wider(game_live_feed, player.fullName, names_sep = ".")
  game_live_feed <- tidyr::unnest_wider(game_live_feed, playerType, names_sep = ".")
  game_live_feed <<- tidyr::unnest_wider(game_live_feed, player.id, names_sep = ".")
}

getvars <- function(){

venue <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("venue") %>%
  extract2("name")

gameday <<- gamefeed %>% first() %>%
   magrittr::extract2("gameData") %>%
   magrittr::extract2("datetime") %>%
   magrittr::extract2("dateTime") %>%
   lubridate::as_datetime(tz = "GMT") %>%
   lubridate::with_tz(tzone = "America/New_York")

home_team <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("name")

away_team <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("name")

away_team_id <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("id")

home_team_id <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("id")

home_team_tricode <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("home") %>%
  extract2("triCode")

away_team_tricode <<- gamefeed %>% first() %>%
  extract2("gameData") %>%
  extract2("teams") %>%
  extract2("away") %>%
  extract2("triCode")

}

# Some fixed variables
setenv()

# Get teams Schedule
getschedule(2020,10,datetoday)

# Set game id
gameid <- game_address$gamePk[length(game_address$gamePk)-1]

#Get game data
getgamefeed(gameid)

getvars()

# After this code 2 files were created, the names are random so you have to import manually
py_run_string("import hockey_scraper")
py_run_string(paste0("hockey_scraper.scrape_games([",gameid,"], True)"))




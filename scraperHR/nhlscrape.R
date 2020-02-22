# package nhlscrape ----
library(nhlscrape)


SetDbPath(example = TRUE)
AddAllTeamsDb()
idtor<-GetTeamId("TOR")
AddTeamRoster(10,20192020)
GetDbPath()

team_id <- GetTeamId("TOR")
gids <- GetGameIdRange(team_id, "2019-10-02", "2020-2-21")

# Add games
AddGameEvents(gids)

# Get stats for player
# Tavares
player_id <- GetPlayerId("John Tavares")

stats <- GetPlayerStats(player_id, gids, team_id)
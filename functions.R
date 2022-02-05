library(tidyverse)
library(rvest)

# Webscraping function that returns cap_hit, costs per point, etc
# this is scrapped from capfriendly.com

getcosts <- function(team){
webpage2 <- read_html(paste0("https://www.capfriendly.com/cost-per-point/2021/season/all/all/all/goals/desc/default/", team))

names2 <- webpage2 %>%
  html_nodes("th") %>%
  html_text()

names2 <- names2[! names2 %in% c("P. BONUSES")]

data2 <- webpage2 %>%
  html_nodes("td") %>%
  html_text()


data_df <- matrix(
  data = data2,
  ncol = 19,
  byrow = T,
  dimnames = list(NULL, names2)
)

cls <- cols(
  number = col_integer(),
  player = col_character(),
  pos = col_character(),
  age = col_integer(),
  team = col_character(),
  type = col_character(),
  expiry = col_character(),
  cap_hit = col_number(),
  gp = col_integer(),
  sh = col_integer(),
  g = col_integer(),
  a = col_integer(),
  p = col_integer(),
  toi = col_time(format = "%M:%S"),
  sh_2 = col_number(),
  g_2 = col_number(),
  a_2 = col_number(),
  p_2 = col_number(),
  min = col_number()
)



costs <<- data_df %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(across(where(is.character), ~ str_remove(.x, "(^\\$)"))) %>%
  type_convert(col_types = cls) %>%
  separate(player, sep = ",", into = c("last_name", "first_name")) %>%
  unite(first_name, last_name, col = "name", sep =  " ") %>%
  separate(pos, sep = ",", into = c('pos', "pos1"))

}

# Webscraping that return the financial summary for 5 years
# is scraped from spotrac.com

getfinancialsummary <- function(teamname){
webpage <- read_html(paste0("https://www.spotrac.com/nhl/", teamname, "/"))


data <- webpage %>%
  html_nodes("td") %>%
  html_text()

titles <- webpage %>%
  html_nodes("th") %>%
  html_text()

# subset titles, subject to site layout
titles <- titles[42:length(titles)]
titles[1] <- "names"

# subset data, subject to site layout
data1 <- data[49:length(data)]

data_1 <- matrix(
  data = data1,
  ncol = 7,
  byrow = T,
  dimnames = list(NULL, titles))

cls <- cols(
  names = col_character(),
  pos = col_character(),
  x2020 = col_number(),
  x2021 = col_number(),
  x2022 = col_number(),
  x2023 = col_number(),
  x2024 = col_number()
)

financial_summary <<- data_1 %>%
  as_tibble() %>%
  janitor::clean_names() %>%
    #str_remove_all("[[:punct:]]") %>%
  #str_trim(side = c("both"))
  mutate(across(where(is.character), ~ str_remove(.x, "(^\\$)"))) %>%
  mutate(across(where(is.character), ~ str_trim(.x, side = c("both")))) %>%
  mutate(across(where(is.character), ~str_replace(.x, "0UFA", "-1"))) %>%
  mutate(across(where(is.character), ~str_replace(.x, "0RFA", "-2"))) %>%
  type_convert(col_types = cls)
}

# Jerseys data from hockey reference

getjerseys <- function(tricode){

tricode <- "MTL"
webpage1 <- read_html(paste0("https://www.hockey-reference.com/teams/", tricode,"/numbers.html"))

data1 <- webpage1 %>%
  html_nodes("td") %>%
  html_text()

table1 <- webpage1 %>%
  html_table()

numbers1 <- webpage1 %>%
  html_nodes("caption") %>%
  html_text()

retired1 <- webpage1 %>%
  html_nodes("svg") %>%
  html_text()

retired2 <- as.numeric(retired1)
retired2 <- retired2[!is.na(retired2)]

names(table1) <- numbers1

# probably can be done with bind_rows from dplyr
jerseys <- as.data.frame(do.call(rbind, table1))
jerseys1 <- rownames_to_column(jerseys)


jerseys_df <<- jerseys1 %>%
  separate(rowname, sep = "\\.", into = c("number", "order"), fill = "right") %>%
  replace_na(list(order = 1)) %>%
  separate(X1, sep = "(\\s(?=\\d))", into = c("name", "year"), fill = "right") %>%
  separate(year, sep = "-", into = c("year_start", "year_end"), fill = "right") %>%
  mutate(year_end = if_else(is.na(year_end), year_start, year_end)) %>%
  type_convert(cols(
                    number = col_factor(),
                    order = col_integer(),
                    name = col_character(),
                    year_start = col_integer(),
                    year_end = col_integer())) %>%
  mutate(retired = if_else(number %in% retired2, "YES", "NO"))

possible <- seq(1,99,1)

never_used <<- setdiff(possible, numbers1)

print(paste0("Never used jersey number: ", length(never_used)))
print(never_used)

}

# Set variables for the other functions ----

setenv <- function(){
# named vector
teamcolors <<- c("ANA" = "#111111","ARI" = "#b43143","BOS" = "#c98e03","BUF" = "#00224d","CGY" = "#ce1126",
                "CAR" = "#76232F","CHI" = "#ec132c","COL" = "#6f263d",
                "CBJ" = "#041e42","DAL" = "#008053","DET" = "#a50d27","EDM" = "#fc4c02","FLA" = "#b9975b",
                "LAK" = "#a2aaad","MIN" = "#154734","MTL" = "#851425",
                "NSH" = "#ffb81c","NJD" = "#a50d27","NYI" = "#003087","NYR" = "#0049e6","OTT" = "#c69214",
                "PHI" = "#fa4616","PIT" = "#ffb81c","SJS" = "#008599",
                "STL" = "#002f87","TBL" = "#002d80","TOR" = "#00205b","VAN" = "#008852","VGK" = "#3f4e5a",
                "WSH" = "#ec1337","WPG" = "#031630")

nhlteams <<- c("anaheim-ducks","phoenix-coyotes", "boston-bruins", "buffalo-sabres",
             "calgary-flames", "carolina-hurricanes", "chicago-blackhawks", "colorado-avalanche",
             "columbus-blue-jackets", "dallas-stars", "detroit-red-wings", "edmonton-oilers",
             "florida-panthers", "los-angeles-kings", "minnesota-wild", "montreal-canadiens",
             "nashville-predators", "new-jersey-devils", "new-york-islanders", "new-york-rangers",
             "ottawa-senators", "philadelphia-flyers", "pittsburgh-penguins", "san-jose-sharks",
             "st-louis-blues", "tampa-bay-lightning", "toronto-maple-leafs", "vancouver-canucks",
             "vegas-golden-knights", "washington-capitals", "winnipeg-jets")

season_start <<- as.Date("2021-01-13")

teamsfiles <- str_to_title(str_replace_all(nhlteams, "[:punct:]", " "))
teamsfiles <- str_replace_all(teamsfiles, "Phoenix", "Arizona")
teamsfiles <<- str_replace(teamsfiles, "St ", "St. ")

teams_abrv <- read.csv("~/NHLonR/teams_abrev2.csv", stringsAsFactors = F)
teams_abrv <<- as_tibble(teams_abrv)
datetoday <<- Sys.Date()
reference <<- sprintf("Source: www.quanthockey.com, %s", datetoday)

}

# This is from the team Analysis =============================================================

# Retrieving data from website and save a file on disk ----

getteamdata <- function(ii, jj = ii){

  index <- rep(ii:jj, 1)

  for (i in index) {
    options(warn=-1)
    message(paste0("Retriving data from ", nhlteams[i]),"\r",appendLF=T)
    Sys.sleep(2) # waiting time so that the website doesn't blck your ip
webpage <-
      read_html(
        paste0("https://www.quanthockey.com/nhl/team-game-logs/", nhlteams[i],"-2020-21-nhl-game-log.html"))

    # Teams data
    webdata <- webpage %>%
      html_nodes("td") %>%
      html_text()
    teamname <- webpage %>%
      html_nodes("title") %>%
      html_text()
    titles <-  webpage %>%
      html_nodes("th") %>%
      html_text()

    #cleaning stats names
    stat_team <- gsub("%", ".", titles[9:35])
    stat_team <- gsub("-", "", stat_team)

    # extracting team name
    teamname <- str_remove(teamname, "( Ga.*)")
    teamname <<- teamname

    # Creating a data frame
    dfs <- matrix(
      webdata,
      ncol = 27,
      byrow = T,
      dimnames = list(NULL, stat_team)
    )

    dfs <- as_tibble(dfs)

    dfs <- rename(dfs, Gms = Rk)
    dfs$Gms <- as.numeric(as.character(dfs$Gms))
    dfs$Gms <- rev(dfs$Gms)
    dfs$Date <- as.Date(dfs$Date, format = "%Y-%m-%d")
    dfs$GD <- as.numeric(as.character(dfs$GD))
    dfs$PDO <- as.numeric(sub("%","",dfs$PDO))/100
    dfs$PDOA <- as.numeric(sub("%","",dfs$PDOA))/100
    dfs$SH. <- as.numeric(sub("%","",dfs$SH.))/100
    dfs$SH.A <- as.numeric(sub("%","",dfs$SH.A))/100
    dfs$FO. <- as.numeric(sub("%","",dfs$FO.))/100
    dfs$SV. <- as.numeric(sub("%","",dfs$SV.))/100
    dfs$SV.A <- as.numeric(sub("%","",dfs$SV.A))/100
    dfs$name <- teamname

    dfs <- dfs %>%
      select(name, everything()) # nifty way of reordering columns

    dfs <- left_join(dfs, teams_abrv, by = c("name" = "Teams"))

    # Counting points

    dfs$PTS <- 0
    sum = 0
    for (i in nrow(dfs):1) {
      if (dfs$Result[i] == "Win" | dfs$Result[i] == "OT Win"| dfs$Result[i] == "SO Win") {
        sum = sum + 2
        dfs$PTS[i] = sum
      }
      if(dfs$Result[i] == "OT Loss" | dfs$Result[i] == "SO Loss"){
        sum = sum + 1
        dfs$PTS[i] = sum
      }
      if(dfs$Result[i] == "Loss" ){
        dfs$PTS[i] = sum
      }
    }

    dfs$Result <- as.factor(str_trim(as.character(dfs$Result)))
    dfs$overall <- NA
    dfs$overall <- fct_collapse(dfs$Result, Win = c("Win", "OT Win", "SO Win"), Loss = c("Loss"), OL = c("SO Loss", "OT Loss"))
    dfs$WOL <- ifelse(dfs$GD > 0, "Win", "Loss")
    #writing file on the disk
    write.csv(dfs, file = paste0("Data/",teamname,datetoday,".csv"), row.names = F)

  }
}

# Retrieve standings ----

getstandings <- function(v = "l", sd = "202021", ed = sd){


  # v is the variable that control if the standings is per 'd'ivision or 'l'eague
  #sd = start date and ed = end date, as it is will return the most recent but can be passed any date
  # from the 2020-21 season in the following order %Y-%m-%d.

  webpage <-
      read_html(
        paste0("https://www.quanthockey.com/nhl/standings/standings.php?v=",v,"&s=pts&so=desc&ha=all&sd=",sd,"&ed=",ed,""))

    # Teams data
    webdata <- webpage %>%
      html_nodes("td") %>%
      html_text()

    titles <-  webpage %>%
      html_nodes("th") %>%
      html_text()

     #cleaning stats names
    stat_team <- gsub("%", ".", titles[10:47])
    stat_team <- gsub("-", "", stat_team)
    stat_team <- gsub("/", "p", stat_team)

       # Creating a data frame
    dfg <- matrix(
      webdata,
      ncol = 38,
      byrow = T,
      dimnames = list(NULL, stat_team)
    )

    dfg <- as_tibble(dfg)

    abrv <- rename(teams_abrv, Abbrv = Team)

    dfg <- left_join(dfg, abrv, by = c("Team" = "Teams"))

# such a hassle just to convert the number to the right class ughh
# probably can just call type_convert on the whole df
    dfg$PDO <- as.numeric(sub("%","",dfg$PDO))/100
    dfg$PDOA <- as.numeric(sub("%","",dfg$PDOA))/100
    dfg$SH. <- as.numeric(sub("%","",dfg$SH.))/100
    dfg$SH.A <- as.numeric(sub("%","",dfg$SH.A))/100
    dfg$FO. <- as.numeric(sub("%","",dfg$FO.))/100
    dfg$SV. <- as.numeric(sub("%","",dfg$SV.))/100
    dfg$SV.A <- as.numeric(sub("%","",dfg$SV.A))/100

    cls <- cols(
      .default = col_double(),
      Rk = col_integer(),
      Team = col_character(),
      Abbrv = col_character(),
      Div = col_factor(),
      Conf = col_character(),
      City = col_character()
    )

   dfg <- type_convert(dfg, col_types = cls)

# returning data frame
  dfg <- dfg %>%  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(where(is.character), ~"League Avg.")))
  stands <<- dfg

}

getrankings <- function(){

  vdate <- seq(season_start, datetoday, by ="days")
  firstpass <- TRUE

  pb = txtProgressBar(min = 0, max = length(vdate), initial = 0) # progress bar

  for (i in 1:length(vdate)) {

    Sys.sleep(2) # seems like quanthokey has implementeded some kinda ip block functionality

    setTxtProgressBar(pb,i)  # progress bar

    df <- getstandings(v = "l", sd = vdate[1], ed = vdate[i])

    df <- df %>%
      mutate(date = vdate[i])

    if (firstpass) {
      table_rankings <<- df
    }else{
      table_rankings <<- bind_rows(table_rankings, df)
    }
    firstpass <- FALSE
  }
}

#Reading data from disk ----

readteamdata <- function(team=teamname, date=datetoday){

  cls <- cols(
    .default = col_double(),
    Opponent = col_character(),
    Loc. = col_factor(levels = c("Away", "Home")),
    Result = col_factor(levels = rev(c("Win", "OT Win", "SO Win", "SO Loss", "OT Loss", "Loss"))),
    Conf = col_character(),
    Div = col_factor(),
    Team = col_character(),
    Date = col_date(format = "%Y-%m-%d"),
    overall = col_factor(levels = c("Win", "Loss", "OL")),
    WOL = col_factor(),
    Gms = col_integer(),
    name = col_character(),
    City = col_character()
    )

  if(length(team) >= 2){
    # concatenating both arguments in the function call teamname and date
    filesread <- str_c(team, date)
    # readind all files of the vector teamname
    dfp <- map(filesread, ~read_csv(paste0("~/NHLonR/NHLschedulescraper/Data/", .x, ".csv"), col_types = cls))
    dfpp <- unlist(dfp, recursive = F)
    dfpp <- bind_rows(dfp)

    dfs <<- dfpp
  }else{

    dfs <<- read_csv(paste0("~/NHLonR/NHLschedulescraper/Data/",team,date,".csv"), col_types = cls)
}
}

# Quick Summary: league and team ----

quicksummary <- function(i = "league"){

  if(i == "league"){
  qksummary <<- dfs %>%
  summarise_at(vars(GF:BSD), mean, na.rm = TRUE) %>%
  add_column(name = "League Average")
  }
  if(i != "league") {
    getteamid(i)
    dummydf <- dfs %>%
      filter(name == teamname)

   points <- dummydf$PTS[1]

  ptspct = dummydf$PTS[1]/(dummydf$Gms[1]*2)
  pace = ptspct*112

  gamesplayed <- dummydf$Gms[1]

  last10 <- fct_count(dummydf$overall[1:10])

  lastmonth <- dummydf %>%
    select(Date, overall) %>%
    filter(Date >= datetoday - months(1)) %>%
    select(overall) %>%
    as_vector() %>%
    fct_count() %>%
    select(n) %>%
    as_vector() %>%
    str_c(as.character(), collapse = "-")

  #dtsince <- "2021-01-13"
  sncdate <- dummydf %>%
    select(Date, overall) %>%
    filter(Date >= datetoday - lubridate::weeks(1)) %>%
    select(overall) %>%
    as_vector() %>%
    fct_count() %>%
    select(n) %>%
    as_vector() %>%
    str_c(as.character(), collapse = "-")


  overall <- fct_count(dummydf$overall)

  Qks <<- tibble(
    descr = c("Games Played: ", "Points: ", "PTS%: ", "Pace: ", "Overall: ", "Last 10: ",
              "Last Month: ", "Last Week: "),
    val = c(gamesplayed, points, round(ptspct, 3), round(pace, 0), str_c(as.character(overall$n), collapse = "-"),
            str_c(as.character(last10$n), collapse = "-"), lastmonth, sncdate)
   )
  # writing file on disk
  write_tsv(Qks, paste0("Data/",teamname," ",datetoday," Quicksumm.tsv"), col_names = F)


  }

}

# Retrieve team Id ----

getteamid <- function(name){
  name <- str_to_lower(name)
  name <- str_replace_all(name, " ", "-")
  name <- str_replace_all(name, "\\.", "")
  name <- str_replace(name, "arizona", "phoenix")

  idx <<- str_which(nhlteams, name)

  if (length(idx)==0 | length(idx) > 1) {
    message("Invalid Name")
    stop("Try a valid name")
  }

  name <- nhlteams[idx]
  tname <- str_to_title(str_replace_all(name, "-", " "))
  tname <- str_replace(tname, "Phoenix", "Arizona")
  tname <- str_replace(tname, "St ", "St. ")

  teamname <<- tname

  message(paste0("ID of ", tname, " is: ", idx))
}

# standings only base on points and games, use getstandings function ----

league_standings <- function(i = "overall"){


   if(i == "div" | i == "division"){
    league_stands <<- dfs %>% group_by(name, Div) %>%  top_n(1, Gms) %>% arrange(desc(PTS), Gms) %>% select(name,PTS,Div,Gms)
   }
  if(i == "overall"){
   league_stands <<- dfs %>%  group_by(name, Div) %>%  top_n(1, Gms) %>% arrange(desc(PTS), Gms) %>% select(name,PTS,Div,Gms)
  }
}

# Own implementation of full standings classification, still incomplete ----

ranking <- function(){

dff <- dfs %>%
    filter(Gms <= 5)

points <- dff %>%
    select(name, Team, PTS, Gms) %>%
    group_by(Gms) %>%
    #slice_max(Gms) %>%
    arrange(desc(PTS), Gms) %>%
    dplyr::ungroup()

wins <- dff %>%
  select(name, Gms, Team, Result, PTS) %>%
  group_by(Gms, Team) %>%
  summarise(wins = fct_count(Result)) %>%
  group_by(Gms, Team) %>%
  filter(wins$f == "Win" | wins$f == "OT Win")  %>%
  group_by(Team, Gms) %>%
  summarise(win = sum(wins$n)) %>%
  arrange(desc(win))%>%
  dplyr::ungroup()

goals <- dff %>%
  select(name, Gms, Team, Result, PTS, GD) %>%
  group_by(Gms, Team) %>%
  summarise(GD = sum(GD)) %>%
  arrange(desc(GD)) %>%
  dplyr::ungroup()

division <- teams_abrv %>%
  select(Team:Conf)

rankings <- left_join(points, wins, by = c("Team", "Gms"))
rankings <- left_join(rankings, division, by = "Team")

test <- rankings %>%
  dplyr::group_by(Gms,Team) %>%
  mutate(fct = paste(PTS,Gms,win, sep = ",")) %>%
  arrange(Gms) %>%
  #dplyr::ungroup() %>%
  dplyr::group_by(Gms) %>%
  #mutate(rk = rank(fct, ties.method = "max")) %>%
  mutate(rank = as.numeric(as_factor(fct))) %>%
  arrange(Gms,rank)

test$oppo <- ifelse(duplicated(test$fct), lag(test$Team), NA )

headtohead <- dfs %>%
  filter(Team == test$Team, Oppo == test$oppo) %>%
  select(name:Result, Team:Div.Oppo)

#test$headtohead <- dfs %>%
# filter(Team == , Oppo == ) %>%

df_test <- filter(dfs, Gms <= 2)

gamesplayed <- sort(unique(dfs$Gms))

df_test <- map(gamesplayed, ~ filter(dfs, Gms <= .x))

df_filtered <- select(filter(df_test[[15]], grepl("Tor", name)), name, Opponent, Date, Result, WOL)

a <- df_filtered %>%
  group_by(Opponent) %>%
  summarise(series = fct_count(WOL), .groups = "drop") %>%
  dplyr::ungroup()

a1 <- a %>%
  group_by(Opponent) %>%
  summarise(dif = series$n) %>%
  mutate(hth = dif - lead(dif)) %>%
  filter(is.numeric(hth))


rankings <- left_join(rankings, goals, by = "Team")



}

# create a new df from using the stands df for the dashboard
#(call only after call getstandings()) ----

setupStands <- function(){

reshaped_stands <- stands %>%
  select(Team, Abbrv, Div, everything()) %>%
  select(-City, -Conf) %>%
  filter(!Team == "League Avg.") %>%
  gather(Rk:BSDpGP, key = "stats", value = "value")

stands_summary_max <- stands %>%
  select(Team, Abbrv, Div, everything()) %>%
  select(-City, -Conf) %>%
  filter(!Team == "League Avg.") %>%
  mutate(across(where(is.numeric), max)) %>%
  gather(Rk:BSDpGP, key = "stats", value = "max")

stands_summary_min <- stands %>%
  select(Team, Abbrv, Div, everything()) %>%
  select(-City, -Conf) %>%
  filter(!Team == "League Avg.") %>%
  mutate(across(where(is.numeric), min)) %>%
  gather(Rk:BSDpGP, key = "stats", value = "min")

stands_summary_rank <- stands %>%
  filter(!Team == "League Avg.") %>%
  select(Team, Abbrv, Div, everything()) %>%
  select(-City, -Conf) %>%
  mutate(across(c(4,5,7,8,13,15,18,21,25, 27, 32, 34,38), ~rank(.x, ties.method = c("min"))))

c <- seq(4,40,1)
c <- c[! c %in% c(4,5,7,8,13,15,18,21,25, 27, 32, 34,38)]

stands_summary_rank2 <- stands_summary_rank %>%
  mutate(across(all_of(c), ~rank(-.x, ties.method = c("max"))))

ranking <- stands_summary_rank2 %>%
gather(Rk:BSDpGP, key = "stats", value = "order")

reshaped_stands_with_max <- left_join(reshaped_stands, stands_summary_max, by = c("Team", "Abbrv", "Div", "stats"))
reshaped_stands_all <- left_join(reshaped_stands_with_max, stands_summary_min, by = c("Team", "Abbrv", "Div", "stats"))
reshaped_stands_full <<- left_join(reshaped_stands_all, ranking, by = c("Team", "Abbrv", "Div", "stats"))
}

# ============================================================================================

# NHL API
gettodaygames<- function() {

  schedule2 <- nhlapi::nhl_schedule_today()

  schedule1 <- schedule2 %>%
    first() %>%
    magrittr::extract2("dates") %>% magrittr::extract2("games") %>% first() %>% tibble()

  schedule <<- schedule1 %>%
    select(gamePk, teams.home.team.name,teams.home.score, teams.away.team.name, teams.away.score,
           gameDate, status.abstractGameState, gameType, season, link, venue.name)
}
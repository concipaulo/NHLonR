library(tidyverse)
library(rvest)
library(janitor)
library(hrbrthemes)
library(ggrepel)
# additional
#library(gghighlight) # highlight some lines
#library(plotly) # iterative plot

# Set variables for the other functions ----
setenv <- function(){
# named vector
teamcolors <<- c("ANA" = "#111111","ARI" = "#b43143","BOS" = "#c98e03","BUF" = "#00224d","CGY" = "#ce1126",
                "CAR" = "#76232F","CHI" = "#ec132c","COL" = "#6f263d",
                "CBJ" = "#041e42","DAL" = "#008053","DET" = "#a50d27","EDM" = "#fc4c02","FLA" = "#b9975b",
                "LAK" = "#a2aaad","MIN" = "#154734","MTL" = "#851425",
                "NSH" = "#ffb81c","NJD" = "#a50d27","NYI" = "#003087","NYR" = "#0039b3","OTT" = "#c69214",
                "PHI" = "#fa4616","PIT" = "#ffb81c","SJS" = "#008599", "SEA" = "#99D9D9",
                "STL" = "#002f87","TBL" = "#002d80","TOR" = "#00205b","VAN" = "#008852","VGK" = "#3f4e5a",
                "WSH" = "#ec1337","WPG" = "#031630")

nhlteams <<- c("anaheim-ducks","phoenix-coyotes", "boston-bruins", "buffalo-sabres",
             "calgary-flames", "carolina-hurricanes", "chicago-blackhawks", "colorado-avalanche",
             "columbus-blue-jackets", "dallas-stars", "detroit-red-wings", "edmonton-oilers",
             "florida-panthers", "los-angeles-kings", "minnesota-wild", "montreal-canadiens",
             "nashville-predators", "new-jersey-devils", "new-york-islanders", "new-york-rangers",
             "ottawa-senators", "philadelphia-flyers", "pittsburgh-penguins", "san-jose-sharks",
             "seattle-kraken",
             "st-louis-blues", "tampa-bay-lightning", "toronto-maple-leafs", "vancouver-canucks",
             "vegas-golden-knights", "washington-capitals", "winnipeg-jets")

teamsfiles <<- str_to_title(str_replace_all(nhlteams, "[:punct:]", " "))
teamsfiles <<- str_replace_all(teamsfiles, "Phoenix", "Arizona")
teamsfiles <<- str_replace(teamsfiles, "St ", "St. ")

teams_abrv <- read.csv("~/NHLonR/teams_abrev.csv", stringsAsFactors = F)
teams_abrv <<- as_tibble(teams_abrv)
datetoday <<- Sys.Date()
reference <<- sprintf("Source: www.quanthockey.com, %s", datetoday)
season_start <<- as.Date("2021-10-12", format = "%Y-%m-%d")

}

# Retrieving data from website and save a file on disk ----
getteamdata <- function(ii, jj = ii){

  index <- rep(ii:jj, 1)

  for (i in index) {
    options(warn=-1)
    message(paste0("Retriving data from ", nhlteams[i]),"\r",appendLF=T)
    Sys.sleep(2) # waiting time so that the website doesn't blck your ip
webpage <-
      read_html(
        paste0("https://www.quanthockey.com/nhl/team-game-logs/", nhlteams[i],"-2021-22-nhl-game-log.html"))

    # Teams data
    webdata <- webpage %>%
      html_table()

    webdata1 <- webdata[[1]] %>%
      row_to_names(1) %>%
      type_convert(col_types = cols(
        .default = col_double(),
        Date = col_date(format = "%Y-%m-%d"),
        Opponent = col_character(),
        Loc. = col_character(),
        Result = col_character(),
        PDO = col_number(),
        `PDO-A` = col_number(),
        `SH%` = col_number(),
        `SH%-A` = col_number(),
        `FO%` = col_number(),
        `SV%` = col_number(),
        `SV%-A` = col_number()
        ))

  webdata2 <- webdata1 %>%
    clean_names()

  dfs <- webdata2

# ==========
    teamname <- webpage %>%
      html_nodes("title") %>%
      html_text()


    # extracting team name
    teamname <- str_remove(teamname, "( Ga.*)")
    teamname <<- teamname

 # ---------
    dfs <- rename(dfs, Gms = rk)
    #dfs$Gms <- as.numeric(as.character(dfs$Gms))
    dfs$Gms <- rev(dfs$Gms)
    #dfs$Date <- as.Date(dfs$Date, format = "%Y-%m-%d")
    #dfs$GD <- as.numeric(as.character(dfs$GD))
    dfs$PDO <- dfs$pdo/100
    dfs$PDOA <- dfs$pdo_a/100
    dfs$SH. <- dfs$sh_percent/100
    dfs$SH.A <- dfs$sh_percent_a/100
    dfs$FO. <- dfs$fo_percent/100
    dfs$SV. <- dfs$sv_percent/100
    dfs$SV.A <- dfs$sv_percent_a/100
    dfs$name <- teamname

    dfs <- dfs %>%
      select(name, everything()) # nifty way of reordering columns

    dfs <- left_join(dfs, teams_abrv, by = c("name" = "Teams"))

    # Counting points

    dfs$PTS <- 0
    sum = 0
    for (i in nrow(dfs):1) {
      if (dfs$result[i] == "Win" | dfs$result[i] == "OT Win"| dfs$result[i] == "SO Win") {
        sum = sum + 2
        dfs$PTS[i] = sum
      }
      if(dfs$result[i] == "OT Loss" | dfs$result[i] == "SO Loss"){
        sum = sum + 1
        dfs$PTS[i] = sum
      }
      if(dfs$result[i] == "Loss" ){
        dfs$PTS[i] = sum
      }
    }

    dfs$result <- as.factor(str_trim(as.character(dfs$result)))
    dfs$overall <- NA
    dfs$overall <- fct_collapse(dfs$result, Win = c("Win", "OT Win", "SO Win"), Loss = c("Loss"), OL = c("SO Loss", "OT Loss"))
    dfs$WOL <- ifelse(dfs$gd > 0, "Win", "Loss")
    #writing file on the disk
    write.csv(dfs, file = paste0("Data/",teamname,datetoday,".csv"), row.names = F)

  }
}

# Retrieve standings
getstandings <- function(v = "l", sd = "202122", ed = sd){

  # v is the variable that control if the standings is per 'd'ivision or 'l'eague
  #sd = start date and ed = end date, as it is will return the most recent but can be passed any date
  # from the 2020-21 season in the following order %Y-%m-%d.

  # v = "l"
  # sd = "202122"
  # ed = "202122"

  webpage <-
      read_html(
        paste0("https://www.quanthockey.com/nhl/standings/standings.php?v=",v,"&s=pts&so=desc&ha=all&sd=",sd,"&ed=",ed,""))

    # Teams data
    webdata <- webpage %>%
      html_table()

    webdata1 <- webdata[[1]] %>%
      row_to_names(row_number = 1) %>%
      clean_names() %>%
      type_convert(cols(
        .default = col_double(),
        team = col_character(),
        pdo = col_number(),
        pdo_a = col_number(),
        pp_percent = col_number(),
        pk_percent = col_number(),
        sh_percent = col_number(),
        sh_percent_a = col_number(),
        fo_percent = col_number(),
        sv_percent = col_number(),
        sv_percent_a = col_number()
      )) %>%
      mutate(
        pdo = pdo/100,
        pdo_a = pdo_a/100,
        pp_percent = pp_percent/100,
        pk_percent = pk_percent/100,
        sh_percent = sh_percent/100,
        sh_percent_a = sh_percent_a/100,
        fo_percent = fo_percent/100,
        sv_percent = sv_percent/100,
        sv_percent_a = sv_percent_a/100
      )


    dfg <- webdata1

    abrv <- rename(teams_abrv, Abbrv = Team)

    dfg <- left_join(dfg, abrv, by = c("team" = "Teams"))

    cls <- cols(
      .default = col_double(),
      Rk = col_integer(),
      team = col_character(),
      Abbrv = col_character(),
      Div = col_factor(),
      Conf = col_character(),
      City = col_character()
    )

   dfg <- type_convert(dfg, col_types = cls)

# returning data frame
    stands <<- dfg

}

#Reading data from disk ----
readteamdata <- function(team=teamname, date=datetoday){

  cls <- cols(
    .default = col_double(),
    opponent = col_character(),
    loc = col_factor(levels = c("Away", "Home")),
    result = col_factor(levels = rev(c("Win", "OT Win", "SO Win", "SO Loss", "OT Loss", "Loss"))),
    Conf = col_character(),
    Div = col_factor(),
    Team = col_character(),
    date = col_date(format = "%Y-%m-%d"),
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
    dfp <- map(filesread, ~read_csv(paste0("Data/", .x, ".csv"), col_types = cls))
    dfpp <- unlist(dfp, recursive = F)
    dfpp <- bind_rows(dfp)

    dfs <<- dfpp
  }else{

    dfs <<- read_csv(paste0("Data/",team,date,".csv"), col_types = cls)
}
}

# Quick Summary: league and team ----
quicksummary <- function(i = "league"){

  if(i == "league"){
  qksummary <<- dfs %>%
  summarise_at(vars(gf:bs_d), mean, na.rm = TRUE) %>%
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
    select(date, overall) %>%
    filter(date >= datetoday - months(1)) %>%
    select(overall) %>%
    as_vector() %>%
    fct_count() %>%
    select(n) %>%
    as_vector() %>%
    str_c(as.character(), collapse = "-")

  dtsince <- "2022-01-01"
  sncdate <- dummydf %>%
    select(date, overall) %>%
    filter(date >= dtsince) %>%
    select(overall) %>%
    as_vector() %>%
    fct_count() %>%
    select(n) %>%
    as_vector() %>%
    str_c(as.character(), collapse = "-")


  overall <- fct_count(dummydf$overall)

  Qks <<- tibble(
    descr = c("Games Played: ", "Points: ", "PTS%: ", "Pace: ", "Overall: ", "Last 10: ",
              "Last Month: ", paste0("Since ", dtsince," : ")),
    val = c(gamesplayed, points, round(ptspct, 3), round(pace, 0), str_c(as.character(overall$n), collapse = "-"),
            str_c(as.character(last10$n), collapse = "-"), lastmonth, sncdate)
   )
  #
  # write_tsv(Qks, paste0("Data/",teamname," ",datetoday," Quicksumm.tsv"), col_names = F)


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

points <- dfs %>%
    select(name, Team, PTS, Gms) %>%
    group_by(Team) %>%
    slice_max(Gms) %>%
    arrange(desc(PTS), Gms) %>%
    dplyr::ungroup()

wins <- dfs %>%
  select(name, Team, Result, PTS) %>%
  group_by(Team) %>%
  summarise(wins = fct_count(Result)) %>%
  filter(wins$f == "Win" | wins$f == "OT Win") %>%
  group_by(Team) %>%
  summarise(win = sum(wins$n)) %>%
  arrange(desc(win))%>%
  dplyr::ungroup()

goals <- dfs %>%
  select(name, Team, Result, PTS, GD) %>%
  group_by(Team) %>%
  summarise(GD = sum(GD)) %>%
  arrange(desc(GD)) %>%
  dplyr::ungroup()

rankings <- left_join(points, wins, by = "Team")

test <- rankings %>%
  group_by(Team) %>%
  mutate(fct = paste(PTS,Gms,win, sep = ",")) %>%
  dplyr::ungroup()

test$oppo <- if_else(duplicated(test$fct), lag(test$Team), "")

#test$headtohead <- dfs %>%
# filter(Team == , Oppo == ) %>%






rankings <- left_join(rankings, goals, by = "Team")



}

# Functions calls

setenv()

# Retrieve data for all teams, can take a minute
getteamdata(1,32)

# Read data from disk, can read from a single team or a vector with team names
# The second argument is the date, which assumes is the datetoday by default.
# Files should have the name as "teamname""datetoday".csv. Read function for more information.
readteamdata(teamsfiles)

getteamid("leafs")

quicksummary()

getstandings()

# Teams average ----
teams_avg <- dfs %>%
  group_by(name) %>%
  summarise_at(vars(gf:bs_d), mean, na.rm = TRUE)

# victories that counts on the standins i.e. no SO Win ----
victories <- dfs %>%
  select(name:result, Team:WOL) %>%
  group_by(Team, Gms, Div) %>%
  summarise(wins = fct_count(result)) %>%
  filter(wins$f == "Win" | wins$f == "OT Win") %>%
  summarise(win = sum(wins$n)) %>%
  dplyr::ungroup() %>%
  select(Team, Gms, win)

dfs <- left_join(dfs, victories, by = c("Team", "Gms"))

# Rankings between the division ----
tracinggames <- dfs %>%
  select(name:date, Team:win) %>%
  group_by(Gms, Div) %>%
  arrange(desc(PTS), win) %>%
  mutate(rankings = row_number())

# Opponents abbreviations ----
oppo <- teams_abrv %>%
  select(Teams:Div) %>%
  rename(Oppo = Team, Div.Oppo = Div)

dfs <- left_join(dfs, oppo, by = c("opponent" = "Teams"))

# League average  ----
# stole from the internet lol
stands <- stands %>%
  bind_rows(summarise(.,
                      across(where(is.numeric), mean),
                      across(where(is.character), ~"League Avg.")))

# New Plots ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(date, PTS)) +
      geom_line(lwd = 1, col = teamcolors[idx])+
      geom_point(shape = 21, size = 6, alpha = 0.95, aes(fill= result, col=result, group = seq_along(date)))+
      #gghighlight(Date >= "2021-02-01")+
      theme_modern_rc(grid = "X,x,Y,y")+
      xlab("Games") +
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) + # uncomment if its a date
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      scale_fill_brewer(palette = "RdYlGn", name = "Result")+
      scale_color_brewer(palette = "RdYlGn", name = "Result")+
      labs(caption = reference, title = paste0(teamname," Record"), subtitle = "2020-21 Regular Season")


 ggsave(
  paste0("plots/",teamname,"/pts_so_far", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)


# Route ---
tracinggames %>%
  filter(Div == "ATLANTIC") %>%
  ggplot(aes(Gms, rankings, group = Team)) +
      geom_path(lwd = 1, aes(col = Team))+
      geom_point(shape = 21, size = 5, alpha = 0.95, aes(fill = Team, col = Team))+
      facet_wrap(vars(Div))+
      theme_modern_rc(grid = "X,x,Y")+
      theme(strip.text = element_text(colour = 'Grey'))+
      xlab("Games") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous(trans = "reverse", breaks = scales::pretty_breaks(n = 6))+
      scale_color_manual(values = teamcolors, aesthetics = c("color", "fill"))+

      labs(caption = reference, title = paste0("Division Rankings"), subtitle = "2020-21 Regular Season")

tracinggames %>%
  filter(Div == "ATLANTIC") %>%
  #filter(Team == "TOR" | Team == "MTL"| Team == "EDM") %>%
  ggplot(aes(date, PTS, group = Team))+
      geom_line(lwd = 1, aes(col = Team))+
      geom_point(shape = 21, size = 5, alpha = 0.99, aes(fill = Team, col = Team))+
      facet_wrap(vars(Div))+
      theme_modern_rc(grid = "X,x,Y,y")+
      theme(strip.text = element_text(colour = 'Grey'))+
      xlab("Games")+
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10))+ # uncomment if its a date
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_color_manual(values = teamcolors, aesthetics = c("color", "fill"))+

      labs(caption = reference, title = paste0("Division Record"), subtitle = "2020-21 Regular Season")

# Lollipop chart, coolest of all imo
# big numbers Goals, points, games, ...
var1 <- teamname
var2 <- "League"
stands %>%
  filter(team == var1 | grepl(var2, team)) %>%
  dplyr::select(gp, w, l, pts, gf, ga) %>%
  t() %>% as.data.frame() %>%
  #row_to_names(1) %>%
  rownames_to_column() %>%
  arrange(V1) %>%
  mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=V2, yend=V1), color="magenta", lwd = 0.7) +
    geom_point(size=5, color=teamcolors[idx]) +
    geom_point(aes(y=V2), size=5, color="#69b3a2", alpha=0.3) +
    hrbrthemes::theme_modern_rc(grid = "X,x,Y") +
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ylab("Value") +
    xlab("Stats")+
   labs(caption = reference, title = paste0(teamname, " Against League Average"), subtitle = "2020-21 Regular Season")

# small numbers, like sv., pdo., sh., ...
stands %>%
  filter(team == var1 | grepl(var2, team)) %>%
  dplyr::select(sv_percent, pdo, sh_percent, fo_percent, pts_gp) %>%
  t() %>% as.data.frame() %>%
  #row_to_names(1) %>%
  rownames_to_column() %>%
  arrange(V2) %>%
  mutate(rowname=factor(rowname, rowname)) %>%
  ggplot( aes(x=rowname, y=V1)) +
    geom_segment( aes(x=rowname ,xend=rowname, y=V2, yend=V1), color="magenta", lwd = 0.8) +
    geom_point(size=5, color=teamcolors[idx]) +
    geom_point(aes(y=V2), size=5, color="#69b3a2", alpha=0.3) +
    hrbrthemes::theme_modern_rc(grid = "X,x,Y") +
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    ylab("Value") +
    xlab("Stats")+
   labs(caption = reference, title = paste0(teamname, " Against League Average"), subtitle = "2020-21 Regular Season")

# Parallel Coordinate
# This is really versatile but I couldn't find a nice dataset to feed to :(
dfs %>%
  mutate(PTS. = PTS/Gms) %>%
  #filter(Div == "NORTH") %>%
  #filter(name == var1 | grepl(var2, name)) %>%
  filter(name == var1) %>%
  select(WOL, PDO, SV., SH., FO., PTS.) %>%
GGally::ggparcoord(
    columns = 2:6, groupColumn = 1,
    #order = "Clumpy",
    order = "anyClass",
    scale = "globalminmax",
    #missing = "mean",
    showPoints = TRUE,
    title = paste0("Parallel Coordinate Plot for the ", teamname," Games"),
    #mapping = ggplot2::aes(size = 2),
    alphaLines = 0.3
    ) +
  #ggplot2::scale_size_identity() +
  #scale_color_viridis_d() +
  #scale_color_manual(values=c( "magenta", rep("grey", 6))) + #highlighting
  scale_color_manual(values=c( "magenta", "grey")) + #highlighting
  #scale_color_brewer(palette = "RdYlGn", name = "Team")+
  theme_modern_rc()

# example of parallel coordinate plot
 GGally::ggparcoord(data = iris, columns = 1:4, groupColumn = 5, order = "anyClass")


# Points percentage

 dfs %>%
  mutate(PTS. = PTS /(Gms*2)) %>%
  filter(grepl(teamname, name) ) %>%
  ggplot(aes(Gms, PTS.)) +
  geom_line(stat = "identity", col = "magenta") +
  geom_point(shape = 21, size = 5, aes( col = Team, fill = Team)) +
  theme(legend.title = element_blank())+
  theme_modern_rc(grid = "X,y,Y", ) +
  # change background color
  #theme(panel.background = element_rect(fill = "grey10", color = "grey10"), plot.background = element_rect(fill = "grey10")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  ylab("PTS%") +
  xlab("Games") +
  scale_color_manual(values = teamcolors, aesthetics = c("color", "fill"))+
  #scale_color_manual(values=c( "magenta", rep("grey", 6))) + #highlighting
  labs(caption = reference, title = paste0(teamname," PTS%"), subtitle = "2020-21 Regular Season")



# Old stuff ----
# Sv%
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(date, SV.)) +
  geom_path(stat = "identity", col = teamcolors[idx]) +
  geom_point(shape = 21, size = 5, col = teamcolors[idx], aes(fill = result)) +
  geom_hline(aes(yintercept = qksummary$sv_percent/100), col = "magenta", label = "Avg")+
  annotate("text", x = as.Date("2021-10-10", "%Y-%m-%d"), y = (qksummary$sv_percent/100+ 0.005), label = "League Avg.", col = "magenta")+
  theme(legend.title = element_blank())+
  #gghighlight(Date >= "2020-02-25")+
  theme_modern_rc(grid = "X,x,y,Y") +
  theme(legend.position = "left")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) +
  ylab("Save %") +
  xlab("Date") +
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  #scale_color_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_manual(values = teamcolors, aesthetics = c("color"))+
  labs(caption = reference, title = paste0(teamname," SV%"), subtitle = "2020-21 Regular Season")
  # geom_label(col = "snow3", fill = "grey13",
  #   data = dfs %>% filter(grepl(teamname, name), SV. < "0.8"),
  #   aes(label = oppo),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  paste0("plots/",teamname,"/svpctpergp", datetoday, ".png"),
  width = 13,
  height = 8,
  units = "in",
  dpi = 320
)

# unrevised ----
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(date, fill = WOL, color = WOL)) +
  geom_col(aes(y = gd), width = 0.9) +
  #gghighlight(Date >= "2020-02-25")+
  theme_modern_rc(grid = "X,Y") +
  theme(legend.position = "none")+
  #scale_colour_manual(values =  c("#2eb82e", "brown1")) +
  #scale_fill_manual(values =  c("springgreen2", "firebrick1"),  aesthetics = c("fill")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) +
  ylab("Net Goals") +
  labs(caption = reference, title = paste0(teamname," Goal Differential per Game"), subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/",teamname,"/goaldiff", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# Sh% in funtion to sv% ----
# TODO plot line of PDO = 102
#pdoc <- dfs$SH. + dfs$SV.
dfs %>%
  filter(grepl(teamname, name)) %>%
  mutate(pdoc = SH. + SV.)%>%
  ggplot(aes(SH., SV.)) +
  geom_abline(intercept = 1, slope = -1, col = "magenta", lwd = 1.1)+
  geom_point(shape = 21, size = 9, aes(fill = result, col = result)) +
  #gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
  theme_modern_rc() +
  theme(legend.position = "left") +
  #scale_fill_viridis_d(option = "D", begin = 0.1, end = 0.95) +
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_brewer(palette = "RdYlGn", name = "Result")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  scale_x_percent(breaks = scales::pretty_breaks(n = 5)) +
  ylab("SV%") +
  xlab("SH%") +
   geom_label_repel(
    col = "grey", fill = "#1e1e1e",
    data = dfs %>% filter(grepl(teamname, name), PDO < "1", WOL == "Win"),
    #data = dfs %>% filter(grepl(teamname, name), SV. < "0.8"),
    aes(label = Oppo),
    point.padding = 10,
    show.legend = FALSE
  )+
  labs(caption = reference, title = paste0(teamname," SV% in Relation to SH%"), subtitle = "2020-21 Regular Season") +
  annotate("text", label = "PDO = 100", x = 0.179, y = 0.8, size = 5, colour = "magenta")

ggsave(
 paste0("plots/",teamname,"/shpcttosvpct", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# PDO per game ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Gms)) +
  # trick to make lines glow :)
  #geom_line(stat = "identity", color = "magenta", aes(y= PDO), size = 3, alpha = 0.1) +
  #geom_line(stat = "identity", color = "magenta", aes(y= PDO), size = 2, alpha = 0.2) +
  geom_line(stat = "identity", color = "magenta", aes(y= PDO), size = 0.8, alpha = 0.5) +
  geom_point(aes(y = PDO, fill = result), col = "magenta", shape = 21, size = 10) +
  #gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
  theme_modern_rc(grid = "X,Y,y")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  theme(legend.position = "left") +
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_brewer(palette = "RdYlGn", name = "Result")+
  #scale_color_manual(values = teamcolors, aesthetics = c("color", "fill"))+
  xlab("Game") +
  labs(caption = reference, title = paste0(teamname," PDO per Game"), subtitle = "2020-21 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Oppo),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  paste0("plots/",teamname,"/pdopergame", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# Save% with results ----
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Gms, SV.)) +
  #geom_line(stat = "identity", color = "#ffffff") +
  geom_hline(yintercept = qksummary$SV., color = "magenta", lwd = 1.1)+
  geom_point(aes(fill = result, col = result), shape = 21, size = 10) +
  #gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
  theme_modern_rc(grid = "X,Y,y")+
  theme(legend.position = "left")+
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_brewer(palette = "RdYlGn", name = "Result")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 8)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
  xlab("Game") +
  labs(caption = reference, title = paste0(teamname," SV% per Game"), subtitle = "2020-21 Regular Season") +
  annotate("text", x = 1.5, y = (qksummary$SV.+ 0.005), label = "League Avg.", col = "magenta")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Oppo),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
   paste0("plots/",teamname,"/savepergame", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# Average shots % in results and location ----

dfs %>%
  mutate(result = reorder(result, SH., median)) %>%
  filter(grepl(teamname, name)) %>%
  select(Team, result, SH.) %>%
  ggplot(aes(x = result, y = SH., fill = result)) +
  geom_boxplot(color = "magenta", fill = teamcolors[idx], notch = F) +
  #geom_violin()+
  theme_modern_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Results") +
  labs(caption = reference, title = paste0(teamname," SH% in Relation to Results"), subtitle = "2020-21 Regular Season")


ggsave(
  paste0("plots/",teamname,"/shpcttoresults", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(loc,SH.)) +
  geom_boxplot(color = "magenta", fill = teamcolors[idx]) +
  theme_modern_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Location") +
  labs(caption = reference, title = paste0(teamname," SH% in Relation to Location"), subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shtoloc", datetoday, ".png"),
 width = 11,
  height = 6,
  dpi = 320
)
#
# # SH% against all opponents

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Oppo, SH., Div.Oppo)) +
  geom_bar(stat = "summary", fun = "mean", color = "magenta", fill = teamcolors[idx]) +
  theme_modern_rc(grid = "Y,y")+
  theme(strip.text = element_text(colour = 'snow1'))+
  facet_wrap(~Div.Oppo, scales = "free_x")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Opponent")+
  labs(caption = reference, title = paste0(teamname," SH% for Each Opponent"), subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shpcttoteams", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)


#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Div.Oppo,SH.)) +
  geom_boxplot(color = "magenta", fill = teamcolors[idx]) +
  theme_modern_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SH% for Each Division"), subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shpcttodiv", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

#

# SV% against opponents
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Oppo, SV.)) +
  geom_boxplot(color = "magenta", fill = teamcolors[idx], notch = F) +
  theme_modern_rc()+
  theme(strip.text = element_text(colour = 'snow1'))+
  facet_wrap(~Div.Oppo, scales = "free_x")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Opponent")+
  labs(caption = reference, title = paste0(teamname," SV% Against Opponents"), subtitle = "2021-22 Regular Season")

ggsave(
   paste0("plots/",teamname,"/shpcttoconf", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Div.Oppo, SV.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SV% Against Division"), subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/",teamname,"/svpcttodiv", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)
# Save% against each team ----
dfs %>%
  filter(grepl(teamname, name)) %>%
  group_by(Oppo, Div.Oppo) %>%
  summarise(sv_mean = mean(SV.)) %>%
  ggplot(aes(Oppo, sv_mean)) +
  #geom_bar(stat = "summary", fun = "mean", color = "magenta", fill = teamcolors[idx], width = 0.8) +
  geom_bar(stat = "summary", fun="mean", color = "magenta", fill = teamcolors[idx], width = 0.8)+
  #geom_hline(yintercept = qksummary$sv_percent/100, color = "magenta", lwd = 1.1)+
  geom_text(size = 4, aes(label = round(sv_mean, 3)), col = "magenta", position = position_dodge(width = -1), vjust = 1.5)+
  #annotate("text", x = 4, y = (qksummary$sv_percent/100+ 0.009), label = "League Avg.", col = "magenta")+
  facet_wrap(~Div.Oppo, scales = "free_x") +
  theme_modern_rc(grid = "Y,y")+
  theme(strip.text = element_text(colour = 'Grey'))+
  coord_cartesian(ylim = c(0.6, 1))+
  scale_y_percent(breaks = scales::pretty_breaks(n = 5))+
  xlab("Team") +
  labs(caption = reference, title = paste0(teamname," Average SV% Against Opponents"), subtitle = "2020-21 Regular Season")

ggsave(
 paste0("plots/",teamname,"/svpcttoteam", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# Number of goals in relation to result of game ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(result, gf, fill= result)) +
  geom_boxplot(color = "magenta")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth = 0.3)+
  theme_modern_rc()+
  xlab("Result") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_fill_brewer(palette = "RdYlGn", name = "Nº Games")+
  scale_color_brewer(palette = "RdYlGn", name = "Nº Games")+
  labs(caption = reference, title = paste0(teamname," Outcome of Game by Number of Goals Scored For"),
                                            subtitle = "2020-21 Regular Season")

ggsave(
   paste0("plots/",teamname,"/resultpergoalsfor", datetoday, ".png"),
 width = 11,
  height = 6,
  dpi = 320
)

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(result, ga, fill= result)) +
  geom_boxplot(color = "magenta")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth = 0.3)+
  theme_modern_rc()+
  xlab("Result") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_fill_brewer(palette = "RdYlGn", name = "Nº Games")+
  scale_color_brewer(palette = "RdYlGn", name = "Nº Games")+
  labs(caption = reference, title = paste0(teamname," Outcome of Game by Number of Goals Scored Against"),
                                            subtitle = "2020-21 Regular Season")

ggsave(
 paste0("plots/",teamname,"/resultspergoalagainst", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)

# WOL ----
#
dfs%>%
  filter(grepl(teamname, name)) %>%
  ggplot() +
  geom_count(aes(Oppo, overall), col= "magenta")+
  scale_size(breaks = 1:10, name = "Nº of \nOcurrences")+
  theme_modern_rc()+
  theme(strip.text = element_text(colour = 'snow2'))+
  facet_wrap(~Div.Oppo, scales = "free_x")+
  xlab("Opponent")+
  labs(caption = reference, title = paste0(teamname," Distribution of Wins/Losses Across the Division"),
       subtitle = "2020-21 Regular Season")

ggsave(
   paste0("plots/",teamname,"/wolperteams", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 320
)
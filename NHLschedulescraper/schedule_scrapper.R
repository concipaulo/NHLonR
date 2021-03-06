# Loading libraries -----
library(rvest)
library(magrittr)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(gghighlight)
#library(gganimate)
#library(gifski)


# Functions ----
setenv <- function(){
teamcolors <<- c("#b5985a", "#8c2633", "#fcb514", "#002654", "#ce1126", "#76232F", "#cc8a00", "#6f263d",
            "#041e42", "#006341", "#c8102e", "#fc4c02", "#b9975b", "#a2aaad", "#154734", "#a6192e",
            "#ffb81c", "#c8102e", "#003087", "#0033a0", "#c69214", "#fa4616", "#ffb81c", "#006272",
            "#041e42", "#00205b", "#00205b", "#008852", "#b9975b", "#041e42", "#041e42")

nhlteams <<- c("anaheim-ducks","phoenix-coyotes", "boston-bruins", "buffalo-sabres",
             "calgary-flames", "carolina-hurricanes", "chicago-blackhawks", "colorado-avalanche",
             "columbus-blue-jackets", "dallas-stars", "detroit-red-wings", "edmonton-oilers",
             "florida-panthers", "los-angeles-kings", "minnesota-wild", "montreal-canadiens",
             "nashville-predators", "new-jersey-devils", "new-york-islanders", "new-york-rangers",
             "ottawa-senators", "philadelphia-flyers", "pittsburgh-penguins", "san-jose-sharks",
             "st-louis-blues", "tampa-bay-lightning", "toronto-maple-leafs", "vancouver-canucks",
             "vegas-golden-knights", "washington-capitals", "winnipeg-jets")

teamsfiles <<- str_to_title(str_replace_all(nhlteams, "[:punct:]", " "))
teamsfiles <<- str_replace_all(teamsfiles, "Phoenix", "Arizona")
teamsfiles <<- str_replace(teamsfiles, "St ", "St. ")

teams_abrv <- read.csv("~/NHLonR/teams_abrev2.csv", stringsAsFactors = F)
teams_abrv <<- as_tibble(teams_abrv)
datetoday <<- Sys.Date()
reference <<- sprintf("Source: www.quanthockey.com, %s", datetoday)

}

getteamdata <- function(ii, jj = ii){

  index <- rep(ii:jj, 1)

  for (i in index) {
    #svMisc::progress(index * 100/length(idx))
    options(warn=-1)
    message(paste0("Retriving data from ", nhlteams[i]),"\r",appendLF=T)
    Sys.sleep(2)

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
    dfs <-matrix(
      webdata,
      ncol = 27,
      byrow = T,
      dimnames = list(NULL, stat_team)
    )

    dfs <- as.data.frame(dfs)

    dfs$Gms <- as.numeric(as.character(dfs$Rk))
    #dfs$Gms <- rev(dfs$Gms)
    dfs$Date <- as.Date(dfs$Date, format = "%Y-%m-%d")
    dfs$GD <- as.numeric(as.character(dfs$GD))
    dfs$PDO <- as.numeric(sub("%","",dfs$PDO))/100
    dfs$PDOA <- as.numeric(sub("%","",dfs$PDOA))/100
    dfs$SH. <- as.numeric(sub("%","",dfs$SH.))/100
    dfs$SH.A <- as.numeric(sub("%","",dfs$SH.A))/100
    dfs$FO. <- as.numeric(sub("%","",dfs$FO.))/100
    dfs$SV. <- as.numeric(sub("%","",dfs$SV.))/100
    dfs$SV.A <- as.numeric(sub("%","",dfs$SV.A))/100


    # Adding new columns
    dfs$Team <- NA
    dfs$Div <- NA
    dfs$Conf <- NA

    for (i in 1:nrow(dfs)) {
      for (j in 1:nrow(teams_abrv)) {
        if (dfs[i, 3] == as.character(teams_abrv[j, 1])) {
          dfs[i,30] <- teams_abrv[j, 4]
          dfs[i,29] <- teams_abrv[j, 3]
          dfs[i,28] <- teams_abrv[j, 2]
        }
      }
    }

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

    # Writing backup
    write.csv(dfs, file = paste0("Data/",teamname,datetoday,".csv"), row.names = F)
  }
}

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
    Rk = col_integer()
    )

  if(length(team) >= 2){
    filesread <- str_c(team, date)

    dfp <- map(filesread, ~read_csv(paste0("Data/", .x, ".csv"), col_types = cls))
    dfpp <- unlist(dfp, recursive = F)
    dfpp <- bind_rows(dfp, .id= "name")

    dfpp$Team

    for(i in 1:length(filesread)){
      for(j in 1:nrow(dfpp)){
      if(i == dfpp$name[j]){
        dfpp$name[j] = filesread[i]
        }
      }
    }

    dfpp$name <- dfpp$name %>% str_replace_all( "[:digit:]+[:punct:]*", "")
    dfs <<- dfpp
  }else{

    dfs <<- read_csv(paste0("Data/",team,date,".csv"), col_types = cls)
}
}

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

  ptspct = dummydf$PTS[1]/(dummydf$Rk[1]*2)
  pace = ptspct*112

  gamesplayed <- dummydf$Rk[1]

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

  dtsince <- "2021-01-13"
  sncdate <- dummydf %>%
    select(Date, overall) %>%
    filter(Date >= dtsince) %>%
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

league_standings <- function(i = "overall"){


   if(i == "div" | i == "division"){
    league_stands <<- dfs %>% group_by(name, Div) %>%  top_n(1, Rk) %>% arrange(Div, desc(PTS)) %>% select(name,PTS,Div,Rk)
   }
  if(i == "overall"){
   league_stands <<- dfs %>%  group_by(name, Div) %>%  top_n(1, Rk) %>% arrange(desc(PTS)) %>% select(name,PTS,Div,Rk)
  }
}

# Retrieve Website Data for all teams ----
setenv()

getteamid("leafs")

getteamdata(1,31)

# Reading file one team at the time or multiple teams ----
datetoread <- "2021-01-30"
readteamdata(teamsfiles, datetoday)

# Quick summary of the team read above, takes teamname and datetoday from global evn ----

quicksummary()

teamsavg <- dfs %>%
  group_by(name) %>%
  summarise_at(vars(GF:BSD), mean, na.rm = TRUE)


# CHARTS ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Date, PTS)) +
      geom_point(shape = 21, size = 6, alpha = 0.95, aes(fill= Result, col=Result, group = seq_along(Date)))+
      geom_line(col = teamcolors[idx], lwd = 1.5)+

      #gghighlight(Date >= "2020-02-25")+
      theme_modern_rc(grid = "X,x,Y")+
      xlab("Games") +
      #scale_x_continuous(breaks = scales::pretty_breaks(n = 10))+
      scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
      scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) + # uncomment if its a date
      scale_fill_brewer(palette = "RdYlGn", name = "Result")+
      scale_color_brewer(palette = "RdYlGn", name = "Result")+
      labs(caption = reference, title = paste0(teamname," Record"), subtitle = "2019-20 Regular Season")


 ggsave(
  paste0("plots/",teamname,"/pts_so_far", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# This make an animation of the chart above ----
#gganim <- point + transition_reveal(Date)

#animate(gganim, height = 720, width = 1280, nframes = 225, fps =25 ,
#       end_pause = 30, renderer = gifski_renderer(paste0(teamname,".gif")))

# Save pct one color ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Date, SV.)) +
  geom_path(stat = "identity", col = teamcolors[idx]) +
  geom_point(
    shape = 21,
    size = 5,
    col = teamcolors[idx],
    fill = teamcolors[idx]
  ) +
   geom_hline(yintercept = qksummary$SV.)+
  #gghighlight(Date >= "2020-02-25")+
  theme_ipsum_rc(grid = "X,y,Y") +
  scale_y_percent() +
  scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) +
  ylab("Save %") +
  xlab("Date") +
  labs(caption = reference, title = paste0(teamname," SV%"), subtitle = "2019-20 Regular Season")+
  geom_label(
    col = "black",
    data = dfs %>% filter(grepl(teamname, name), SV. < "0.8"),
    aes(label = Team),
    nudge_y =  0.01,
    nudge_x =  -0.01,
    show.legend = FALSE
  )

ggsave(
  paste0("plots/",teamname,"/svpctpergp", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Goal differential ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Date, fill = WOL, color = WOL)) +
  geom_col(aes(y = GD), width = 1) +
  gghighlight(Date >= "2020-02-25")+
  theme_ipsum_rc(grid = "X,Y") +
  theme(legend.position = "none")+
  scale_colour_manual(values =  c( "#ff1a1a","#2eb82e")) +
  scale_fill_manual(values =  c( "#ff1a1a","#2eb82e"),  aesthetics = c("fill")) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  scale_x_date(date_labels = "%d-%b", breaks = scales::pretty_breaks(n = 10)) +
  ylab("Net Goals") +
  labs(caption = reference, title = paste0(teamname," Goal Differential per Game"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/goaldiff", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Sh% in funtion to sv% ----
# TODO plot line of PDO = 102
#pdoc <- dfs$SH. + dfs$SV.
dfs %>%
  filter(grepl(teamname, name)) %>%
  mutate(pdoc = SH. + SV.)%>%
  ggplot(aes(SH., SV.)) +
  geom_abline(intercept = 1, slope = -1, col = "#F0FFFF", lwd = 1.5)+
  geom_point(shape = 21, size = 10, aes(fill = Result, col = Result)) +
  gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
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
    col = "black",
    data = dfs %>% filter(grepl(teamname, name), PDO < "1", WOL == "Win"),
    aes(label = Team),
    point.padding = 10,
    show.legend = FALSE
  )+
  labs(caption = reference, title = paste0(teamname," SV% in Relation to SH%"), subtitle = "2019-20 Regular Season") +
  annotate("text", label = "PDO = 100", x = 0.179, y = 0.8, size = 5, colour = "#F0FFFF")

ggsave(
 paste0("plots/",teamname,"/shpcttosvpct", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# PDO per game ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Rk)) +
  geom_line(stat = "identity", color = teamcolors[idx], aes(y= PDO), lwd = 1) +
  geom_point(aes(y = PDO, fill = Result, col = Result),
    shape = 21,
    size = 10
  ) +
  gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
  theme_modern_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6))+
  theme(legend.position = "left") +
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_brewer(palette = "RdYlGn", name = "Result")+
  xlab("Game") +
  labs(caption = reference, title = paste0(teamname," PDO per Game"), subtitle = "2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  paste0("plots/",teamname,"/pdopergame", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Save% with results ----
dfs %>%
  ggplot(aes(Rk, SV.)) +
  #geom_line(stat = "identity", color = "#ffffff") +
  geom_hline(yintercept = 0.905, color = "deepskyblue", lwd = 1.1)+
  geom_point(aes(fill = Result, col = Result),
    shape = 21,
    size = 10
  ) +
  gghighlight(Date >= "2020-02-25", use_direct_label = FALSE)+
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_brewer(palette = "RdYlGn", name = "Result")+
  scale_color_brewer(palette = "RdYlGn", name = "Result")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Game") +
  labs(caption = reference, title = paste0(teamname," SV% per Game"), subtitle = "2019-20 Regular Season") +
  annotate("text", label = "Lg. Avg.", x = 1.5, y = 0.910, size = 5, colour = "#001233")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
   paste0("plots/",teamname,"/savepergame", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Average shots % in results and location ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Result,SH.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Results") +
  labs(caption = reference, title = paste0(teamname," SH% in Relation to Results"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shpcttoresults", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Loc.,SH.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Results") +
  labs(caption = reference, title = paste0(teamname," SH% in Relation to Location"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shtoloc", datetoday, ".png"),
 width = 11,
  height = 6,
  dpi = 600
)
#
# # SH% against all opponents
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Team,SH.)) +
  geom_bar(stat = "summary", fun = "mean", color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc(grid = "Y,y")+
  facet_wrap(~Div, scales = "free_x")+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SH% for Each Opponent"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shpcttoteams", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Div,SH.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SH% for Each Division"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/shpcttodiv", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Conf,SH.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SH% Against Conferences"), subtitle = "2019-20 Regular Season")

ggsave(
   paste0("plots/",teamname,"/shpctodiv", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# SV% against opponents ----
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Conf, SV.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SV% Against Conferences"), subtitle = "2019-20 Regular Season")

ggsave(
   paste0("plots/",teamname,"/shpcttoconf", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
#
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Div, SV.)) +
  geom_boxplot(color = "#00001a", fill = teamcolors[idx]) +
  theme_ipsum_rc()+
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  labs(caption = reference, title = paste0(teamname," SV% Against Division"), subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/",teamname,"/svpcttodiv", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
# Save% against each team ----
dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Team, SV.)) +
  geom_bar(stat = "summary", fun = "mean", color = "#00001a", fill = teamcolors[idx]) +
  facet_wrap(~Div, scales = "free_x") +
  theme_ipsum_rc(grid = "Y,y")+
  coord_cartesian(ylim = c(0.6, 1))+
  scale_y_percent(breaks = scales::pretty_breaks(n = 5))+
  xlab("Team") +
  labs(caption = reference, title = paste0(teamname," Average SV% Against Opponents"), subtitle = "2019-20 Regular Season")

ggsave(
 paste0("plots/",teamname,"/svpcttoteam", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Number of goals in relation to result of game ----

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Result,GF, fill= Result)) +
  geom_boxplot(color = teamcolors[idx])+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth = 0.3)+
  theme_ipsum_rc()+
  xlab("Result") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_fill_brewer(palette = "RdYlGn", name = "Nº Games")+
  scale_color_brewer(palette = "RdYlGn", name = "Nº Games")+
  labs(caption = reference, title = paste0(teamname," Outcome of Game by Number of Goals Scored For"),
                                            subtitle = "2019-20 Regular Season")

ggsave(
   paste0("plots/",teamname,"/resultpergoalsfor", datetoday, ".png"),
 width = 11,
  height = 6,
  dpi = 600
)

dfs %>%
  filter(grepl(teamname, name)) %>%
  ggplot(aes(Result,GA, fill= Result)) +
  geom_boxplot(color = teamcolors[idx])+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.8, binwidth = 0.3)+
  theme_ipsum_rc()+
  xlab("Result") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_fill_brewer(palette = "RdYlGn", name = "Nº Games")+
  scale_color_brewer(palette = "RdYlGn", name = "Nº Games")+
  labs(caption = reference, title = paste0(teamname," Outcome of Game by Number of Goals Scored Against"),
                                            subtitle = "2019-20 Regular Season")

ggsave(
 paste0("plots/",teamname,"/resultspergoalagainst", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)

# WOL ----

dfs%>%
  filter(grepl(teamname, name)) %>%
  ggplot() +
  geom_count(aes(Team, overall), col= teamcolors[idx])+
  scale_size(breaks = 1:10, name = "Nº of \nOcurrences")+
  theme_ipsum_rc()+
  facet_wrap(~Div, scales = "free_x")+
  labs(caption = reference, title = paste0(teamname," Distribution of Wins/Losses across the league"),
       subtitle = "2019-20 Regular Season")

ggsave(
   paste0("plots/",teamname,"/wolperteams", datetoday, ".png"),
  width = 11,
  height = 6,
  dpi = 600
)
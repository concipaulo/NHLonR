library(rvest)
library(magrittr)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(scales)
library(gganimate)
library(gifski)


teams_abrv <- read.csv("~/NHLonR/teams_abrev.csv", stringsAsFactors = F)
# URL ----
#regular season page
teamcolors <- c("#b5985a", "#8c2633", "#fcb514", "#002654", "#ce1126", "#76232F", "#cc8a00", "#6f263d",
            "#041e42", "#006341", "#c8102e", "#fc4c02", "#b9975b", "#a2aaad", "#154734", "#a6192e",
            "#ffb81c", "#c8102e", "#003087", "#0033a0", "#c69214", "#fa4616", "#ffb81c", "#006272",
            "#041e42", "#00205b", "#00205b", "#008852", "#b9975b", "#041e42", "#041e42")

nhlteams <- c("anaheim-ducks","phoenix-coyotes", "boston-bruins", "buffalo-sabres",
             "calgary-flames", "carolina-hurricanes", "chicago-blackhawks", "colorado-avalanche",
             "columbus-blue-jackets", "dallas-stars", "detroit-red-wings", "edmonton-oilers",
             "florida-panthers", "los-angeles-kings", "minnesota-wild", "montreal-canadiens",
             "nashville-predators", "new-jersey-devils", "new-york-islanders", "new-york-rangers",
             "ottawa-senators", "philadelphia-flyers", "pittsburgh-penguins", "san-jose-sharks",
             "st-louis-blues", "tampa-bay-lightning", "toronto-maple-leafs", "vancouver-canucks",
             "vegas-golden-knights", "washington-capitals", "winnipeg-jets")

idx = 27
index <- seq(1:31)


webpage <-
  read_html(
    paste0("https://www.quanthockey.com/nhl/team-game-logs/", nhlteams[idx],"-2019-20-nhl-game-log.html"
  ))
# playoffs format
# team.playoff.html <- read_html("https://www.quanthockey.com/nhl/team-game-logs/st-louis-blues-2018-19-nhl-playoff-game-log.html")
#player stats first 50
#https://www.quanthockey.com/nhl/seasons/2019-20-nhl-players-stats.html

# Scraping data ----
datetoday <- Sys.Date()
reference <- sprintf("Source: www.quanthockey.com, %s", datetoday)
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
stat_name <- gsub("%", ".", titles[9:35])
stat_name <- gsub("-", "", stat_name)

# extracting team name
teamname <- str_remove(teamname, "( Ga.*)")

# creating a data frame ----
dfs <-matrix(
    webdata,
    ncol = 27,
    byrow = T,
    dimnames = list(NULL, stat_name)
  )

dfs <- as_tibble(dfs)

#dfs <- tbl_df(dfs) # dplyr local data frame

# formatting data :( ----
dfs$Rk <- as.numeric(as.character(dfs$Rk))
dfs$Rk <- rev(dfs$Rk)
#dfs$Team <- rep(teamname, length(dfs$Rk))
dfs$Date <- as.Date(dfs$Date, format = "%Y-%m-%d")
dfs$PDO <- as.numeric(sub("%","",dfs$PDO))/100
dfs$PDOA <- as.numeric(sub("%","",dfs$PDOA))/100
dfs$SH. <- as.numeric(sub("%","",dfs$SH.))/100
dfs$SH.A <- as.numeric(sub("%","",dfs$SH.A))/100
dfs$FO. <- as.numeric(sub("%","",dfs$FO.))/100
dfs$SV. <- as.numeric(sub("%","",dfs$SV.))/100
dfs$SV.A <- as.numeric(sub("%","",dfs$SV.A))/100

dfsfact <- stat_name[4:5]
dfs[dfsfact] <- lapply(dfs[dfsfact], factor)

dfs$GD <- as.numeric(as.character(dfs$GD))
dfs$GF <- as.numeric(as.character(dfs$GF))
dfs$GA <- as.numeric(as.character(dfs$GA))
dfs$SF <- as.numeric(as.character(dfs$SF))
dfs$SA <- as.numeric(as.character(dfs$SA))
dfs$SD <- as.numeric(as.character(dfs$SD))
dfs$FOW <- as.numeric(as.character(dfs$FOW))
dfs$FOL <- as.numeric(as.character(dfs$FOL))
dfs$FOD <- as.numeric(as.character(dfs$FOD))
dfs$HITS <- as.numeric(as.character(dfs$HITS))
dfs$HITSA <- as.numeric(as.character(dfs$HITSA))
dfs$HITSD <- as.numeric(as.character(dfs$HITSD))
dfs$BS <- as.numeric(as.character(dfs$BS))
dfs$BSA <- as.numeric(as.character(dfs$BSA))
dfs$BSD <- as.numeric(as.character(dfs$BSD))


dfs$Opponent <- as.character(dfs$Opponent)
#str(dfs)
#str(teams_abrv)
teams_abrv <- as_tibble(teams_abrv)

#adding new columns
for (i in 1:nrow(dfs)) {
  for (j in 1:nrow(teams_abrv)) {
    if (dfs[i, 3] == teams_abrv[j, 1]) {
      dfs[i,28] <- teams_abrv[j, 4]
      dfs[i,29] <- teams_abrv[j, 3]
      dfs[i,30] <- teams_abrv[j, 2]
    }
  }
}

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
dfs$Result  <- factor(dfs$Result, levels = rev(c("Win", "OT Win", "SO Win", "SO Loss", "OT Loss", "Loss")))

#renaming new columns
#colnames(dfs)[28] <- "CONF"
#colnames(dfs)[29] <- "DIV"
#colnames(dfs)[30] <- "TM"

#datexaxis <-
# seq(as.Date(dfs$Date[length(dfs$Date)], format = "%Y-%m-%d"),
#    as.Date(dfs$Date[1], format = "%Y-%m-%d") + 10,
#    by = "week")

point <- ggplot(dfs,aes(Date, PTS)) +
      geom_point(shape =21, size = 6, alpha = 0.95, aes(fill= Result, col=Result, group = seq_along(Date)))+
      geom_line(col = teamcolors[idx], lwd = 1.5)+
      theme_ipsum_rc()+
      xlab("Date") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
      scale_fill_brewer(palette = "Spectral", name = "Result")+
      scale_color_brewer(palette = "Spectral", name = "Result")+
      labs(title = teamname,  caption = reference)

point

 ggsave(
  paste0("plots/",teamname,"pointssofar.png"),
  width = 15,
  height = 8.5,
  dpi = 600
)


#This make an animation of the chart above ----
#gganim <- point + transition_reveal(Date)

#animate(gganim, height = 720, width = 1280, nframes = 225, fps =25 ,
#       end_pause = 30, renderer = gifski_renderer(paste0(teamname,".gif")))


#save pct one color ----
dfs %>%
  ggplot(aes(Date, SV.)) +
  geom_path(stat = "identity", col = "#00205b") +
  geom_point(
    shape = 21,
    size = 2,
    col = "#00205b",
    fill = "#00205b"
  ) +
  theme_ipsum_rc(grid = "X,y,Y") +
  # theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
  scale_color_ft() +
  # scale_colour_manual(values =  c("#ffffff", "#00205b")) +
  # scale_fill_manual(values =  c("#ffffff", "#00205b"),
  # aesthetics = c("colour", "fill")) +
  scale_y_percent() +
  # scale_x_date(date_labels = "%d-%m", breaks = datexaxis) +
  ylab("Save %") +
  xlab("Date") +
  labs(caption = reference) +
  ggtitle("Save %", subtitle = "2019-20 Regular Season") +
  geom_label(
    col = "black",
    data = dfs %>% filter(SV. < "0.8"),
    aes(label = Opponent),
    nudge_y =  0.01,
    nudge_x =  -0.01,
    show.legend = FALSE
  )

ggsave(
  "plots/savepercentofleafs.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# goal differential ----
dfs$color <- ifelse(dfs$GD >= 0, '1', '0')

dfs %>%
  ggplot(aes(Date, fill = color, color = color)) +
  geom_col(aes(y = GD)) +
  theme_ipsum_rc(grid = "X,Y") +
  theme(legend.position = "none")+
  scale_colour_manual( values =  c( "#fcb514","#00205b")) +
  scale_fill_manual( values =  c( "#fcb514","#00205b"),  aesthetics = c( "fill")) +
  scale_y_continuous(limits = c(-6,6), breaks = c(-6:6))+
  ylab("Net Goals") +
  labs(caption = reference) +
  ggtitle("Goal Differential per Game", subtitle="2019-20 Regular Season")
#geom_label_repel(data=dfs %>% filter(GD <= -3), aes(label = as.character(Opponent)))

ggsave(
  "plots/netgoals2019-20_leafs.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# sh% in funtion to sv% ----
# TODO plot line of PDO = 102
pdoc <- dfs$SH. + dfs$SV.
dfs %>%
  mutate(pdoc = SH. + SV.)%>%
  ggplot(aes(SH., SV., col = Result, fill = Result)) +
  geom_abline(intercept = 1, slope = -1, col = "#00205b")+
  geom_point(shape = 21, size = 10 ) +
  theme_ipsum_rc() +
  theme(legend.position = "left") +
  scale_y_percent() +
  scale_x_percent() +
  scale_fill_viridis_d()+
  ylab("SV%") +
  xlab("SH%") +
  labs(caption = reference) +
  ggtitle("SV% in Relation to SH%", subtitle = "2019-20 Regular Season")+
  annotate("text", label = "PDO = 100", x = 0.2, y = 0.78, size = 5, colour = "#00205b")

ggsave(
  "plots/svpct_shpct.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# PDO per game ----
dfs %>%
  ggplot(aes(Rk)) +
  geom_line(stat = "identity", color = "#00205b", aes(y= PDO)) +
  geom_point(aes(y = PDO, fill = Result),
    shape = 21,
    size = 10,
    col = "#00205b"
  ) +
  theme_ipsum_rc()+
  theme(legend.position = "left") +
  scale_fill_viridis_d() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("PDO per Game", subtitle="2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "plots/pdo_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# save% with results ----
dfs %>%
  ggplot(aes(Rk, SV.)) +
  #geom_line(stat = "identity", color = "#ffffff") +
  geom_hline(yintercept = 0.905, color = "deepskyblue", lwd = 2)+
  geom_point(aes(fill = Result),
    shape = 21,
    size = 10,
    col = "#00205b"
  ) +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent(breaks = scales::pretty_breaks(n = 6)) +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("SV% per Game", subtitle ="2019-20 Regular Season")+
  annotate("text", label = "Lg. Avg.", x = 1.5, y = 0.915, size = 5, colour = "#001233")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "plots/psvpct_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# average shots % in results and location ----

dfs %>%
  ggplot(aes(Result,SF)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  # scale_y_percent() +
  xlab("Results") +
  labs(caption = reference) +
  ggtitle("SF in Relation to Results", subtitle="2019-20 Regular Season")+
   geom_label(
     color = "#ffffff", fill = "#669cff",
     data = dfs %>% filter(SF > 50),
     aes(label = Team),
     nudge_y =  -2,
     nudge_x =  0,
     show.legend = FALSE
   )

ggsave(
  "plots/sf_result_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Loc., SF)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("SF in Relation to Location", subtitle="2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "plots/sf_loc_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# SH% against all opponents
dfs %>%
  ggplot(aes(Team, SF)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  xlab("Team") +
  labs(caption = reference) +
  ggtitle("SF Against All Opponents",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/shpct_tm_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Div, SH.)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Division") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Divisions",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/shpct_div_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Conf, SH.)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Conference") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Conferences",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/shpct_conf_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# SV% against opponents ----
dfs %>%
  ggplot(aes(Conf, SV.)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Conference") +
  labs(caption = reference) +
  ggtitle("Average SV% Against Conferences",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/svpct_conf_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Div, SH.)) +
  geom_boxplot(color = "#00205b", fill = "#669cff") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Division") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Division",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/shpct_div_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Team, SH.)) +
  geom_bar(stat = "summary", fun.y = mean, color = "#00205b", fill = "#669cff") +
  #facet_wrap(vars(DIV), scales = "free_x") +
  theme_ipsum_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  coord_cartesian(ylim = c(0, 0.18))+
  scale_y_percent(breaks = scales::pretty_breaks(n = 10))+
  #scale_y_percent() +
  xlab("Team") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Opponents",
          subtitle = "2019-20 Regular Season")

ggsave(
  "plots/shpct_tm_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

# number of goals in relation to result of game

dfs %>%
  ggplot(aes(Result,GF, fill= Result)) +
  geom_boxplot(
    color = "#00205b"#, fill = "#1313fb") +
  )+
  #geom_jitter(shape=21, position=position_jitter(0.2), size = 3, fill="#ffffff")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 0.3)+
   theme_ipsum_rc()+
  xlab("Result") +
  scale_fill_brewer(palette = "RdBu", name = "Nº Games")+
  labs(caption = reference) +
  ggtitle("Outcome of Game by Number of Goals Scored For",
          subtitle = "2019-20 Regular Season")
   # geom_label(color = "#ffffff", fill = "#1313fb",
   #   data = dfs %>% filter(GF > 6),
   #   aes(label = TM),
   #   nudge_y =  0.01,
   #   nudge_x =  -0.2,
   #   show.legend = FALSE
   # )

ggsave(
  "plots/goals_results_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)

dfs %>%
  ggplot(aes(Result,GA, fill= Result)) +
  geom_boxplot(
    color = "#00205b"#, fill = "#1313fb") +
  )+
  #geom_jitter(shape=21, position=position_jitter(0.2), size = 3, fill="#ffffff")+
  geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 0.3)+
   theme_ipsum_rc()+
  xlab("Result") +
  scale_fill_brewer(palette = "RdBu", name = "Nº Games")+
  labs(caption = reference) +
  ggtitle("Outcome of Game by Number of Goals Scored Against",
          subtitle = "2019-20 Regular Season")
   # geom_label(color = "#ffffff", fill = "#1313fb",
   #   data = dfs %>% filter(GF > 6),
   #   aes(label = TM),
   #   nudge_y =  0.01,
   #   nudge_x =  -0.2,
   #   show.legend = FALSE
   # )

ggsave(
  "plots/goalsagt_result_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)


ggplot(dfs,aes(Date, PTS)) +
geom_point(shape =21, size = 6, alpha = 0.95, aes(fill= Result, col=Result, group = seq_along(Date)))+
geom_line(col = "#00205b", lwd = 1.5)+
theme_ipsum_rc()+
xlab("Date") +
scale_y_continuous(breaks = scales::pretty_breaks(n = 8))+
scale_fill_brewer(palette = "Spectral", name = "Result")+
scale_color_brewer(palette = "Spectral", name = "Result")+
labs(caption = reference) +
ggtitle("How Leafs got its Points this Season")

ggsave(
  "plots/points_date_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = 600
)
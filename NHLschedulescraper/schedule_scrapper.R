library(rvest)
library(dplyr)
library(magrittr)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggrepel)
library(readr)

teams_abrv <- read.csv("teams_abrev.csv")
# URL ----
#regular season page
leafswebpage <-
  read_html(
    "https://www.quanthockey.com/nhl/team-game-logs/toronto-maple-leafs-2019-20-nhl-game-log.html"
  )
# playoffs format
# team.playoff.html <- read_html("https://www.quanthockey.com/nhl/team-game-logs/st-louis-blues-2018-19-nhl-playoff-game-log.html")
#player stats first 50
#https://www.quanthockey.com/nhl/seasons/2019-20-nhl-players-stats.html

# Scraping data ----
datetoday <- Sys.Date()
reference <- sprintf("Source: www.quanthockey.com, %s", datetoday)
webdata <- leafswebpage %>%
  html_nodes("td") %>%
  html_text()
teamname <- leafswebpage %>%
  html_nodes("title") %>%
  html_text()
titles <-  leafswebpage %>%
  html_nodes("th") %>%
  html_text()

stat_name <- gsub("%", ".", titles[9:35])
stat_name <- gsub("-", "", stat_name)

# extracting teams name
teamname <- str_remove(teamname, "( G.*)")
#
# creating a data frame ----
dfs <-matrix(
    webdata,
    ncol = 27,
    byrow = T,
    dimnames = list(NULL, stat_name)
  )

dfs <- as.data.frame(dfs)

#dfs <- tbl_df(dfs) # dplyr local data frame

# formatting data :( ----
dfs$Rk <- as.numeric(as.character(dfs$Rk))
#dfs$Team <- rep(teamname, length(dfs$Rk))
dfs$Date <- as.Date(dfs$Date, format = "%Y-%m-%d")
dfs$PDO <- as.numeric(sub("%","",dfs$PDO))/100
dfs$PDOA <- as.numeric(sub("%","",dfs$PDOA))/100
dfs$SH. <- as.numeric(sub("%","",dfs$SH.))/100
dfs$SH.A <- as.numeric(sub("%","",dfs$SH.A))/100
dfs$FO. <- as.numeric(sub("%","",dfs$FO.))/100
dfs$SV. <- as.numeric(sub("%","",dfs$SV.))/100
dfs$SV.A <- as.numeric(sub("%","",dfs$SV.A))/100


dfs$GD <- as.numeric(as.character(dfs$GD))

dfsfact <- stat_name[3:5]
dfs[dfsfact] <- lapply(dfs[dfsfact], factor)

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


str(dfs)
str(teams_abrv)

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
#renaming new columns
colnames(dfs)[28] <- "CONF"
colnames(dfs)[29] <- "DIV"
colnames(dfs)[30] <- "TM"

#datexaxis <-
# seq(as.Date(dfs$Date[length(dfs$Date)], format = "%Y-%m-%d"),
#    as.Date(dfs$Date[1], format = "%Y-%m-%d") + 10,
#    by = "week")

# save pct one color ----
dfs %>%
  ggplot(aes(Date, SV.)) +
  geom_path(stat = "identity", col = "#ffffff") +
  geom_point(
    shape = 21,
    size = 2,
    col = "#ffffff",
    fill = "#ffffff"
  ) +
  theme_ft_rc(grid = "X,y,Y") +
  theme(axis.text.y = element_text(angle = 90, hjust = 1)) +
  scale_color_ft() +
  # scale_colour_manual(values =  c("#ffffff", "#00205b")) +
  # scale_fill_manual(values =  c("#ffffff", "#00205b"),
  # aesthetics = c("colour", "fill")) +
  scale_y_percent() +
  # scale_x_date(date_labels = "%d-%m", breaks = datexaxis) +
  ylab("Save %") +
  xlab("Date") +
  labs(caption = reference) +
  ggtitle("Save % in the 2019-20 Regular Season") +
  geom_label(
    col = "black",
    data = dfs %>% filter(SV. < "0.8"),
    aes(label = Opponent),
    nudge_y =  0.01,
    nudge_x =  -0.01,
    show.legend = FALSE
  )

ggsave(
  "savepercentofleafs.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# goal differential ----
dfs$color <- ifelse(dfs$GD >= 0, '1', '0')

dfs %>%
  ggplot(aes(Date, fill = color, color = color)) +
  geom_col(aes(y = GD)) +
  theme_ft_rc(grid = "X,Y") +
  theme(legend.position = "none")+
  scale_colour_manual( values =  c( "#fcb514","#00205b")) +
  scale_fill_manual( values =  c( "#fcb514","#00205b"),  aesthetics = c( "fill")) +
  scale_y_continuous(limits = c(-6,6), breaks = c(-6:6))+
  ylab("Net Goals") +
  labs(caption = reference) +
  ggtitle("Net Goals in the 2019-20 Regular Season")
#geom_label_repel(data=dfs %>% filter(GD <= -3), aes(label = as.character(Opponent)))

ggsave(
  "Netgoals2019-20_leafs.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# sh% in funtion to sv% ----
dfs %>%
  ggplot(aes(SH., SV., col = Result, fill = Result)) +
  geom_point(shape = 21, size = 5 ) +
  theme_ft_rc() +
  scale_y_percent() +
  scale_x_percent() +
  ylab("SV%") +
  xlab("SH%") +
  labs(caption = reference) +
  ggtitle("SV% in Relation to SH% in the 2019-20 Regular Season")
#geom_label_repel(data=dfs %>% filter(GD <= -3), aes(label = as.character(Opponent)))

ggsave(
  "svpct_shpct.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# PDO per game ----
dfs %>%
  ggplot(aes(Rk)) +
  geom_line(stat = "identity", color = "#ffffff", aes(y= PDO)) +
  geom_point(aes(y = PDO, fill = Result),
    shape = 21,
    size = 10,
    col = "#ffffff"
  ) +
  theme_ft_rc()+
  theme(legend.position = "left") +
  scale_fill_viridis_d() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("PDO in the 2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "pdo_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# save% with results ----
dfs %>%
  ggplot(aes(Rk, SV.)) +
  geom_line(stat = "identity", color = "#ffffff") +
  geom_smooth(method = "lm", se = F)+
  geom_point(aes(fill = Result),
    shape = 21,
    size = 10,
    col = "#ffffff"
  ) +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("SV. in the 2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "psvpct_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# average shots % in results and location ----

dfs %>%
  ggplot(aes(Result, SH.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("SH% in the 2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "shpct_result_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

dfs %>%
  ggplot(aes(Loc., SH.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Game") +
  labs(caption = reference) +
  ggtitle("SH% in the 2019-20 Regular Season")
  # geom_label(
  #   col = "black",
  #   data = dfs %>% filter(SV. < "0.8"),
  #   aes(label = Opponent),
  #   nudge_y =  0.01,
  #   nudge_x =  -0.01,
  #   show.legend = FALSE
  # )

ggsave(
  "shpct_loc_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# SH% against all opponents
dfs %>%
  ggplot(aes(TM, SH.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Team") +
  labs(caption = reference) +
  ggtitle("Average SH% Against All Opponents",
          subtitle = "2019-20 Regular Season")

ggsave(
  "shpct_tm_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

dfs %>%
  ggplot(aes(DIV, SH.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Division") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Divisions",
          subtitle = "2019-20 Regular Season")

ggsave(
  "shpct_div_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

dfs %>%
  ggplot(aes(CONF, SH.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Conference") +
  labs(caption = reference) +
  ggtitle("Average SH% Against Conferences",
          subtitle = "2019-20 Regular Season")

ggsave(
  "shpct_conf_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

# SV% against opponents ----
dfs %>%
  ggplot(aes(CONF, SV.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Conference") +
  labs(caption = reference) +
  ggtitle("Average SV% Against Conferences",
          subtitle = "2019-20 Regular Season")

ggsave(
  "svpct_conf_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

dfs %>%
  ggplot(aes(DIV, SV.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Division") +
  labs(caption = reference) +
  ggtitle("Average SV% Against Division",
          subtitle = "2019-20 Regular Season")

ggsave(
  "svpct_div_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

dfs %>%
  ggplot(aes(TM, SV.)) +
  geom_boxplot(color = "#ffffff", fill = "#1313fb") +
  #facet_wrap(vars(DIV), scales = "free_x") +
  theme_ft_rc()+
  theme(legend.position = "left")+
  scale_fill_viridis_d() +
  scale_y_percent() +
  xlab("Team") +
  labs(caption = reference) +
  ggtitle("Average SV% Against All Opponents",
          subtitle = "2019-20 Regular Season")

ggsave(
  "svpct_tm_2019-20.png",
  width = 15,
  height = 8.5,
  dpi = "retina"
)

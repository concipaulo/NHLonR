#! diagnostics off
# load packages ----
Packages <- c("ggthemes","ggrepel","ggExtra","viridis",
              "gridExtra", "hrbrthemes","tidyverse", "gghighlight")
invisible(lapply(Packages, library, character.only = TRUE))

# Functions
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


#teams_abrv <- read.csv("~/NHLonR/teams_abrev.csv", stringsAsFactors = F)
#teams_abrv <- as_tibble(teams_abrv)
datetoday <<- Sys.Date()
reference <<- sprintf("Source: www.hockey-reference.com, %s", datetoday)

}

loaddata <- function() {
  teams_1419 <- read.csv("teams2014-2019rs.csv")
  teams_2020 <- read.csv("teams_2020.csv")
  teams_abrv <- read.csv("teams_abrev.csv")

  # creating data frame ----
  df <- do.call("rbind", list(teams_1419, teams_2020))

  df <- as_tibble(df)
  #df <- data.frame(df)
  df$Year <- as.factor(df$Year)

  # Cleaning teams name and creating the playoff variable ----
  df$Playoff <- (rep("NO", nrow(df)))
  df$Playoff[which(df$Year == "2020")] <- "2019-20"

  # regular expression to match any playoff teams
  rexp <- "[\\w .]+[ ](\\w)*[*]"
  df$Playoff[which(str_detect(df$Teams, rexp))] <- "YES"
  # cleaning teams names ----
  df$Teams <- str_remove_all(df$Teams, "[*]")

  teams_abrv <- as_tibble(teams_abrv)

  # Joinning data frames ----
  for (i in 1:nrow(df)) {
    for (j in 1:nrow(teams_abrv)) {
      if (df[i, 1] == teams_abrv[j, 1]) {
        df[i, 59] <- teams_abrv[j, 4]
        df[i, 60] <- teams_abrv[j, 3]
        df[i, 61] <- teams_abrv[j, 2]
      }
    }
  }
  # renaming columns names ----
  #names(df)[names(df) == 'V59'] <- 'Conf'
  #names(df)[names(df) == 'V60'] <- 'Div'
  #names(df)[names(df) == 'V61'] <- 'Team'

  # analysing data, managing variables ----
  df$Playoff <- as.factor(df$Playoff)
  #df$Teams <- as.factor(df$Teams)

  # creating a new variable called season
  df$Season <- ifelse(df$Year == "2020", '2020', '2014-2019')
  df$Season <- as.factor(df$Season)
  df$Team[which(df$Team == "PHO")] <- "ARI"

  dataset <<- df

}

# Calls
setenv()

loaddata()

# basics ====
plot(dataset$Team, dataset$PTS.)

# ggplot ----
ggplot(dataset, aes(as.factor(Div), PTS., col = Playoff)) +
  geom_boxplot()

# creating a linear regression model
plot(dataset$PTS, dataset$SV.)
modsvpt <- lm(PTS ~ SV., dataset)

plot(modsvpt)

# linear regression on ggplot
ggplot(dataset, aes(PTS, SV.)) +
  geom_point() +
  geom_smooth(method = "lm")

# creating a theme plot ----
# clean <- theme(
#   panel.grid = element_blank(),
#   panel.background = element_rect(fill = "white"),
#   panel.border = element_rect(color = "black", fill = "2019-20", size = 0.2),
#   plot.title = element_text(hjust = 0.5),
#   axis.text = element_text(size = 12),
#   legend.position = "none"
# )


# plotting PTS% in function of vitories ----

ggplot(dataset, aes(W, PTS.)) +
  theme_ipsum_rc() +
  geom_point(
    data = dataset,
    aes(fill = Season, col = Season),
    shape = 21,
    size = 7,
    show.legend = T,
    alpha = 0.6
  ) +
  geom_smooth(data = filter(dataset, Season == "2014-2019"), method = "lm", col = "firebrick", se = F) +
  ylab("PTS%") +
  labs(title = "PTS% in function of victories", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday)) +
  scale_color_wsj(palette = "rgby")
  #gghighlight(Team == "TOR", Year == "2020")
  #scale_fill_viridis_d()+
  # geom_label(fill = "white", data=dataset %>% filter(Team == "TOR" & Year == "2020")
  #            ,aes(label=Team),
  #            nudge_y = 0.02,
  #            show.legend = FALSE)

ggsave(
  paste0("plots/victories_to_ptspercent ",datetoday, " .png"),
  width = 10,
  height = 6,
  dpi = 600
)

ggplot(dataset, aes(Team, PTS.)) +
  geom_boxplot()+
  theme_ipsum_rc() +
  ylab("PTS%") +
  ggtitle("Average PTS%", subtitle = "since 2013-14") +
  labs(caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/ptspct_teams",datetoday, " .png"),
  width = 11,
  height = 6,
  dpi = 600
)


# plot winninf in function of PTS
ggplot(dataset, aes(W, PTS, col = Playoff, fill=Playoff)) +
  geom_point(shape = 20,
    size = 7,
    show.legend = T,
    alpha=0.91
    ) +
  theme_ipsum_rc() +
  scale_color_wsj(palette = "rgby") +
  #scale_fill_viridis_d(direction = -1) +
  ylab("PTS") +
  labs(title = "PTS in function of victories", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/victories_to_pts ", datetoday," .png"),
  width = 11,
  height = 6,
  dpi = 600
)

# verifying the correlation between the two variables
cor(dataset$W, dataset$PTS., method = "pearson")

dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(OL, PTS., col = Playoff, fill = Playoff)) +
  geom_point(shape = 21, size = 7, alpha = 0.6) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se = F) +
  theme_ipsum_rc() +
  scale_color_wsj(palette = "rgby") +
  #scale_color_viridis_d() +
  ylab("PTS%") +
  labs(title = "PTS% in function of OL", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/ol_to_ptspercent ", datetoday, " .png"),
  width = 11,
  height = 6,
  dpi = 600
)

model_points_wins <- lm(W ~ PTS., dataset)
summary(model_points_wins)

dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(Playoff, PTS)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_point(position = position_jitter(), col = "firebrick", size = 7) +
  theme_ipsum_rc(grid = "Y") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj()+
  ylab("PTS") +
  labs(title = "Average Number of Points to Reach the Playoffs", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
   paste0("plots/mean_points_to_playoff ", datetoday, " .png"),
  width = 11,
  height = 6,
  dpi = 600
)

dplyr::filter(dataset, Year != 2020) %>%
  ggplot(aes(as.factor(Div), PTS., col = Playoff)) +
  geom_boxplot(alpha = 1) +
  theme_ipsum_rc(grid = "X,Y") +
  scale_color_wsj() +
  scale_y_percent(breaks = scales::pretty_breaks(n = 7))+
  ylab("PTS%") +
  xlab("Division") +
  labs(title = "Efficiency per division to reach the Playoffs", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/efficiency_per_division ", datetoday, " .png"),
  width = 11,
  height = 6,
  dpi = 600
)

# Goals ----
dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(S, GF)) +
  geom_point(shape = 21, size = 7, aes(fill = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  theme(legend.position = "none")+
  scale_color_wsj(palette = "colors6") +
  ylab("Goals For") +
  xlab("Shots") +
  labs(title = "Goals For in Function of Shots", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

  ggsave(
    paste0("plots/shots_to_goals ", datetoday, " .png"),
    width = 11,
    height = 6,
    dpi = 600
  )

dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(S./100, GF)) +
  geom_point(shape = 21, size = 7, aes(fill = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  theme(legend.position = "none")+
  scale_x_percent(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  ylab("Goals For") +
  xlab("Shooting percentage") +
  labs(title = "Goals For in function of shooting percentage", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

  ggsave(
    paste0("plots/shootingpercentage_to_goals ", datetoday, " .png"),
    width = 11,
    height = 6,
    dpi = 600
  )

cor(dataset$S., dataset$GF, method = "pearson")

dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(S./100, W)) +
  geom_point(shape = 21, size = 7, aes(fill = Playoff, col = Playoff), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  scale_x_percent(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  theme(legend.position = "right") +
  ylab("Wins") +
  xlab("Shooting percentage") +
  labs(title = "Wins in function of shooting percentage", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

  ggsave(
    paste0("plots/wins_to_shpct ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

hist(dataset$S.)
summary(dataset$S.)
cor(dataset$S., dataset$W, method = "pearson")

# Powerplay ----
dplyr::filter(dataset, Playoff != "2019-20") %>%
  ggplot(aes(PPO, GF)) +
  geom_point(shape = 21, size = 7, aes(fill = Season, col = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  theme(legend.position = "none") +
  ylab("Goals For") +
  xlab("Powerplay Opportunities (PPO)") +
  labs(title = "Goals For in function of Powerplay Opportunities", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

  ggsave(
         paste0("plots/ppo_to_goals ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

cor(dataset$PPO, dataset$GF)
hist(dataset$oPIM.G)


dplyr::filter(dataset, Playoff != "2019-20") %>%
  mutate(ppgoalpergame = PP / GP) %>%
  ggplot(aes(oPIMpG, ppgoalpergame)) +
  geom_point(shape = 21, size = 7, aes(fill = Season, col = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  theme(legend.position = "none") +
  ylab("Powerplay Goals per game") +
  xlab("Opposing team penalty minutes per game") +
  labs(title = "Powerplay goals in function of Penalty minutes", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
       paste0("plots/ppg_to_opim ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

cor(dataset$oPIMpG, ppgoalpergame)
mean(ppgoalpergame)
mean(dataset$PP / dataset$GP)
mean(teams_2019$PP.)


ggplot(dataset, aes(Team, oPIMpG)) +
  geom_bar(
    stat = "summary",
    fun.y = "mean",
    fill = "steelblue",
    col = "deepskyblue",
    width = 0.8) +
  theme_ipsum_rc() +
  ylab("Mean penalty minutes of opposing team per game") +
  xlab("Team") +
  labs(title = "Average Penalty Minutes of Opposing Team", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
       paste0("plots/opim_per_team ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

dataset%>%
  group_by(Team) %>%
  summarise(opimg = mean(oPIMpG), pimg = mean(PIMpG)) %>%
  mutate(pim = opimg - pimg,
         color = ifelse(pim >= 0, '1', '0')) %>%

ggplot(aes(x = Team, y = pim, fill = color)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  theme_ipsum_rc(grid = "Y") +
  scale_color_wsj(palette = "colors6") +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  ylab("Average penalty minutes per game (minutes)") +
  xlab("Team") +
  labs(title = "Average PIM/G", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
        paste0("plots/avgpim ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

# goal diff ----
# really necessary?


dataset %>%
  mutate(goaldif = GF - GA)%>%
  ggplot(aes(Team, goaldif, fill = as.factor(Year), label = goaldif)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ipsum_rc() +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Goal Differential") +
  xlab("Team") +
  scale_fill_viridis_d()+
  labs(title = "Goal Differential in Regular Season", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
       paste0("plots/goaldif_2020 ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)



dataset %>%
  mutate(shotdif = S - SA)%>%
  ggplot(aes(Team, shotdif, fill = as.factor(Year), label = shotdif)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ipsum_rc() +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Shot Differential") +
  xlab("Team") +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(-2000, 2000, 500))+
  labs(title = "Shot Differential in Regular Season", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
       paste0("plots/shotdif_2020 ", datetoday, " .png"),
         width = 11, height = 6, dpi =600)

dataset %>%
  filter(Playoff != "2019-20") %>%
  mutate(shotdif = S - SA, goaldif = GF - GA)%>%
  ggplot(aes(shotdif, goaldif)) +
  geom_point(shape = 21, size = 7, aes(fill = Season, col = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  theme(legend.position = "none") +
  ylab("Goal Differential") +
  xlab("Shot Differential") +
  labs(title = "Correlation between Goal Differential and Shot Differential", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/shotdif_to_goaldiff ", datetoday, " .png"),
  width = 11, height = 6, dpi =600)


#dataset.w <- aggregate(W ~ Div, dataset, mean)
# the above do the same as group_by and summarise
dataset%>%
  group_by(Div)%>%
  summarise(victories = mean(W))%>%
  ggplot(aes(Div, victories, label = round(victories, 2))) +
  geom_col(fill = "steelblue", col = "deepskyblue", width = 0.8) +
  theme_ipsum_rc() +
  ylab("Mean of Victories") +
  xlab("Division") +
  geom_text(size = 7,
            position = position_stack(vjust = 0.5),
            col = "white")+
  labs(title = "Mean Victories per Season", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
 paste0("plots/mean_victories ", datetoday, " .png"),
  width = 11, height = 6, dpi =600)



ggplot(dataset, aes(PIMpG)) +
  geom_histogram(fill = "steelblue",
                 col = "deepskyblue",
                 bins = 15) +
  theme_ipsum_rc() +
  ylab("Frequency") +
  xlab("Penalty Minutes per Game") +
  labs(title = "Mean of Penalty Minutes per Game", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
 paste0("plots/mean_penalty_minutes ", datetoday, " .png"),
  width = 11, height = 6, dpi =600)

ggplot(dataset, aes(PPA)) +
  geom_histogram(fill = "steelblue",
                 col = "deepskyblue",
                 bins = 15) +
  theme_ipsum_rc() +
  ylab("Frequency") +
  xlab("Goals while Shorthanded") +
  labs(title = "Mean of Shorthanded Goals per Game", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
  paste0("plots/mean_short_goals ", datetoday, " .png"),
  width = 11, height = 6, dpi =600)

mean(dataset$PPA)

# Points total ----
dataset %>%
  ggplot(aes(Team, PTS, fill = Year)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ipsum_rc() +
  scale_fill_viridis_d(direction =-1) +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Points") +
  xlab("Team") +
  labs(title = "Points in Regular Season", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))

ggsave(
        paste0("plots/pts_2020 ", datetoday, " .png"),
  width = 11, height = 6, dpi =600)

# Analising teams defensive stats ----
# you have to include a new column if you want to put label in it
dataset$tgevg<- (dataset$EVGA + dataset$EVGF)/dataset$GP
dataset$tgevg

dataset%>%
  ggplot(aes(tgevg, TGpG)) +
  geom_point(shape = 21, size = 7, aes(fill = Season, col = Season), alpha = 0.6) +
  geom_smooth(method = "lm", col = "firebrick", se =F) +
  theme_ipsum_rc() +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_color_wsj(palette = "colors6") +
  xlab("TGEV.G")+
  ylab("TG.G") +
  labs(title = "Total Goals in function of Total Goals Even Strength", subtitle = "since 2013-14",
       caption = paste0("Source: hockey-reference.com ", datetoday))
  scale_fill_viridis_d()+
  geom_label(data = dataset%>%filter(tgevg >= 5.0 & Year == "2020"),aes(label = Team) )

ggsave(
  "plots/victories_to_ptspercent.png",
  width = 10,
  height = 6,
  dpi = 600
)

# plot winninf in function of PTS

ggplot(dataset, aes(GA, PTS, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T,
    alpha=0.5
    ) +
  theme_ipsum_rc() +
  scale_fill_viridis_d() +
  ylab("PTS") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS in Function of Goals Allowed since 2013-14")+
  geom_label(fill = "black", data=dataset %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 4,
             show.legend = FALSE
  )

ggsave(
  "plots/GA_to_pts.png",
  width = 10,
  height = 6,
  dpi = 600
)

# losses per game played
# you have to include a new column if you want to put label in it
# only if you want to label with
dataset$lep <- (dataset$L + dataset$OL)/dataset$GP

dataset %>%
    ggplot(aes(W, lep, col = Playoff, fill = Playoff)) +
    geom_point(shape = 21,size = 4, show.legend = T,alpha = 0.5) +
    geom_smooth(method = "lm",col = "deepskyblue",fill = "deepskyblue2",se = F) +
    theme_ipsum_rc() +
    scale_fill_viridis_d() +
    xlab("Losses") +
    ylab("Losses %") +
    labs(caption = "Source: hockey-reference.com, 02-18-2020") +
    ggtitle("PTS% in Function of Losses per Game since 2013-14")+
    geom_label_repel(fill = "black",data = dataset %>% filter(Team == "TOR" & Year == "2020"),
                    aes(label = Team),nudge_y = 0.02,show.legend = FALSE)

ggsave("plots/pts_to_lep.png",width = 10,height = 6,dpi = 600)

cor(dataset$PTS., dataset$lep)

dataset$TGApG <- dataset$GA/dataset$GP

dataset%>%
  ggplot(aes(W, TGApG, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21, size = 4, show.legend = T, alpha=0.95) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se = F) +
  theme_ipsum_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("TGA/G") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("TGA/G in Function of Victories since 2013-14")+
  geom_label(fill = "black", data=dataset %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team), show.legend = FALSE,
             nudge_y = 0.07)+
  geom_label(fill = "black", data=dataset %>% filter(TGApG <= "2.6" & Year == "2020")
             ,aes(label=Team),show.legend = FALSE,
             nudge_y = 0.07)


ggsave("plots/goals_allow_victory.png",width = 10,height = 6,dpi = 600)

dataset$TGFpG  <- dataset$GF/dataset$GP

dataset%>%
ggplot(aes(W, TGFpG, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,size = 4,show.legend = T, alpha=0.99) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ipsum_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("TGF/G") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("TGF/G in Function of Victories since 2013-14")+
  geom_label(fill = "black", data=dataset %>% filter(TGFpG >= "3.5" & Year == "2020")
             ,aes(label=Team),nudge_y = 0.1,show.legend = FALSE)

ggsave("plots/goals_allow_victory.png",width = 10,height = 6,dpi = 600)

# advanced stats where recorded only from 2016-17 forward. ----

dataset%>%filter(as.character(Year) >= "2017")%>%
ggplot(aes(W, EVCF., col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,size = 4,show.legend = T, alpha=0.99) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ipsum_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("CF %") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("CF% in Function of Victories since 2013-14")+
  geom_label(fill = "black", data=dataset%>% filter(EVCF. >= "53" & Year == "2020")
             ,aes(label=Team),nudge_y = - 0.5,show.legend = FALSE)

ggsave("plots/corsipct_victory.png",width = 10,height = 6,dpi = 600)


dataset%>%filter(as.character(Year) >= "2017")%>%
ggplot(aes(W, EVPDO, col = Playoff, fill = Playoff)) +
  geom_point(shape = 21, size = 4, show.legend = T, alpha=0.99) +
  geom_smooth(method = "lm",col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ipsum_rc() +
  #scale_shape_manual(values=c(15,16,17,18))+
  #scale_color_manual(values=c('#800040','#ffff00', '#00cc00', "blue", "red", "black", "orange"))+
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("PDO") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PDO in Function of Victories since 2016-17")+
  geom_label_repel(fill = "black", data=dataset %>% filter(EVPDO < 99 & Playoff == "YES")
             ,aes(label = paste(Team, Year)),
             nudge_x = - 1.5,
             nudge_y = -0.3,
             show.legend = FALSE
  )+
  geom_label_repel(fill = "black", data=dataset %>% filter(EVPDO >= 101 & Year == "2020")
             ,aes(label= Team),
             # nudge_x = - 1.5,
             nudge_y = - 0.3,
             show.legend = FALSE
  )

cor(dataset.2017$W, dataset.2017$PDO)

ggsave("plots/pdo_victory.png",width = 10,height = 6,dpi = 600)
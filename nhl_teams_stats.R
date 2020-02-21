# !diagnostics off
# load packages ----
Packages <- c(
  "dplyr",
  "magrittr",
  "ggplot2",
  "ggthemes",
  "readr",
  "stringr",
  "ggrepel",
  "ggExtra",
  "viridis",
  "MASS",
  "gridExtra",
  "ggthemr"
)
invisible(lapply(Packages, library, character.only = TRUE))

# more themes ----
# be carrefull with the functions that new packages masks.
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# loading data ----
teams_2014 <- read.csv("teams_2014.csv")
teams_2015 <- read.csv("teams_2015.csv")
teams_2016 <- read.csv("teams_2016.csv")
teams_2017 <- read.csv("teams_2017.csv")
teams_2018 <- read.csv("teams_2018.csv")
teams_2019 <- read.csv("teams_2019.csv")
teams_2020 <- read.csv("teams_2020.csv")
teams_abrv <- read.csv("teams_abrev.csv")

# add year, we have to put manually because the data was scrapped over the
# years
teams_2014$Year <- cbind(Year = rep(2014, nrow(teams_2014)))
teams_2015$Year <- cbind(Year = rep(2015, nrow(teams_2015)))
teams_2016$Year <- cbind(Year = rep(2016, nrow(teams_2016)))
teams_2017$Year <- cbind(Year = rep(2017, nrow(teams_2017)))
teams_2018$Year <- cbind(Year = rep(2018, nrow(teams_2018)))
teams_2019$Year <- cbind(Year = rep(2019, nrow(teams_2019)))
teams_2020$Year <- cbind(Year = rep(2020, nrow(teams_2020)))

# creating data frame ----
df <-
  Reduce(
    function(x, y)
      merge(x, y, all = TRUE),
    list(
      teams_2014,
      teams_2015,
      teams_2016,
      teams_2017,
      teams_2018,
      teams_2019,
      teams_2020
    )
  )

df <- data.frame(df)
df <- tbl_df(df) # dplyr local data frame

df$Year <- as.factor(df$Year)

# Cleaning teams name and creating the playoff variable ----
df$Playoff <- (rep("NO", nrow(df)))
df$Playoff[which(df$Year == "2020")] <- "2019-20"

# regular expression to match any playoff teams
rexp <- "[\\w .]+[ ](\\w)*[*]"

df$Playoff[which(str_detect(df$Teams, rexp))] <- "YES"
# cleaning teams names ----
df$Teams <- str_remove_all(df$Teams, "[*]")

teams_abrv <- as.data.frame(teams_abrv)
df <- as.data.frame(df)

# Joinning data frames ----
for (i in 1:nrow(df)) {
  for (j in 1:nrow(teams_abrv)) {
    if (df[i, 2] == teams_abrv[j, 1]) {
      df[i, 60] <- teams_abrv[j, 4]
      df[i, 61] <- teams_abrv[j, 3]
      df[i, 62] <- teams_abrv[j, 2]
    }
  }
}
# renaming columns names ----
names(df)[names(df) == 'V60'] <- 'Conf'
names(df)[names(df) == 'V61'] <- 'Div'
names(df)[names(df) == 'V62'] <- 'Team'

# analysing data, managing variables ----
df$Playoff <- as.factor(df$Playoff)
df$Rk <- as.factor(df$Rk)
df$Teams <- as.factor(df$Teams)
# creating a new variable called season
df$Season <- ifelse(df$Year == "2020", '2020', '2014-2019')


str(df)

# basics ====
plot(df$Team, df$PTS.)

plot(teams_2019$W, teams_2019$PTS., col = "blue")

# ggplot ----
ggplot(df, aes(as.factor(Div), PTS., col = Playoff)) +
  geom_boxplot()

# creating a linear regression model
plot(df$PTS, df$SV.)
modsvpt <- lm(PTS ~ SV., df)

plot(modsvpt)

# linear regression on ggplot
ggplot(df, aes(PTS, SV.)) +
  geom_point() +
  geom_smooth(method = "lm")

# creating a theme plot ----
clean <- theme(
  panel.grid = element_blank(),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = "2019-20", size = 0.2),
  plot.title = element_text(hjust = 0.5),
  axis.text = element_text(size = 12),
  legend.position = "none"
)


# plotting PTS% in function of vitories ----

ggplot(df, aes(W, PTS., fill = Season)) +
  theme_ft_rc() +
  geom_point(
    data = df,
    shape = 21,
    size = 2,
    show.legend = T
  ) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  ylab("PTS%") +
  ggtitle("PTS% in function of victories since 2013-14") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  scale_fill_viridis_d()+
  geom_label(data=df %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             nudge_x = - 1.5,
             show.legend = FALSE)

ggsave(
  "victories_to_ptspercent.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# plot winninf in function of PTS
ggplot(df, aes(W, PTS, col = Playoff, fill=Play)) +
  geom_point(shape = 21,
    size = 2,
    show.legend = T, 
    alpha=0.5
    ) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  ylab("PTS") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS in function of victories since 2013-14")
ggsave(
  "victories_to_pts.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# verifying the correlation between the two variables
cor(df$W, df$PTS., method = "pearson")

dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(OL, PTS., col = Playoff, fill = Playoff)) +
  geom_point(shape = 21, size = 2,) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  scale_color_viridis_d() +
  ylab("PTS%") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS% in function of OL since 2013-14")
ggsave(
  "ol_to_ptspercent.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

model_points_wins <- lm(W ~ PTS., df)
summary(model_points_wins)

dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(Playoff, PTS)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  geom_point(position = position_jitter(), col = "orangered") +
  theme_ft_rc() +
  ylab("PTS%") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS% in function of number of victories since 2013-14")
ggsave(
  "mean_points_to_Playoff.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(as.factor(Div), PTS., col = Playoff)) +
  geom_boxplot(alpha = 0) +
  theme_ft_rc() +
  scale_color_wsj() +
  ylab("Mean of PTS%") +
  xlab("Division") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Mean efficiency per division to reach the Playoff since 2013-14")
ggsave(
  "efficiency_per_division.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# Goals ----
dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(S, GF)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  scale_fill_ft() +
  ylab("Goals For") +
  xlab("Shots") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Goals For in function of shots since 2013-14") +
  ggsave(
    "shots_to_goals.png",
    width = 10,
    height = 6,
    dpi = "retina"
  )

dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(S., GF)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  ylab("Goals For") +
  xlab("Shooting percentage") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Goals For in function of shooting percentage since 2013-14") +
  ggsave(
    "shootingpercentage_to_goals.png",
    width = 10,
    height = 6,
    dpi = "retina"
  )

cor(df$S., df$GF, method = "pearson")

dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(S., W, col = Playoff)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  theme(legend.position = "right") +
  ylab("Wins") +
  xlab("Shooting percentage") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Wins in function of shooting percentage since 2013-14") +
  ggsave("wins_to_goals.png", width = 10, height = 6)

hist(df$S.)
summary(df$S.)
cor(df$S., df$W, method = "pearson")

# Powerplay ----
dplyr::filter(df, Playoff != "2019-20") %>%
  ggplot(aes(PPO, GF)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  ylab("Goals For") +
  xlab("Powerplay Opportunities(PPO)") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Goals For in function of Powerplay Opportunities since 2013-14") +
  ggsave("ppo_to_goals.png",
         width = 10,
         height = 6,
         dpi = "retina")

cor(df$PPO, df$GF)
hist(df$oPIM.G)

ppgoalpergame <- as.array(df$PP / df$GP)

ggplot(df, aes(oPIM.G, ppgoalpergame)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  ylab("Powerplay Goals per game") +
  xlab("Opposing team penalty minutes per game") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Powerplay goals in function of Penalty minutes since 2013-14") +
  ggsave("ppg_to_opim.png",
         width = 10,
         height = 6,
         dpi = "retina")

cor(df$oPIM.G, ppgoalpergame)
mean(ppgoalpergame)
mean(df$PP / df$GP)
mean(teams_2019$PP.)


ggplot(df, aes(Team, oPIM.G)) +
  geom_bar(
    stat = "summary",
    fun.y = "mean",
    fill = "deepskyblue",
    col = "deepskyblue",
    width = 0.8
  ) +
  theme_ft_rc() +
  ylab("Mean of penalty minutes of opposing team per game") +
  xlab("Team") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Mean of Penalty minutes of opposing team since 2013-14")
ggsave(
  "opim_per_team.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

netpmin <- as.array(df$oPIM.G - df$PIM.G)
ggplot(df, aes(Team, netpmin)) +
  geom_bar(
    stat = "summary",
    fun.y = "mean",
    fill = "deepskyblue",
    col = "deepskyblue",
    width = 0.8
  ) +
  theme_ft_rc() +
  ylab("Mean of penalty minutes team per game") +
  xlab("Team") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Net penalty since 2013-14")
ggsave(
  "net_mean_pim_per_team.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

df$pim <- (df$oPIM.G - df$PIM.G)
dfg <- aggregate(pim ~ Team, df, mean)
dfg$color <- ifelse(dfg$pim >= 0, '1', '0')

dfg %>%
  ggplot(aes(x = Team, y = pim, fill = color)) +
  geom_bar(stat = "summary", fun.y = "mean") +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(size = 0.1)) +
  ylab("Net penalty minutes per game") +
  xlab("Team") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Net PIM/G since 2013-14")
ggsave("netpim.png",
       width = 10,
       height = 6,
       dpi = "retina")

# goal diff ----
summary(df)
df$goaldif <- df$GF - df$GA
df$Team[which(df$Team == "PHO")] <- "ARI"

df %>%
  ggplot(aes(Team, goaldif, fill = as.factor(Year), label = goaldif)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ft_rc() +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Goal Differential") +
  xlab("Team") +
  ggtitle("Goal Differential in Regular Season since 2013-14") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020 \n * Phoenix Coyotes was rename to Arizona Coyotes in 2014-15") +
  scale_fill_viridis_d() +

ggsave("goaldif_2020.png",
       width = 10,
       height = 6,
       dpi = "retina")


df$shotdif <- df$S - df$SA
df %>%
  ggplot(aes(Team, shotdif, fill = as.factor(Year), label = shotdif)) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ft_rc() +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Shot Differential") +
  xlab("Team") +
  ggtitle("Shot Differential in Regular Season since 2013-14") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  scale_fill_viridis_d() +
  scale_y_continuous(breaks = seq(-2000, 2000, 500))

ggsave("shotdif.png",
       width = 10,
       height = 6,
       dpi = "retina")

df %>%
  ggplot(aes(shotdif, goaldif)) +
  geom_point(shape = 21, size = 2, fill = "Yellow") +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  guides(fill = guide_legend(title = "Year")) +
  ylab("Goal Differential") +
  xlab("Shot Differential") +
  ggtitle(
    "Correlation between Goal Differential and Shot Differential \nin Regular Season since 2013-14"
  ) +
  labs(caption = "Source: hockey-reference.com, 02-18-2020")

ggsave(
  "shotdif_to_goaldiff.png",
  width = 10,
  height = 6,
  dpi = "retina"
)


df.w <- aggregate(W ~ Div, df, mean)

ggplot(df.w, aes(Div, W, label = round(W, 2))) +
  geom_col(fill = "deepskyblue", col = "deepskyblue", width = 0.8) +
  theme_ft_rc() +
  ylab("Mean of Victories") +
  xlab("Division") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Mean Victories per Season since 2013-14") +
  geom_text(size = 7,
            position = position_stack(vjust = 0.5),
            col = "white")
ggsave(
  "mean_victories_per season1.png",
  width = 10,
  height = 6,
  dpi = "retina"
)


ggplot(df, aes(PIM.G)) +
  geom_histogram(fill = "deepskyblue",
                 col = "deepskyblue",
                 binwidth = 0.4) +
  theme_ft_rc() +
  ylab("Frequency") +
  xlab("Penalty Minutes per Game") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Mean of Penalty Minutes per Game since 2013-14")

ggsave(
  "mean_penalty_minutes.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

ggplot(df, aes(PPA)) +
  geom_histogram(fill = "deepskyblue",
                 col = "deepskyblue",
                 binwidth = 1) +
  theme_ft_rc() +
  ylab("Frequency") +
  xlab("Goals while Shorthanded") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("Mean of Shorthanded Goals per Game since 2013-14") +
  ggsave(
    "mean_short_goals.png",
    width = 10,
    height = 6,
    dpi = "retina"
  )

mean(df$PPA)

# Points total ----
df %>%
  ggplot(aes(Team, PTS, fill = as.factor(Year))) +
  geom_bar(stat = "identity",
           position = "dodge",
           col = 0) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  theme(legend.position = "left",
        panel.grid.major.y = element_line(size = 0.1)) +
  guides(fill = guide_legend(title = "Year")) +
  facet_wrap(vars(Div), scales = "free_x") +
  ylab("Points") +
  xlab("Team") +
  ggtitle("Points in Regular Season since 2013-14") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020 \n * Phoenix Coyotes was rename to Arizona Coyotes in 2014-15")
ggsave("pts_2020.png",
       width = 10,
       height = 6,
       dpi = "retina")

# Analising teams defensive stats ----

df$TGEV.G <- (df$EVGA + df$EVGF)/df$GP

df %>%
ggplot(aes(TGEV.G, TG.G, fill = Season)) +
  theme_ft_rc() +
  geom_point(
    data = df,
    shape = 21,
    size = 2,
    show.legend = T
  ) +
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  xlab("TGEV.G")+
  ylab("TG.G") +
  ggtitle("PTS% in function of victories since 2013-14") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  scale_fill_viridis_d()+
  geom_label(col = "purple", data=df %>% filter(TG.G <= "5.5" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = - 0.2,
             show.legend = FALSE
  )+ 
  geom_label(data=df %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = - 0.2,
             show.legend = FALSE
  )

ggsave(
  "victories_to_ptspercent.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# plot winninf in function of PTS

ggplot(df, aes(GA, PTS, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  ylab("PTS") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS in Function of Goals Allowed since 2013-14")+
  geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 4,
             show.legend = FALSE
  )

ggsave(
  "GA_to_pts.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# losses per game played 

df$lep <- (df$L + df$OL)/df$GP

df%>%
ggplot(aes(L, lep, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +
  scale_x_continuous(trans = "reverse")+
  scale_y_continuous(trans = "reverse")+
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  xlab("Losses") +
  ylab("Losses %") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PTS% in Function of Losses per Game since 2013-14")+
  geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 0.02,
             show.legend = FALSE
  )+
  geom_label(fill = "black", data=df %>% filter(lep <= "0.4" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 0.05,
             show.legend = FALSE
  )

ggsave(
  "pts_to_lep.png",
  width = 10,
  height = 6,
  dpi = "retina"
)
cor(df$PTS., df$lep)

df$TGA.G <- df$GA/df$GP

df%>%
ggplot(aes(W, TGA.G, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +

  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2") +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("TGA/G") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("TGA/G in Function of Victories since 2013-14")+
  geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 0.1,
             show.legend = FALSE
  )+
  geom_label(fill = "black", data=df %>% filter(TGA.G <= "2.6" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 0.1,
             show.legend = FALSE
  )

ggsave(
  "goals_allow_victory.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

df$TGF.G  <- df$GF/df$GP

df%>%
ggplot(aes(W, TGF.G, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +
  #scale_x_continuous(trans = "reverse")+
  #scale_y_continuous(trans = "reverse")+
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("TGF/G") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("TGF/G in Function of Victories since 2013-14")+
  # geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
  #            ,aes(label=Team),
  #            # nudge_x = - 1.5,
  #            nudge_y = 0.1,
  #            show.legend = FALSE
  # )+
  geom_label(fill = "black", data=df %>% filter(TGF.G >= "3.5" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = 0.1,
             show.legend = FALSE
  )

ggsave(
  "goals_allow_victory.png",
  width = 10,
  height = 6,
  dpi = "retina"
)

# advanced stats where recorded only from 2016-17 forward. ----
df.2017 <- df%>%
  filter(as.character(Year) >= "2017")

df.2017%>%
ggplot(aes(W, CF., col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +
  #scale_x_continuous(trans = "reverse")+
  #scale_y_continuous(trans = "reverse")+
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("CF %") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("CF% in Function of Victories since 2013-14")+
  # geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
  #            ,aes(label=Team),
  #            # nudge_x = - 1.5,
  #            nudge_y = 0.1,
  #            show.legend = FALSE
  # )+
  geom_label(fill = "black", data=df.2017 %>% filter(CF. >= "53" & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = - 0.5,
             show.legend = FALSE
  )

ggsave(
  "corsipct_victory.png",
  width = 10,
  height = 6,
  dpi = "retina"
)


df.2017%>%
ggplot(aes(W, PDO, col = Playoff, fill=Playoff)) +
  geom_point(shape = 21,
    size = 3,
    show.legend = T, 
    alpha=0.5
    ) +
  #scale_x_continuous(trans = "reverse")+
  #scale_y_continuous(trans = "reverse")+
  geom_smooth(method = "lm", col = "deepskyblue", fill = "deepskyblue2", se=F) +
  theme_ft_rc() +
  scale_fill_viridis_d() +
  xlab("Wins") +
  ylab("PDO") +
  labs(caption = "Source: hockey-reference.com, 02-18-2020") +
  ggtitle("PDO in Function of Victories since 2016-17")+
  # geom_label(fill = "black", data=df %>% filter(Team == "TOR" & Year == "2020")
  #            ,aes(label=Team),
  #            # nudge_x = - 1.5,
  #            nudge_y = 0.1,
  #            show.legend = FALSE
  # )+
  geom_label(fill = "black", data=df.2017 %>% filter(PDO >= 101 & Year == "2020")
             ,aes(label=Team),
             # nudge_x = - 1.5,
             nudge_y = - 0.3,
             show.legend = FALSE
  )

cor(df.2017$W, df.2017$PDO)

ggsave(
  "pdo_victory.png",
  width = 10,
  height = 6,
  dpi = "retina"
)
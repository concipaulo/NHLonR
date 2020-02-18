# load packages ----
Packages <- c("dplyr", "magrittr", "ggplot2", "ggthemes", "readr", 
              "stringr", "ggrepel", "ggExtra", "viridis", "MASS", "gridExtra", "ggthemr")
invisible(lapply(Packages, library, character.only = TRUE))


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
df <- Reduce(function(x, y) merge(x, y, all=TRUE), list(teams_2014, teams_2015, teams_2016, teams_2017, teams_2018, teams_2019, teams_2020))

df$Year <- as.factor(df$Year)

# Cleaning teams name and creating the playoff variable ----
df$Playoff <- (rep("NO", nrow(df)))
df$Playoff[which(df$Year == "2020")] <- NA

# regular expression to match any playoff teams
rexp <- "[\\w .]+[ ](\\w)*[*]"

df$Playoff[which(str_detect(df$Teams, rexp))] <- "YES"
# cleaning teams names ----
df$Teams <- str_remove_all(df$Teams, "[*]")

  # Joinning data frames ----
for (i in 1:nrow(df)) {
  for (j in 1:nrow(teams_abrv)) {
    if (df[i,2] == teams_abrv[j,1]){
      df[i,60] <- teams_abrv[j,4]
      df[i,61] <- teams_abrv[j,3]
      df[i,62] <- teams_abrv[j,2]
    }
  }
}
# renaming columns names ----
names(df)[names(df) == 'V60'] <- 'Conf'
names(df)[names(df) == 'V61'] <- 'Div'
names(df)[names(df) == 'V62'] <- 'Team'

# analysing data, managing variables ----
str(df)
df$Playoff <- as.factor(df$Playoff)
df$Rk <- as.factor(df$Rk)
df$Teams <- as.factor(df$Teams)

# Plottling ----
 # basics ====
plot(df$Team, df$PTS.)

plot(teams_2019$W, teams_2019$PTS., col= "blue")

# ggplot ----
ggplot(df, aes(as.factor(Div), PTS., col= Playoff))+
  geom_boxplot()

# creating a linear regression model 
plot(df$PTS, df$SV.)
modsvpt <- lm(PTS ~ SV., df)

plot(modsvpt)

# linear regression on ggplot
ggplot(df, aes(PTS, SV.))+
  geom_point()+
  geom_smooth(method="lm")

# creating a theme plot ----
clean <- theme(panel.grid = element_blank(),
      panel.background = element_rect(fill="white"),
      panel.border = element_rect(color = "black", fill = "NA", size=0.2),
      plot.title = element_text(hjust = 0.5),
      axis.text=element_text(size=12),
      legend.position="none")

# creating a new variable called season
df$Season <- ifelse(df$Year =="2020", '2020', '2014-2019')

# plotting PTS% in function of vitories ----
ggplot(df, aes(W, PTS., fill=Season))+
  theme_economist()+
  geom_point(data = df, shape=21, size =2, show.legend = T)+
  geom_smooth(method="lm", col="deepskyblue", fill="deepskyblue2")+
  ylab("PTS%")+
  ggtitle("PTS% in function of victories since 2013-14")+
labs(caption = "Source: hockey-reference.com, 02-15-2020")+
  scale_fill_economist()

ggsave("victories_to_ptspercent.png", width=10, height=6)

# more themes ----
# be carrefull with the functions that new packages masks.
library(hrbrthemes)
library(gcookbook)
library(tidyverse)

# plot winninf in function of PTS
ggplot(df, aes(W, PTS, col=Playoff))+
  geom_point()+
  theme_ft_rc() +
  scale_fill_economist()+
  ylab("PTS")+
  ggtitle("PTS in function of victories since 2013-14")
ggsave("victories_to_pts.png", width=10, height=6, dpi = 320)

# verifying the correlation between the two variables
cor(df$W, df$PTS., method = "pearson")

ggplot(df, aes(OL, PTS.))+
  geom_point()+
  geom_smooth(method="lm")+
  clean +
  ylab("PTS%")+
  ggtitle("PTS% in function of OL since 2013-14")
ggsave("ol_to_ptspercent.png", width=10, height=6)

model_points_wins <- lm(W ~ PTS., df)
summary(model_points_wins)

pts_poffs <- ggplot(df, aes(Playoff, PTS))+
  geom_bar(stat = "summary", fun.y="mean", fill="LightBlue")+
  geom_point(position=position_jitter(), col = "red")
  
pts_poffs + theme_economist() +
  ylab("PTS%")+
  ggtitle("PTS% in function of number of victories since 2013-14")
ggsave("mean_points_to_Playoff.png", width=10, height=6)


ggplot(df, aes(as.factor(Div), PTS., col= Playoff))+
  geom_boxplot()+
theme(panel.grid = element_blank(),
      panel.background = element_rect(fill="white"),
      panel.border = element_rect(color = "black", fill = "NA", size=0.2),
      plot.title = element_text(hjust = 0.5),
      axis.text=element_text(size=12),
      axis.title.x = element_text(size=14),
      axis.title.y = element_text(size=14))+
  ylab("Mean of PTS%")+
  xlab("Division")+
  ggtitle("Mean efficiency per division to reach the Playoff since 2013-14")
ggsave("efficiency_per_division.png", width=10, height=6)

# Goals ----
ggplot(df, aes(S, GF))+
  geom_point()+
  geom_smooth(method = "lm")+
  clean +
  ylab("Goals For")+
  xlab("Shots")+
  ggtitle("Goals For in function of shots since 2013-14")
ggsave("shots_to_goals.png", width=10, height=6)

ggplot(df, aes(S., GF))+
  geom_point()+
  geom_smooth(method = "lm")+
  clean +
  ylab("Goals For")+
  xlab("Shooting percentage")+
  ggtitle("Goals For in function of shooting percentage since 2013-14")
ggsave("shootingpercentage_to_goals.png", width=10, height=6)

cor(df$S., df$GF, method = "pearson")

plot_wg <- ggplot(df, aes(S., W, col = Playoff))+
  geom_point()+
  clean +
  theme(legend.position = c(0.9, 0.2))+
  ylab("Wins")+
  xlab("Shooting percentage")+
  ggtitle("Wins in function of shooting percentage since 2013-14")
plot_wg
ggMarginal(plot_wg, type="densigram")

ggsave("wins_to_goals.png", width=10, height=6)

hist(df$S.)
summary(df$S.)
cor(df$S., df$W, method = "pearson")

# Powerplay ----
ggplot(df, aes(PPO, GF))+
  geom_point()+
  geom_smooth(method = "lm")+
  clean+
  ylab("Goals For")+
  xlab("Powerplay Opportunities(PPO)")+
  ggtitle("Goals For in function of Powerplay Opportunities since 2013-14")
ggsave("ppo_to_goals.png", width=10, height=6)
cor(df$PPO, df$GF)

hist(df$oPIM.G)

ppgoalpergame <- as.array(df$PP/df$GP)

ggplot(df, aes(oPIM.G, ppgoalpergame))+
  geom_point(col="deepskyblue1")+
  geom_smooth(method = "lm", col="red", fill="red2")+
  clean +
  ylab("Powerplay Goals per game")+
  xlab("Opposing team penalty minutes per game")+
  ggtitle("Powerplay goals in function of Penalty minutes since 2013-14")

cor(df$oPIM.G, ppgoalpergame)
mean(ppgoalpergame)
mean(df$PP/df$GP)
mean(teams_2019$PP.)

netpmin <- as.array(df$oPIM.G - df$PIM.G)

ggplot(df, aes(Team, oPIM.G))+
  geom_bar(stat="summary", fun.y = "mean",)+
  theme_economist() +
  ylab("Mean of penalty minutes of opposing team per game")+
  xlab("Team")+
  ggtitle("Mean of Penalty minutes of opposing team since 2013-14")
ggsave("opim_per_team.png", width=15, height=6)

ggplot(df, aes(Team, netpmin))+
  geom_bar(stat="summary", fun.y = "mean")+
  clean +
  ylab("Mean of penalty minutes team per game")+
  xlab("Team")+
  ggtitle("Net penalty since 2013-14")
ggsave("net_mean_pim_per_team.png", width=15, height=6)

df$pim <- (df$oPIM.G - df$PIM.G)
df$color <- ifelse(df$pim >= 0, '1', '0')
dfg <- aggregate(pim ~ Team, df, mean)
dfg$color <- ifelse(dfg$pim >= 0, '1', '0')

dfg%>%
  ggplot(aes(x=Team,y=pim, fill=color))+
  geom_bar(stat="summary", fun.y="mean" )+
    theme_economist_white(gray_bg = F) +
    scale_fill_discrete()+
    theme(legend.position = "none", panel.grid.major.y = element_line(size = 0.1))+
    ylab("Net penalty minutes per game") +
    xlab("Team")+
    ggtitle("Net PIM/G since 2013-14")
ggsave("netpim.png", width=15, height=6, type="cairo-png", dpi=320)

# goal diff ----
summary(df)
df$goaldif <- df$GF - df$GA
df$Team[which(df$Team == "PHO")] <- "ARI"

df %>%
  ggplot(aes(Team, goaldif, fill=as.factor(Year), label=goaldif))+
    geom_bar(stat="identity", position = "dodge")+
    theme_economist(base_size = 12, base_family = "Roboto") +
    theme(legend.position = "left", panel.grid.major.y = element_line(size = 0.1) )+
    guides(fill=guide_legend(title="Year"))+
    facet_wrap(vars(Div), scales = "free")+
    ylab("Goal Differential")+
    xlab("Team")+
    ggtitle("Goal Differential in Regular Season since 2013-14")+
    labs(caption = "Source: hockey-reference.com, 02-17-2020 \n * Phoenix Coyotes was rename to Arizona Coyotes in 2014-15")+
    scale_fill_brewer(palette = "Spectral")

ggsave("goaldif_2020.png", width=15, height=6, type="cairo-png", dpi=320)


df$shotdif <- df$S - df$SA
df %>%
  ggplot(aes(Team, shotdif, fill=as.factor(Year), label=shotdif))+
  geom_bar(stat="identity")+
  theme_economist() +
  theme(legend.position = "left", panel.grid.major.y = element_line(size = 0.1) )+
  guides(fill=guide_legend(title="Year"))+
  ylab("Shot Differential")+
  xlab("Team")+
  ggtitle("Shot Differential in Regular Season since 2013-14")+
  labs(caption = "Source: hockey-reference.com, 02-12-2020")+
  scale_fill_brewer(palette = "Spectral")+
  scale_y_continuous(breaks=seq(-2000,2000,500))+
  geom_text(size = 3, position = position_stack(vjust = 0.5), col = "black")

ggsave("shotdif.png", width=15, height=6, type="cairo-png", dpi=320)

df %>%
  ggplot(aes(shotdif, goaldif))+
    geom_point(col="deepskyblue")+
    geom_smooth(method = "lm", col = "red4")+
    theme_economist_white(gray_bg = F) +
    guides(fill=guide_legend(title="Year"))+
    ylab("Goal Differential")+
    xlab("Shot Differential")+
    ggtitle("Correlation between Goal Differential and Shot Differential \nin Regular Season since 2013-14")+
    labs(caption = "Source: hockey-reference.com, 02-12-2020")
    
ggsave("shotdif_to_goaldiff.png", width=15, height=6, type="cairo-png", dpi=320)


df.w <- aggregate(W ~ Div, df, mean)

  ggplot(df.w, aes(Div, W, label=round(W,2))) +
    geom_col( width = 0.8) +
    theme_economist() +
    scale_color_economist()+
    ylab("Mean of Victories")+
    xlab("Division")+
    labs(caption = "Source: hockey-reference.com, 02-17-2020")+
    ggtitle("Mean Victories per Season since 2013-14")+
    geom_text(size = 7, position = position_stack(vjust = 0.5), col="white")
ggsave("mean_victories_per season1.png", width=10, height=6, type="cairo-png", dpi=320)


ggplot(df,aes(PIM.G)) +
  geom_histogram(fill = "#0099ff", binwidth = 0.5) +
  theme_economist() +
  ylab("Frequency")+
  xlab("Penalty Minutes per Game")+
  labs(caption = "Source: hockey-reference.com, 02-12-2020")+
  ggtitle("Mean of Penalty Minutes per Game since 2013-14")+
  # geom_text(size = 5, position = position_stack(vjust = 0.5), col="black")
  ggsave("mean_penalty_minutes.png", width=10, height=6, type="cairo-png", dpi=320)

ggplot(df,aes(PPA)) +
    geom_histogram(fill = "#0099ff", bins = 40) +
    theme_economist() +
    ylab("Frequency")+
    xlab("Goals while Shorthanded")+
    labs(caption = "Source: hockey-reference.com, 02-12-2020")+
    ggtitle("Mean of Shorthanded Goals per Game since 2013-14")+
    # geom_text(size = 5, position = position_stack(vjust = 0.5), col="black")
    ggsave("mean_short_goals.png", width=10, height=6, type="cairo-png", dpi=320)

  mean(df$PPA)  
  
# Points total ----
df %>%
  ggplot(aes(Team, PTS, fill=as.factor(Year)))+
    geom_bar(stat="identity", position = "dodge")+
    theme_economist(base_size = 12, base_family = "Roboto") +
    theme(legend.position = "left", panel.grid.major.y = element_line(size = 0.1) )+
    guides(fill=guide_legend(title="Year"))+
    facet_wrap(vars(Div), scales = "free_x")+
    ylab("Points")+
    xlab("Team")+
    ggtitle("Points in Regular Season since 2013-14")+
    labs(caption = "Source: hockey-reference.com, 02-17-2020 \n * Phoenix Coyotes was rename to Arizona Coyotes in 2014-15")+
    scale_fill_brewer(palette = "Spectral")
    
ggsave("pts_2020.png", width=15, height=6, type="cairo-png", dpi=320)
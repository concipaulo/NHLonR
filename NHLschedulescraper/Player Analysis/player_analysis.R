library(rvest)
library(lubridate)
library(tidyverse)
library(hrbrthemes)
library(ggrepel)

# Functions ----
setenv <- function(){
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

datetoday <<- Sys.Date()
reference <<- sprintf("Source: www.quanthockey.com, %s", datetoday)
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

getplayerdata <- function(ii,jj=ii){

  index <- rep(ii:jj, 1)

  for (i in index) {

    options(warn=-1)
    message(paste0("Retriving player data from ", nhlteams[i]))
    Sys.sleep(2)

  playerwebpage <- read_html(paste0("https://www.quanthockey.com/nhl/teams/",nhlteams[i],"-players-2020-21-nhl-stats.html"), encoding = "latin1")
  playerwebdata <- playerwebpage %>%
    html_nodes("td") %>%
    html_text()

  teamname <- playerwebpage %>%
    html_nodes("title") %>%
    html_text()

  playertitles <<-  playerwebpage %>%
    html_nodes("th") %>%
    html_text()

  stat_player <- gsub("%", ".", playertitles[14:63])
  stat_player <- gsub("/", "p", stat_player)
  teamname <- str_remove(teamname, "( \\@.*)")


  #for players
  dfp <-matrix(
    playerwebdata,
    ncol = 50,
    byrow = T,
    dimnames = list(NULL, stat_player)
  )

  tmname <- rep(teamname, nrow(dfp))
  dfp[,2] <- tmname
  colnames(dfp)[2] <- "Team"
  colnames(dfp)[11] <- "P.M"

  test <<- colnames(dfp)
  dfp <- as_tibble(dfp)
  dfp$PPP. <- as.numeric(sub("%","",dfp$PPP.))/100
  dfp$SH. <- as.numeric(sub("%","",dfp$SH.))/100
  dfp$FO. <- as.numeric(sub("%","",dfp$FO.))/100

  # dfp$TOI <- as.POSIXct(dfp$TOI, format = "%M:%S")

  dfp$TOI <- str_pad(dfp$TOI, width = 5, side = "left", pad = "0")
  dfp$ES <- str_pad(dfp$ES, width = 5, side = "left", pad = "0")
  dfp$PP <- str_pad(dfp$PP, width = 5, side = "left", pad = "0")
  dfp$SH <- str_pad(dfp$SH, width = 5, side = "left", pad = "0")


  dfp$Pos <- as.factor(dfp$Pos)

  stat_p <- colnames(dfp)

  dfp <<- dfp

  # Writing a backup file ----
  write.csv(dfp, file = paste0("Data/",teamname,datetoday,"players.csv"), row.names = F)
  }
  message("\nDone!")
}

readplayerdata <- function(ii, jj = ii, dia){

options(warn=-1)
# teams names

teams <- nhlteams[ii:jj]

tname <- str_to_title(str_replace_all(teams, "-", " "))
tname <- str_replace(tname, "Phoenix", "Arizona")
tname <- str_replace(tname, "St ", "St. ")

#date to read, you can change if you had download previously
#dtdata <- dia
# creating a function to read all files in one go
readplayersdata <- function(name, day){

  dfpcols <-cols(
  .default = col_double(),
  Team = col_character(),
  Name = col_character(),
  Pos = col_factor(),
  Age = col_integer(),
  TOI = col_time(format = "%M:%S"),
  ES = col_time(format = "%M:%S"),
  PP = col_time(format = "%M:%S"),
  SH = col_time(format = "%M:%S"))

  read_csv(paste0("~/NHLonR/NHLschedulescraper/Player Analysis/Data/",name,day,"players.csv"), col_types = dfpcols)
}

teams_abrv <- read_csv("~/NHLonR/teams_abrev2.csv", col_types = cols(.default = col_character()))

#creating a scafold to list the teams to read and days
#dia <- datetoread

#scafold <- tibble(name = tname,
#                  day = dia) %>% tidyr::unnest(cols = c("name", "day"))

scafold <- crossing(tname, dia) %>%
  rename(name = tname, day = dia)
#reading all files into a huge table, the data will be nested in the "data" variable
huge.table <- scafold %>%
  mutate(data = purrr::map2(name, day, ~readplayersdata(.x,.y)))
#unnesting the data
dtset <- huge.table%>% dplyr::select(data, day) %>% tidyr::unnest(cols = c(data))

#adding new variables abbreviation of team name, div, conf
dtset <- left_join(dtset, teams_abrv, by = c("Team" = "Teams"))

# for (i in 1:nrow(dtset)) {
#   for (j in 1:nrow(teams_abrv)) {
#     if (dtset[i, 2] == teams_abrv[j, 1]) {
#       dtset[i,51] <- teams_abrv[j, 4]
#       dtset[i,52] <- teams_abrv[j, 3]
#       dtset[i,53] <- teams_abrv[j, 2]
#     }
#   }
# }


#renaming the new columns
#colnames(dtset)[52] <- "CONF"
#colnames(dtset)[53] <- "DIV"
#colnames(dtset)[53] <- "TM"
dtset <- dtset %>% rename(TM = Team.y)

tabbr <- dtset$TM
tabbr <- str_pad(tabbr, width = 4, side = "left", pad = "(")
tabbr <- str_pad(tabbr, width = 5, side = "right", pad = ")")


dtset$Player <- str_c(dtset$Name, tabbr, sep = " ")

dataset <<- dtset

message("\nDone!")
}

# Calls
setenv()

getplayerdata(1,31)

getteamid("leafs")

#dates of downloaded data: 2021-01-30
datetoread <- c("2021-01-30", "2021-02-11")

readplayerdata(1, 31, datetoread)

#Plotting ----

# filtering the data set in ESGp60, GP and Pos.
# dplyr::selecting only the important columns
# sorting from bigger to smallest
# creating a new column to hold the position in the table
# creating a factor of the id and name so they show up from top to bottom
leadergoalsper60 <- dataset %>%
                filter(day == max(day)) %>%
                filter(ESGp60 >= 0.6, Pos != "G") %>%
                dplyr::select(Name, ESGp60, GP, TM)%>%
                arrange(desc(ESGp60)) %>%
                mutate(Rk = row_number())%>%
                mutate(Name = fct_reorder(Name, Rk),
                       Name = fct_rev(Name)) %>%
                select(Rk,Name,ESGp60,TM,GP)

#Summary
qksummary <- dataset %>%
  group_by(day) %>%
  summarise_at(vars(Age:FO., -Pos), mean, na.rm = TRUE) %>%
  add_column(Name = "League Average")



# Even strength goals per 60 ----
dataset %>%
  filter(ESGp60 >= 0.6, Pos != "G", day == max(day)) %>%
  filter(GP >= 10) %>%
  dplyr::select(Player, ESGp60, TM)%>%
  add_row(Player = qksummary$Name, ESGp60 = round(qksummary$ESGp60[2], 3)) %>%
  arrange(desc(ESGp60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15 | Player == "League Average") %>%
  ggplot(aes(ESGp60, Player, label = ESGp60))+
  geom_segment(aes(x=0, y=Player, xend = ESGp60, yend = Player), col = "magenta")+
  geom_point(shape = 19, size = 5, col = "magenta")+
  geom_text(color = 'snow', size = 3, nudge_x = 0.2)+
  theme_modern_rc()+
  xlab("Even Strength Goals per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n "),
       title = paste0("Leaders in Even Strength Goals per 60"),
       subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/ESGoalsper60 ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Leader Goals ----
dataset %>%
  dplyr::select(Player, G, TM)%>%
  arrange(desc(G)) %>%
  mutate(Rk = row_number())%>%
  mutate(Player = fct_reorder(Player, Rk),
         Player = fct_rev(Player))%>%
  filter(Rk <= 15) %>%
  #add_row() %>%
  ggplot(aes(G, Player, label = G))+
  geom_segment(aes(x=3, y= Player, xend = G, yend = Player), col = "magenta")+
  geom_point(shape = 19, size = 5, col = "magenta")+
  geom_text(color = 'snow', size = 3, nudge_x = 0.35)+
  theme_modern_rc()+
  xlab("Goals")+
  ylab("")+
  labs(caption = paste0(reference,"\n"),
       title = paste0("Leaders in Goals"),
       subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/Goalsleader ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Leader Assists ----
dataset %>%
  dplyr::select(Player, A, TM)%>%
  arrange(desc(A)) %>%
  mutate(Rk = row_number())%>%
  mutate(Player = fct_reorder(Player, Rk),
         Player = fct_rev(Player))%>%
  filter(Rk <= 15) %>%
  ggplot(aes(A, Player, label = A))+
  geom_segment(aes(x=3, y= Player, xend = A, yend = Player), col = "magenta")+
  geom_point(shape = 19, size = 5, col = "magenta")+
  geom_text(color = 'snow', size = 3, nudge_x = 0.55)+
  theme_modern_rc()+
  xlab("Assists")+
  ylab("")+
  labs(caption = paste0(reference,"\n"),
       title = paste0("Leaders in Assists"),
       subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/assistsleader ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Leader Points ----
dataset %>%
  dplyr::select(Player, P, TM)%>%
  arrange(desc(P)) %>%
  mutate(Rk = row_number())%>%
  mutate(Player = fct_reorder(Player, Rk),
         Player = fct_rev(Player))%>%
  filter(Rk <= 15) %>%
  ggplot(aes(P, Player, label = P))+
  geom_segment(aes(x=7, y= Player, xend = P, yend = Player), col = "magenta")+
  geom_point(shape = 19, size = 5, col = "magenta")+
  geom_text(color = 'snow', size = 3, nudge_x = 0.7)+
  theme_modern_rc()+
  xlab("Points")+
  ylab("")+
  labs(caption = paste0(reference,"\n"),
       title = paste0("Points Leaders"),
       subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/Pointsleader ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)


# Leader shoting% ----
dataset %>%
  filter(GP > 10, Div == "NORTH") %>%
  dplyr::select(Player, SH., TM)%>%
  arrange(desc(SH.)) %>%
  mutate(Rk = row_number())%>%
  mutate(Player = fct_reorder(Player, Rk),
         Player = fct_rev(Player))%>%
  filter(Rk <= 15) %>%
  ggplot(aes(SH., Player, label = SH.))+
  geom_segment(aes(x=0.18, y= Player, xend = SH., yend = Player), col = "magenta")+
  geom_point(shape = 19, size = 5, col = "magenta")+
  geom_text(aes(label = round(SH.,3)), color = 'snow', size = 3, nudge_x = 0.005)+
  theme_modern_rc()+
  xlab("Points")+
  ylab("")+
  labs(caption = paste0(reference,"\n 10 or more games played"),
       title = paste0("Shoting Percentage"),
       subtitle = "2020-21 Regular Season")

ggsave(
  paste0("plots/shotingpct ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Even strenth assists per 60 ----
dataset %>%
  filter() %>%
  dplyr::select(Player, ESAp60)%>%
  arrange(desc(ESAp60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15) %>%
  ggplot(aes(ESAp60, Player, label = ESAp60))+
  geom_segment(aes(x=1.5, y=Player, xend = ESAp60, yend = Player), col = "black")+
  geom_point(shape = 19, size = 5, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.65)+
  theme_ipsum_rc()+
  xlab("Even Strength Assists per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n Minimum 30 Games Played"),
       title = paste0("Leaders in Even Strength Assists per 60"),
       subtitle = "2019-20 Regular Season")

ggsave(
paste0("plots/ESAssistssper60 ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Even strength points per 60 ----
dataset %>%
  filter(ESPp60 >= 1.5 , Pos != "G", GP > 10) %>%
  dplyr::select(Player, ESPp60)%>%
  arrange(desc(ESPp60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15) %>%
  ggplot(aes(ESPp60, Player, label = ESPp60))+
  geom_segment(aes(x=2.5, y=Player, xend = ESPp60, yend = Player), col = "black")+
  geom_point(shape = 19, size = 5, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.15)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_ipsum_rc()+
  xlab("Even Strength Points per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n Minimum 30 Games Played"),
       title = paste0("Leaders in Even Strength Points per 60"),
       subtitle = "2019-20 Regular Season")

ggsave(
paste0("plots/ESPointsper60 ", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Points per 60  ----
dataset %>%
  filter(Pp60 >= 1.5, Pos != "G") %>%
  dplyr::select(Player, Pp60)%>%
  arrange(desc(Pp60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15) %>%
  ggplot(aes(Pp60, Player, label = Pp60))+
  geom_segment(aes(x=2.5, y=Player, xend = Pp60, yend = Player), col = "black")+
  geom_point(shape = 19, size = 5, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.07)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_ipsum_rc()+
  xlab("Points per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n Minimum 30 Games Played"),
       title = paste0("Leaders in Points per 60"),
       subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/LeaderinPointsper60", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Goals Per 60 ----
dataset %>%
  filter(Gp60 >= 1.5, Pos != "G", GP >= 10) %>%
  dplyr::select(Player, Gp60)%>%
  arrange(desc(Gp60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15) %>%
  ggplot(aes(Gp60, Player, label = Gp60))+
  geom_segment(aes(x=1.5, y=Player, xend = Gp60, yend = Player), col = "black")+
  geom_point(shape = 19, size = 5, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.09)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_ipsum_rc()+
  xlab("Goals per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n Minimum 30 Games Played"),
       title = paste0("Leaders in Goals per 60"),
       subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/LeaderinGoalsper60", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

# Assists per 60 ----
dataset %>%
  filter(Ap60 >= 1.5, GP > 30, Pos != "G") %>%
  dplyr::select(Player, Ap60)%>%
  arrange(desc(Ap60)) %>%
  mutate(id = row_number())%>%
  mutate(Player = fct_reorder(Player, id),
         Player = fct_rev(Player))%>%
  filter(id <= 15) %>%
  ggplot(aes(Ap60, Player, label = Ap60))+
  geom_segment(aes(x=2, y=Player, xend = Ap60, yend = Player), col = "black")+
  geom_point(shape = 19, size = 5, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.07)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_ipsum_rc()+
  xlab("Assists per 60")+
  ylab("")+
  labs(caption = paste0(reference,"\n Minimum 30 Games Played"),
       title = paste0("Leaders in Assists per 60"),
       subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/LeaderinAssists", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)


# time on ice  ----
dataset%>%
  #filter(GP >=30, ES >= 300) %>%
  ggplot(aes(ES, ESAp60)) +
  geom_point()+
  geom_smooth(color = "magenta", se = F)+
  scale_x_time(labels = scales::time_format("%M:%S"), breaks = scales::pretty_breaks(n = 10))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_modern_rc()+
  geom_label_repel(
    col = "black",
    data = dataset %>% filter(ESAp60 > 6),
    aes(label = Player),
    #nudge_y =  0.01,
    #nudge_x =  0.1,
    point.padding = 4,
    force_pull = 2,
    force = 3,
    show.legend = FALSE
  )

#=========================================================================================
#
#                     New section below, is a whole new thing
#
#=========================================================================================
# Who is the better shooter in nhl ----
dataset %>%
  filter(Pos != "G", na.rm = TRUE) %>%
  select(SH.) %>%
  summarise(mean = mean(SH.))


career <- dataset %>%
  filter(Pos != "G", SHOTS >= 1) %>%
  group_by(Name) %>%
  summarise(SHOTS = sum(SHOTS), G = sum(G)) %>%
  mutate(average = G / SHOTS)

career %>%
  arrange(desc(average)) %>%
  head(10)

plot(dataset$SHOTS, dataset$G)

hist(career$average)

filt_career <- career %>%
  filter(SHOTS > 110)

m <- MASS::fitdistr(filt_career$average, dbeta, start = list(shape1 = 5, shape2 = 70))

aj <- fitdistrplus::fitdist(filt_career$average, "beta", method = "mle")#, start = list(shape1 = 1, shape2 = 10))

summary(aj)
plot(aj)

# negative log likelihood of data given alpha; beta
# ll <- function(alpha, beta) {
#   -sum(VGAM::dbetabinom.ab(filt_career$G, filt_career$SHOTS, alpha, beta, log = TRUE))
# }
#
# mm <- stats4::mle(ll, start = list(alpha = 4, beta = 40), method = "L-BFGS-B")
# vcoef <- VGAM::coef(mm)

kstest <- ks.test(filt_career$average, aj$estimate)
kstest

filt_career%>%
  ggplot(aes(x = average))+
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 12)+
  stat_function(fun = function(x) dbeta(x, aj$estimate[1],
                                            aj$estimate[2]), color = "red", size = 1)+
  stat_function(fun = function(x) dbeta(x, vcoef[1],
                                             vcoef[2]), color = "green", size = 1)+
  # # where the expected value is equal to the mean (norm, logis)
  #geom_vline(aes(xintercept=aj$estimate[1], color="red"), linetype="dashed")+
  # beta distribuition expected value
  geom_vline(aes(xintercept=(aj$estimate[1]/(aj$estimate[1]+aj$estimate[2]))), color="red", linetype="dashed")+
  # geom_vline(aes(xintercept=(vcoef[1]/(vcoef[1]+vcoef[2]))), color="green",linetype="dashed")+
  # gamma distribuiton expected value
  # geom_vline(aes(xintercept=(aj$estimate[1]/aj$estimate[2]), color="red"), linetype="dashed")+
  geom_vline(aes(xintercept=mean(career$average)), color="blue", linetype="dotdash")+
  theme_ipsum_rc()+
  annotate("text", x =mean(career$average) - 0.002 , y = 2, label = "League Avg.", color = "blue", angle = 90)+
  # annotate("text", x =(vcoef[1]/(vcoef[1]+vcoef[2])) - 0.002 , y = 2, label = "all Players", color = "green", angle = 90)+
  annotate("text", x =(aj$estimate[1]/(aj$estimate[1]+aj$estimate[2])) - 0.002 , y = 2, label = "Mean of PDF", color = "red", angle = 90)+
  labs(caption = paste0(reference,"\n Minimum 100 Shots"),
       title = paste0("Distribuition of Shooting Percentage in the NHL"),
       subtitle = "2019-20 Regular Season")
ggsave(
  paste0("plots/fitting model beta distribution", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)


career <- career %>%
  mutate(eb_average = (G + aj$estimate[1]) / (SHOTS + aj$estimate[1] + aj$estimate[2]))

career %>%
  arrange(desc(eb_average))%>%
  mutate(id = row_number())%>%
  mutate(Name = fct_reorder(Name, id),
        Name = fct_rev(Name))%>%
  filter(id <= 20) %>%
  ggplot(aes(eb_average, Name, label = id))+
  geom_segment(aes(x=0.15, y=Name, xend = eb_average, yend = Name), col = "black")+
  geom_point(shape = 19, size = 4, col = "black")+
  geom_text(color = 'black', size = 3.5, x = 0.149, nudge_y = 0.2)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_ipsum_rc(grid = "Y,X")+
  xlab("Ajusted Shooting percentage to beta prior")+
  ylab("")+
  labs(caption = paste0(reference),
       title = paste0("Ajusted Shooting Percentage"),
       subtitle = "2019-20 Regular Season")

ggsave(
  paste0("plots/Rk ajusted in shooting percentage", datetoday, ".png"),
  width = 11, height = 6, dpi = 600)

career %>%
ggplot(aes(average, eb_average, color = SHOTS)) +
  geom_hline(yintercept = aj$estimate[1]/(aj$estimate[1]+aj$estimate[2]),
             color = "red", lty = 2) +
  geom_point() +
  geom_abline(color = "red") +
  scale_colour_gradient(trans = "log", breaks = 10 ^ (1:5)) +
  xlab("Shooting average") +
  ylab("Empirical Bayes Estimation Shooting Average")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6))+
  theme_ipsum_rc()+
  labs(caption = paste0(reference),
       title = paste0("Ajusted Shooting Percentage"),
       subtitle = "2019-20 Regular Season")


top50 <- career %>%
  select(Name, eb_average) %>%
  arrange(desc(eb_average)) %>%
  head(50)

#========================================================================================
#========================================================================================

# read player data from 2014 to 2020-feb-29 ----
gethrdata <- function(){

  dfcols <-cols(
  .default = col_double(),
  Player = col_character(),
  Tm = col_character(),
  Pos = col_factor(),
  ATOI = col_time(format = "%M:%S"))

allplr <- read_csv("Players_14-20feb29.csv", col_types = dfcols)

allplr$id <- str_extract(allplr$Player, "(\\\\+)\\w+")
allplr$Player <- str_remove_all(allplr$Player, "(\\\\+)\\w+")
allplr$Spct <- allplr$Spct/100
allplr$Pos <- fct_collapse(allplr$Pos, F = c("C", "C; LW", "F", "LW", "RW", "W"))

allplr <<- allplr

}

gethrdata()


career14g <- allplr %>%
  filter(Tm != "TOT", S > 0) %>%
  group_by(id, Player, Pos) %>%
  summarise(GP = sum(GP), G = sum(G), A = sum(A), PTS = sum(PTS),
            S = sum(S), TOI = sum(TOI)) %>%
  mutate(average = G / S ) %>%
  ungroup()

career14g %>%
  filter(Pos != "D", GP > 82) %>%
  ggplot(aes(average))+
  geom_histogram(bins = 20)

filt_career14g <- career14g%>%
  filter(Pos != "D", GP > 82)


ajj <- fitdistrplus::fitdist(filt_career14g$average, "beta", method = "mle")#, start = list(shape1 = 1, shape2 = 10))

summary(ajj)
plot(aj)

career14g <- career14g %>%
  mutate(eb_average = (G + ajj$estimate[1]) / (S + ajj$estimate[1] + ajj$estimate[2]))

career14g %>%
  arrange(desc(eb_average))%>%
  mutate(idd = row_number())%>%
  mutate(Player = fct_reorder(Player, idd),
        Player = fct_rev(Player))%>%
  filter(idd <= 20) %>%

  ggplot(aes(eb_average, Player, label = round(eb_average, 4)))+
  geom_segment(aes(x=0.144, y=Player, xend = eb_average, yend = Player), col = "black")+
  geom_point(shape = 19, size = 4, col = "black")+
  geom_text(color = 'black', size = 3.5, nudge_x = 0.001, nudge_y = 0)+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8))+
  theme_ipsum_rc(grid = "X,x")+
  xlab("Ajusted Shooting percentage to beta prior")+
  ylab("")+
  labs(caption = paste0(reference),
       title = paste0("Ajusted Shooting Percentage"),
       subtitle = "since 2013-14 Regular Season")

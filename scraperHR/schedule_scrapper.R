library(rvest)
library(dplyr)
library(magrittr)
library(tidyverse)
library(readr)

# URL ----
#regular season page table shared teams stats,
# dont know if they update.
webpage1 <- read_html("https://www.hockey-reference.com/pi/share/nDbKl")
webpage2 <- read_html("https://www.hockey-reference.com/pi/share/WOUJG")

regstats <- webpage1 %>%
  html_nodes("td") %>%
  html_text()

advstats <- webpage2 %>%
  html_nodes("td") %>%
  html_text()

titles_reg <-  webpage1 %>%
  html_nodes("th") %>%
  html_text()

titles_adv <-  webpage2 %>%
  html_nodes("th") %>%
  html_text()

regstatname <- gsub("%", ".", titles_reg[13:44])
regstatname <- gsub("-", "", regstatname)
regstatname <- gsub("/", "p", regstatname)

advstatname <- gsub("%", ".", titles_adv[8:32])
advstatname <- gsub("-", "", advstatname)
advstatname <- gsub("/", "p", advstatname)

# creating a data frame ----
regstat <-matrix(
    regstats,
    ncol = 32,
    byrow = T,
   dimnames = list(NULL, regstatname)
  )

regstat <- as.data.frame(regstat)

advstat <-matrix(
    advstats,
    ncol = 25,
    byrow = T,
   dimnames = list(NULL, advstatname)
  )

advstat <- as.data.frame(advstat)


allstats <- merge(regstat, advstat, by= "V1", all.x = T)

colnames(allstats)[1] <- "Teams"
allstats <- allstats[-c(14), ]

allstats <- allstats[order(allstats$PTS, decreasing = T), ]
#rank <- seq(1:nrow(allstats))

#allstats$Rk <- rank

write.csv(allstats,file = "teams_2020.csv", row.names = F)

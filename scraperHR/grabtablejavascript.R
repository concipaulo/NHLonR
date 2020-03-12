library(rvest)
library(dplyr)
library(magrittr)
library(tidyverse)
library(readr)
library(V8)

# titles so it reduces one job
statshead <- c("Teams","AvAge","GP","W","L","OL","PTS","PTS.","GF","GA","SOW",
               "SOL","SRS","SOS","TGpG","EVGF","EVGA","PP","PPO","PP.","PPA","PPOA",
               "PK.","SH","SHA","PIMpG","oPIMpG","S","S.","SA","SV.","SO","EVS.",
               "EVSV.","EVPDO","EVCF","EVCA","EVCF.","EVFF","EVFA","EVFF.",
               "EVxGF","EVxGA","EVaGF","EVaGA","EVaxDiff","EVSCF","EVSCA",
               "EVSCF.","EVHDF","EVHDA","EVHDF.","EVHDGF","EVHDC.","EVHDGA",
               "EVHDCO.")

# URL ----
#regular season page table shared teams stats, you have to generate
# a shared link of the table, it doesnt update itself :((( fucking javascript!!!!

webpage1 <- read_html("https://www.hockey-reference.com/leagues/NHL_2020.html") # Regular stats
webpage2 <- read_html("http://hkref.com/pi/share/HMPgy") # advanced stats

regstats <- webpage1 %>%
  html_nodes("div") %>%
  html_nodes("script")%>%
  html_text()

ct <- v8()
ct$source(regstats)


read_html(ct$eval(gsub('document.createElement','',regstats))) %>% 
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

write.csv(allstats,file = "teams_2020java.csv", row.names = F)

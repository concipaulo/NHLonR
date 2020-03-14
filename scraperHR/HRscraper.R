library(rvest)
library(tidyverse)

# URL ----
#regular season page table shared teams stats, you have to generate
# a shared link of the table, it doesnt update itself :(((
# Links from Mar 13, 2020
webpage1 <- read_html("https://www.hockey-reference.com/pi/share/F1qvq") # Regular stats
webpage2 <- read_html("https://www.hockey-reference.com/pi/share/ulHMZ") # advanced stats

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

statshead <- c("Teams","AvAge","GP","W","L","OL","PTS","PTS.","GF","GA","SOW",
               "SOL","SRS","SOS","TGpG","EVGF","EVGA","PP","PPO","PP.","PPA","PPOA",
               "PK.","SH","SHA","PIMpG","oPIMpG","S","S.","SA","SV.","SO","EVS.",
               "EVSV.","EVPDO","EVCF","EVCA","EVCF.","EVFF","EVFA","EVFF.",
               "EVxGF","EVxGA","EVaGF","EVaGA","EVaxDiff","EVSCF","EVSCA",
               "EVSCF.","EVHDF","EVHDA","EVHDF.","EVHDGF","EVHDC.","EVHDGA",
               "EVHDCO.")


allstats <- allstats[-c(14), ]

allstats <- allstats[order(allstats$PTS, decreasing = T), ]

# add year, we have to put manually because the data was scrapped over the
# years
allstats$Year <- cbind(Year = rep(2020, nrow(allstats)))
#rank <- seq(1:nrow(allstats))

#allstats$Rk <- rank

write.csv(allstats,file = "teams_2020.csv", row.names = F)

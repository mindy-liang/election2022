#### setup ####

pacman::p_load(tidyverse,xml2, stringr, googledrive, googlesheets4,httr,readxl,jsonlite)

gs4_auth(email = "mindy.liang@thenewslens.com")

path <-"~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會"

#### import ####

# import
download.file("https://download.2022dl.nat.gov.tw/running.json",destfile="running_T_17NOV.json", method = "wget", extra="--no-check-certificate	--http-user=DL000005 --http-password=7JM3MAI")

download.file("https://download.2022dl.nat.gov.tw/final.json",destfile="final_T_17NOV.json", method = "wget", extra="--no-check-certificate	--http-user=DL000005 --http-password=7JM3MAI")

running_T <- fromJSON("running_T_17NOV.json")
final <- fromJSON("final_T_17NOV.json")

finalRaw <- final

citycons <- rbind(finalRaw$T1,finalRaw$T2,finalRaw$T3)

citymayor <- finalRaw$TC

village <- finalRaw$TV

#### data cleaning ####








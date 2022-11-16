#### test ####

pacman::p_load(tidyverse,xml2, stringr, googledrive, googlesheets4,httr,readxl,jsonlite)

gs4_auth(email = "mindy.liang@thenewslens.com")

#### import ####

path <-"~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會"

finalRaw <- fromJSON("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/final_T.json")

citycons <- rbind(finalRaw$T1,finalRaw$T2,finalRaw$T3)

citymayor <- finalRaw$TC

village <- finalRaw$TV

#### data cleaning ####

## area mapping ##

citycons_area <- citycons %>%
  select(1:5) %>%
  mutate(areaCode = ifelse(is.na(areaCode), "00", areaCode),
         deptCode = ifelse(is.na(deptCode), "000", deptCode),
         liCode = ifelse(is.na(liCode), "000", liCode),
         tboxNo = ifelse(is.na(tboxNo), "0000", tboxNo))

citymayor_area <- citymayor %>%
  select(1:5) %>%
  mutate(areaCode = ifelse(is.na(areaCode), "00", areaCode),
         deptCode = ifelse(is.na(deptCode), "000", deptCode),
         liCode = ifelse(is.na(liCode), "000", liCode),
         tboxNo = ifelse(is.na(tboxNo), "0000", tboxNo))

village_area <- village %>%
  select(1:5) %>%
  mutate(areaCode = ifelse(is.na(areaCode), "00", areaCode),
         deptCode = ifelse(is.na(deptCode), "000", deptCode),
         liCode = ifelse(is.na(liCode), "000", liCode),
         tboxNo = ifelse(is.na(tboxNo), "0000", tboxNo))



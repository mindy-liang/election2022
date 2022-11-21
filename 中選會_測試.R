#### setup ####

pacman::p_load(tidyverse,xml2, stringr, googledrive, googlesheets4,httr,readxl,jsonlite)

gs4_auth(email = "mindy.liang@thenewslens.com")

Vote.path <-"~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/election2022/voteData"

#### import ####

# import
download.file("https://download.2022dl.nat.gov.tw/running.json",destfile="running_T_17NOV.json", method = "wget", extra="--no-check-certificate	--http-user=DL000005 --http-password=7JM3MAI")

download.file("https://download.2022dl.nat.gov.tw/final.json",destfile="final_T_17NOV.json", method = "wget", extra="--no-check-certificate	--http-user=DL000005 --http-password=7JM3MAI")

running_T <- fromJSON("running_T_17NOV.json")
final <- fromJSON("final_T_17NOV.json")

finalRaw <- final

citycons <- rbind(finalRaw$T1,finalRaw$T2,finalRaw$T3) %>%
  unnest(cols = c(candTksInfo)) %>%
  select(1:10,17:18) %>%
  rename("選舉人數" = 11, "投票率" = 12)%>%
  mutate(candNo = as.character(candNo))

write_csv(citycons, file.path(Vote.path, "citycons.csv"))

citymayor <- (finalRaw$TC) %>%
  unnest(cols = c(candTksInfo)) %>%
  select(1:10,17:18)%>%
  rename("選舉人數" = 11, "投票率" = 12) %>%
  mutate(candNo = as.character(candNo))

write_csv(citymayor, file.path(Vote.path, "citymayor.csv"))

village <- (finalRaw$TV) %>%
  unnest(cols = c(candTksInfo)) %>%
  select(1:10,17:18)%>%
  rename("選舉人數" = 11, "投票率" = 12)%>%
  mutate(candNo = as.character(candNo))

write_csv(village, file.path(Vote.path, "village.csv"))

#### data cleaning ####

analysis.path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/election2022/選後分析圖表用data"

citymayor.nameData <- citymayor %>%
  filter(is.na(deptCode)) %>%
  left_join(citymayor.list,
            by = c("prvCode", "cityCode",
                   "candNo" = "抽籤號次")) %>%
  mutate(政黨分類 = case_when(
    推薦之政黨== "中國國民黨" ~ "中國國民黨",
    推薦之政黨== "民主進步黨" ~ "民主進步黨",
    推薦之政黨== "台灣民眾黨" ~ "台灣民眾黨",
    推薦之政黨== "時代力量" ~ "時代力量",
    推薦之政黨== "無" ~ "無黨籍",
    推薦之政黨!= c("中國國民黨","民主進步黨",
              "台灣民眾黨","時代力量","無") ~ "其他政黨"))


citycons.nameData <- citycons %>%
  filter(is.na(deptCode)) %>%
  inner_join(citycons.list,
            by = c("prvCode", "cityCode","areaCode",
                   "candNo" = "抽籤號次")) %>%
  mutate(政黨分類 = case_when(
    推薦之政黨== "中國國民黨" ~ "中國國民黨",
    推薦之政黨== "民主進步黨" ~ "民主進步黨",
    推薦之政黨== "台灣民眾黨" ~ "台灣民眾黨",
    推薦之政黨== "時代力量" ~ "時代力量",
    推薦之政黨== "無" ~ "無黨籍",
    推薦之政黨!= c("中國國民黨","民主進步黨",
              "台灣民眾黨","時代力量","無") ~ "其他政黨"))

#各政黨得票率

citymayor.nameData  %>%
  group_by(選舉區,政黨分類) %>%
  summarise(得票率 = sprintf("%5.2f",sum(tksRate))) -> citymayor.voteRate.party 

write_csv(citymayor.voteRate.party,file.path(analysis.path,"各縣市各政黨得票率.csv"))


# 各縣市長藍綠催票率
# 若同黨參選如何計算？

citymayor.nameData %>%
  filter(政黨分類 %in% c("中國國民黨","民主進步黨")) %>%
  group_by(選舉區,政黨分類) %>%
  summarise(催票率 = sprintf("%5.2f",(tks/選舉人數*100))) -> citymayor.goVoteRate.party 

write_csv(citymayor.goVoteRate.party,file.path(analysis.path,"各縣市藍綠縣市長催票率.csv"))

#縣市長&縣市議員分裂情形

citymayor.nameData  %>%
  filter(candVictor %in% c("*","!")) %>%
  select(13,1:2,"選舉區","政黨分類") %>%
  mutate(年份 = "2022") %>%
  select(6,4,5) %>%
  rename("縣市" = 2, "縣市長政黨" = 3) ->leadingParty_citymayors_2022

citycons.nameData %>%
  filter(candVictor %in% c("*","!")) %>%
  select(15,17,21) %>%
  group_by(縣市,政黨分類) %>%
  summarise(席次 = n()) %>%
  filter(!政黨分類 %in% c("無黨籍","其他政黨")) %>%
  arrange(縣市,desc(席次)) %>%
  slice_head(n = 1) %>%
  mutate(年份 = "2022")%>%
  select(4,1,2) %>%
  rename("議會最大黨" = 3) ->leadingParty_citycons_2022

leadingParty_diff_byCounty_2022 <- left_join(leadingParty_citymayors_2022,leadingParty_citycons_2022) %>%
  mutate(是否分裂 =  ifelse(縣市長政黨 == 議會最大黨, "N", "Y"))
  
  
  






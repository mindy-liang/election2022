#### essential ####

taiwan.county.mapping <- read_sheet(ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
                                    sheet = "台灣縣市區域分類")


#### 2. 2022年各縣市議會組成比例 ####

citycons.all %>%
  filter(!is.na(政黨分類),is.na(deptCode)) %>%
  group_by(縣市,政黨分類) %>%
  summarise(當選人數 = length(candVictor[ which(candVictor=="*", candVictor=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2022") %>%
  select(7,1,2,6) %>%
  spread(政黨分類,席次佔比) %>%
  left_join(taiwan.county.mapping, by = c("縣市" ="mapping-縣市")) %>%
  select(9,11,10,8,6,5,7,4,3) %>%
  arrange(編號)-> win.citycons.party.counties

write_csv(win.citycons.party.counties,file.path(analysis.path,"2022年各縣市議會組成比例.csv"))

write_sheet(win.citycons.party.counties,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "2.2022年各縣市議會組成比例")

#### 3. 全台議員總席次佔比變化 ####

#歷屆縣市議員參選資料

history.citycons.list <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                                    sheet = "2002-2018 縣市議員參選名單") %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無黨籍及未經政黨推薦" ~ "無黨籍",
    政黨名稱!= c("中國國民黨","民主進步黨",
             "台灣民眾黨","時代力量","無黨籍及未經政黨推薦") ~ "其他政黨")) %>%
  unique()

citycons.2014 <- filter(history.citycons.list, grepl("2014",年份))
citycons.2018 <- filter(history.citycons.list, grepl("2018",年份))

#計算各政黨議員席次佔比

win.citycons.2014 <- citycons.2014 %>%
  group_by(政黨分類) %>%
  summarise(當選人數 = length(當選註記[ which(當選註記=="*", 當選註記=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2014")


win.citycons.2018 <- citycons.2018 %>%
  group_by(政黨分類) %>%
  summarise(當選人數 = length(當選註記[ which(當選註記=="*", 當選註記=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2018")

win.citycons.2022 <- citycons.all %>%
  filter(is.na(deptCode)& !is.na(政黨分類))%>%
  group_by(政黨分類) %>%
  summarise(當選人數 = sum(candVictor=="*",candVictor=="!"),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2022")

win.citycons <- rbind(win.citycons.2014,win.citycons.2018,win.citycons.2022) %>%
  select(1,6,5) %>%
  spread(政黨分類,席次佔比) %>%
  select(1,7,5,4,6,3,2)

write_sheet(win.citycons,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "3.全台議員總席次佔比變化")


write_csv(win.citycons,file.path(analysis.path,"3.全台議員總席次佔比變化"))

#### 4. 2012起歷屆選舉藍綠催票率 ####

#總統

goVoteRate_president <- rbind(president_12,president_16,president_20) %>%
  filter(政黨名稱 %in% c("民主進步黨","中國國民黨"))%>%
  group_by(年份,政黨名稱)%>%
  summarise(政黨得票率 = 得票率, 政黨催票率 = 催票率) %>%
  mutate(選舉分類 = "總統") %>%
  select(1,5,2,3,4) 

goVoteRate_president[4:5] <- sapply(goVoteRate_president[4:5],as.numeric)

#縣市長 round(政黨得票數/政黨投票數*100,2)

tks_citymayors <- PartyVotes_citymayors%>%
  group_by(年份,政黨名稱) %>%
  summarise(政黨得票數 = sum(得票數)) %>%
  filter(政黨名稱 %in% c("民主進步黨","中國國民黨"))

detail_citymayors <-  PartyVotes_citymayors %>%
  select(1,2,8) %>%
  unique() %>%
  group_by(年份) %>%
  summarise(總選舉人數 = sum(選舉人數))

goVoteRate_citymayors <- tks_citymayors %>%
  left_join(detail_citymayors) %>%
  mutate(政黨催票率 = round(政黨得票數/detail_citymayors$總選舉人數*100,2))


#2022資料

citymayor.nameData %>%
  group_by(推薦之政黨) %>%
  summarise(政黨得票數 = sum(tks)) %>%
  filter(推薦之政黨 %in% c("中國國民黨","民主進步黨")) -> partyvotes_citymayors.2022

citymayor.nameData %>%
  select(2,11,12) %>%
  unique() %>%
  summarise(總投票數 = sum(投票數),
            總選舉人數 = sum(選舉人數)) ->citymayors.voteDetail.2022


goVoteRate_citymayors.2022 <- partyvotes_citymayors.2022 %>%
  group_by(推薦之政黨) %>%
  summarise(政黨得票率 = round(政黨得票數/citymayors.voteDetail.2022$總投票數*100,2),
            政黨催票率 = round(政黨得票數/citymayors.voteDetail.2022$總選舉人數*100,2)) %>%
  mutate(年份 = "2022",選舉分類 = "縣市長") %>%
  select(4,5,1:3) %>%
  rename("政黨名稱" = 3)


goVoteRate_byParty <- rbind(goVoteRate_president,goVoteRate_citymayors,goVoteRate_citymayors.2022) %>%
  arrange(年份,政黨名稱) %>%
  unite(年份,1:2,sep = "-") %>%
  select(1,2,4) %>%
  spread(政黨名稱, 政黨催票率)
  

#上傳資料

write_csv(goVoteRate_byParty,file.path(analysis.path,"2012至今藍綠選舉投票率與催票率變化.csv"))

write_sheet(goVoteRate_byParty,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "4. 2012至今藍綠選舉投票率與催票率變化")

#### 5. 各縣市首長 各政黨得票率 ####

citymayor.nameData  %>%
  group_by(縣市,政黨分類) %>%
  summarise(得票率 = sprintf("%5.2f",sum(tksRate))) %>%
  spread(政黨分類,得票率) %>%
  left_join(taiwan.county.mapping, by = c("縣市" ="mapping-縣市")) %>%
  filter(縣市!="嘉義市") %>%
  select(8,10,9,1,2,4,5,7,3,6) %>%
  arrange(編號)-> citymayor.voteRate.party 

citymayor.nameData  %>%
  filter(縣市!="嘉義市") %>%
  group_by(縣市,政黨分類) %>%
  summarise(催票率 = sprintf("%5.2f",100*tks/選舉人數)) -> citymayor.goVoteRate.party 

write_csv(citymayor.voteRate.party,file.path(analysis.path,"縣市首長各政黨得票率.csv"))
write_csv(citymayor.goVoteRate.party ,file.path(analysis.path,"縣市首長各政黨催票率.csv"))

write_sheet(citymayor.voteRate.party,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "5. 2022年縣市首長各黨得票率")


#### 7. 民眾黨／時代力量／基進個別選上幾席，個別當選比例 ####

citycons.all %>%
  filter(is.na(deptCode)) %>%
  unique() %>%
  group_by(推薦之政黨) %>%
  summarise(當選人數 = length(candVictor[ which(candVictor=="*", candVictor=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2),
            落選人數 = 參選人數-當選人數) %>%
  select(1,2,5,3,4) %>%
  filter(推薦之政黨 %in% c("時代力量","台灣民眾黨","台灣基進")) -> thirdparty.win.rate

write_csv(thirdparty.win.rate,file.path(analysis.path,"民眾黨、時代力量、基進當選情形.csv"))

write_sheet(thirdparty.win.rate,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "7.民眾黨、時代力量、基進當選席次與當選比例")

#### 8. 民眾黨／時代力量／基進在哪幾區得票特別亮眼？ ####

#結算每一個選區催票率排名

GoVote.sequence.byArea <- citycons.all %>%
  filter(is.na(deptCode)) %>%
  mutate(催票率 = (tks/選舉人數)) %>%
  group_by(縣市, areaCode) %>%
  arrange(縣市,areaCode,desc(催票率)) %>%
  mutate(催票率排名 = row_number()) %>%
  left_join(select(select.num.citycons,1:3), by = c("縣市","areaCode"="選區")) %>%
  mutate(落選頭 = 應選人數+1)


#各黨催票率最高的5個選區

GoVote.sequence.byArea %>%
  filter(推薦之政黨 %in% c("時代力量","台灣民眾黨","台灣基進")) %>%
  group_by(推薦之政黨) %>%
  arrange(desc(催票率)) %>%
  slice(1:10) %>%
  mutate(分類 = "各黨催票率前10名選區") -> top10.thirdparty

GoVote.sequence.byArea %>%
  filter(推薦之政黨 %in% c("時代力量","台灣民眾黨","台灣基進")) %>%
  filter(催票率排名==落選頭) %>%
  group_by(推薦之政黨) %>%
  arrange(催票率排名) %>%
  mutate(分類 = "落選頭") -> top_loser.thirdparty

#合併資料

thirdparty.win.detail <- rbind(top10.thirdparty,top_loser.thirdparty) %>%
  select(20,2,3,5,6,9:12,16,17) %>%
  mutate(選區 = paste0(縣市,"第",areaCode,"選區")) %>%
  select(1,5,12,8,10)


write_csv(thirdparty.win.detail,file.path(analysis.path,"民眾黨、時代力量、基進前10名選區及落選頭.csv"))

write_sheet(thirdparty.win.detail,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "8.民眾黨、時代力量、基進前十名選區及落選頭")
  


#### 9. 2018-2022年各政黨得票率、催票率 ####


##2022年

citycons.partyVote.2022 <- citycons.all %>%
  filter(!is.na(政黨分類)) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票數 = sum(tks))

citycons.VoteDetail.2022 <- citycons.all %>%
  select(2,13,14) %>%
  unique() %>%
  summarise(總投票人數 = sum(投票數),
            總選舉人數 = sum(選舉人數))

citycons.partyVote.2022 %>%
  mutate(總投票人數 = citycons.VoteDetail.2022$總投票人數,
         總選舉人數 = citycons.VoteDetail.2022$總選舉人數) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票率 = 政黨得票數/總投票人數,
            政黨催票率 = 政黨得票數/總選舉人數) %>%
  filter(!is.na(政黨分類))%>%
  mutate(年份 = "2022") %>%
  select(4,1:3) -> citycons.party.2022


#合併結果

party.rate.history <- rbind(voteRate.citycons.party.2018,congress.party.2020,party.2020,citycons.party.2022) %>%
  arrange(年份,政黨分類)

write_csv(party.rate.history, file.path(analysis.path,"2018-2022年各政黨得票率、催票率.csv"))

write_sheet(party.rate.history,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "9. 2018-2022年各政黨得票率、催票率")


#### 10. 時力選區第三勢力變化(時力、民眾黨) ####

newparty.dept.2022 <- read_sheet(ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
                                 sheet = "時代力量議員提名選區") %>%
  select(2,1) %>%
  mutate(時力選區 = str_sub(時力選區, 4,9)) 

## 2020政黨票 ###

#需重新整理選區資料
newparty.dept.2022 %>%
  mutate(選區 = sprintf("%02d",parse_number(時力選區))) %>%
  left_join(select(election_area_mapping,c(6,7,3)), by = c("縣市" = "COUNTYNAME", "選區" = "areaCode")) %>%
  unique()-> parse.newparty

thirdparty.tks.2020.dept <- party.tks.2020 %>%
  inner_join(select(parse.newparty, c(1,4)), by = c("縣市","鄉鎮市區"="TOWNNAME")) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨"))

party.countyVoteDetail.2020 <- thirdparty.tks.2020.dept %>%
  select(3,10,11) %>%
  unique() %>%
  group_by(縣市) %>%
  summarise(縣市選區投票人數 = sum(投票數),
            縣市選區選舉人數 = sum(選舉人數))

thirdparty.tks.2020.county <- thirdparty.tks.2020.dept %>%
  group_by(縣市,政黨分類) %>%
  summarise(縣市得票數 = sum(得票數)) %>%
  left_join(party.countyVoteDetail.2020, by = "縣市") %>%
  group_by(縣市, 政黨分類) %>%
  summarise(縣市得票率 = round(100*縣市得票數/縣市選區投票人數,2),
            縣市催票率 = round(100*縣市得票數/縣市選區選舉人數,2)) %>%
  mutate(年份 = "2020-政黨票")

## 2022 縣市議員###

thirdparty.tks.2022.dept <- citycons.all %>%
  filter(!is.na(deptCode)) %>%
  left_join(unique(select(election_area_mapping,c(1:4,7)))) %>% 
  inner_join(select(parse.newparty, c(1,4)), by = c("縣市","TOWNNAME")) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨"))

party.countyVoteDetail.2022 <- thirdparty.tks.2022.dept %>%
  select(2,13,14) %>%
  unique() %>%
  group_by(縣市) %>%
  summarise(縣市選區投票人數 = sum(投票數),
            縣市選區選舉人數 = sum(選舉人數))

thirdparty.tks.2022.county <- thirdparty.tks.2022.dept %>%
  group_by(縣市,政黨分類) %>%
  summarise(縣市得票數 = sum(tks)) %>%
  left_join(party.countyVoteDetail.2020, by = "縣市") %>%
  group_by(縣市, 政黨分類) %>%
  summarise(縣市得票率 = round(100*縣市得票數/縣市選區投票人數,2),
            縣市催票率 = round(100*縣市得票數/縣市選區選舉人數,2)) %>%
  mutate(年份 = "2022-議員選舉")

# 合併2020 及 2022資料

thirdparty.voteRate.history <- rbind(thirdparty.tks.2020.county,thirdparty.tks.2022.county) %>%
  select(5,1,2,4) %>%
  spread(政黨分類, 縣市催票率) %>%
  left_join(taiwan.county.mapping, by = c("縣市" ="mapping-縣市")) %>%
  select(1,5,7,6,2,4,3) %>%
  arrange(年份,編號)

thirdparty.voteRate.history_2020 <- filter(thirdparty.voteRate.history, 年份 =="2020-政黨票") 
thirdparty.voteRate.history_2022 <- filter(thirdparty.voteRate.history, 年份 =="2022-議員選舉")

write_csv(thirdparty.voteRate.history, file.path(analysis.path, "比較時力選區第三勢力變化（時力、民眾黨）_縣市.csv"))

write_sheet(thirdparty.voteRate.history,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "10. 時力選區第三勢力變化(時力、民眾黨)")

write_sheet(thirdparty.voteRate.history_2020,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "10-1. 時力選區第三勢力變化(時力、民眾黨)_2020政黨票")

write_sheet(thirdparty.voteRate.history_2022,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "10-2. 時力選區第三勢力變化(時力、民眾黨)_2022-議員選舉")




#### 10. 時力選區第三勢力變化(時力、民眾黨)_鄉鎮市區 ####
thirdparty.voteRate.2020.dept <- thirdparty.tks.2020.dept %>%
  select(1,3,4,12,8,10,11) %>%
  mutate(催票率 = round(100*得票數/選舉人數,2))

thirdparty.voteRate.2022.dept <-thirdparty.tks.2022.dept %>%
  select(2,18,15,10,13,14) %>%
  mutate(年份 = "2022") %>%
  select(7,1:6) %>%
  rename("鄉鎮市區" = 3,"得票數"=5) %>%
  mutate(催票率 = round(100*得票數/選舉人數,2))

#合併2020 及 2022資料

thirdparty.voteRate.history.dept <- rbind(thirdparty.voteRate.2020.dept,thirdparty.voteRate.2022.dept) %>%
  select(1:4,8)

write_csv(thirdparty.voteRate.history.dept, file.path(analysis.path, "比較時力選區第三勢力變化（時力、民眾黨）_鄉鎮市區.csv"))

write_sheet(thirdparty.voteRate.history.dept,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "10.時力選區第三勢力變化(時力、民眾黨)_鄉鎮市區")
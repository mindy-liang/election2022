#### 各政黨參選各公職人數 ####

candidate %>%
  group_by(推薦之政黨,公職類別) %>%
  summarise(參選人數 = n()) %>%
  spread(公職類別,參選人數) %>%
  filter(推薦之政黨 %in% c("中國國民黨","民主進步黨","時代力量","台灣民眾黨",
                           "台灣基進","小民參政歐巴桑聯盟")) -> candidate.party

write_sheet(candidate.party, ss = "1IDiqoihQXNKf9ekHvHFGZ0doT2gBPBMQi2H0G8e9SBI",
            sheet = "各政黨參選各公職人數")

#### 篩選2020立委選舉資料(全國) ####


#區域立委
elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/區域立委/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/區域立委/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/區域立委/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/區域立委/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/區域立委/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

colnames(elbase) <- c("省市","縣市","選區","鄉鎮市區","村里","名稱")
colnames(elcand) <- c("省市","縣市","選區","鄉鎮市區","村里","號次","名字","政黨代號","性別","出生日期","年齡","出生地","學歷","現任","當選註記")
colnames(elctks) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","號次","得票數","得票率","當選註記")
colnames(elpaty) <- c("政黨代號","政黨名稱")
colnames(elprof) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","有效票","無效票","投票數","選舉人數","人口數","候選人數合計","當選人數合計",
                      "候選人數男","候選人數女","當選人數男","當選人數女","選舉人數對人口數","投票數對選舉人數","當選人數對候選人數")

#去除elcand NA列

elcand[,16] <- NULL
elcand$政黨代號 <- as.character(elcand$政黨代號)
elpaty$政黨代號 <- as.character(elpaty$政黨代號)  


##清除分區編號中的'
elbase <- data.frame(lapply(elbase, function(x) {
  gsub("'", "", x)
}))

elcand <- data.frame(lapply(elcand, function(x) {
  gsub("'", "", x)
}))

elctks <- data.frame(lapply(elctks, function(x) {
  gsub("'", "", x)
}))

elprof <- data.frame(lapply(elprof, function(x) {
  gsub("'", "", x)
}))

congress.2020.all <- elctks %>%
  filter(村里=="0000") %>%
  left_join(
    select(
      filter(elbase, 選區=="00" & 鄉鎮市區=="000"&村里=="0000"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elbase, 選區!="00" & 鄉鎮市區!="000"&村里=="0000"),
      c(1:4,6)
    ), by = c("縣市","省市","選區","鄉鎮市區")) %>%
  left_join(
    select(
      filter(elcand, 選區!="00" & 鄉鎮市區=="000"&村里=="0000"),
      c(1:3,6:8,9,11,14)
    ), by = c("縣市","省市","選區","號次")) %>%
  left_join(
    select(
      filter(elprof, 村里=="0000"),
      c(1:4,9,10)
    ), by = c("縣市","省市","選區","鄉鎮市區")) %>%
  left_join(elpaty,by ="政黨代號") %>%
  select(-c(1,2,4,5,6)) %>%
  mutate(年份 = "2020", 類別 = "立委",分類 = "區域") %>%
  select(16:18,6,7,1,2,8,10,11,15,12,5,3,4,13,14) %>%
  rename("縣市" = 4, "鄉鎮區域" = 5) %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無" ~ "無黨籍",
    政黨名稱!= c("中國國民黨","民主進步黨",
              "台灣民眾黨","時代力量","無") ~ "其他政黨"))

## 山地原住民


#山地立委
elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/山地立委/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/山地立委/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/山地立委/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/山地立委/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/山地立委/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

colnames(elbase) <- c("省市","縣市","選區","鄉鎮市區","村里","名稱")
colnames(elcand) <- c("省市","縣市","選區","鄉鎮市區","村里","號次","名字","政黨代號","性別","出生日期","年齡","出生地","學歷","現任","當選註記")
colnames(elctks) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","號次","得票數","得票率","當選註記")
colnames(elpaty) <- c("政黨代號","政黨名稱")
colnames(elprof) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","有效票","無效票","投票數","選舉人數","人口數","候選人數合計","當選人數合計",
                      "候選人數男","候選人數女","當選人數男","當選人數女","選舉人數對人口數","投票數對選舉人數","當選人數對候選人數")

#去除elcand NA列

elcand[,16] <- NULL
elcand$政黨代號 <- as.character(elcand$政黨代號)
elpaty$政黨代號 <- as.character(elpaty$政黨代號)  


##清除分區編號中的'
elbase <- data.frame(lapply(elbase, function(x) {
  gsub("'", "", x)
}))

elcand <- data.frame(lapply(elcand, function(x) {
  gsub("'", "", x)
}))

elctks <- data.frame(lapply(elctks, function(x) {
  gsub("'", "", x)
}))

elprof <- data.frame(lapply(elprof, function(x) {
  gsub("'", "", x)
}))


congress.2020.am <- elctks %>%
  filter(村里=="0000") %>%
  left_join(
    select(
      filter(elbase, 選區=="00" & 鄉鎮市區=="000"&村里=="0000"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elbase, 鄉鎮市區!="000"&村里=="0000"),
      c(1,2,4,6)
    ), by = c("縣市","省市","鄉鎮市區")) %>%
  left_join(
    select(elcand,c(6:8,9,11,14)), by = c("號次")) %>%
  left_join(
    select(
      filter(elprof, 村里=="0000"),
      c(1:4,9,10)
    ), by = c("縣市","省市","選區","鄉鎮市區")) %>%
  left_join(elpaty,by ="政黨代號") %>%
  select(-c(1,2,4,5,6)) %>%
  mutate(年份 = "2020", 類別 = "立委",分類 = "山地") %>%
  select(16:18,6,7,1,2,8,10,11,15,12,5,3,4,13,14) %>%
  rename("縣市" = 4, "鄉鎮區域" = 5) %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無" ~ "無黨籍",
    政黨名稱!= c("中國國民黨","民主進步黨",
             "台灣民眾黨","時代力量","無") ~ "其他政黨"))


## 平地原住民


#平地立委
elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/平地立委/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/平地立委/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/平地立委/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/平地立委/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/立委/平地立委/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

colnames(elbase) <- c("省市","縣市","選區","鄉鎮市區","村里","名稱")
colnames(elcand) <- c("省市","縣市","選區","鄉鎮市區","村里","號次","名字","政黨代號","性別","出生日期","年齡","出生地","學歷","現任","當選註記")
colnames(elctks) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","號次","得票數","得票率","當選註記")
colnames(elpaty) <- c("政黨代號","政黨名稱")
colnames(elprof) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","有效票","無效票","投票數","選舉人數","人口數","候選人數合計","當選人數合計",
                      "候選人數男","候選人數女","當選人數男","當選人數女","選舉人數對人口數","投票數對選舉人數","當選人數對候選人數")

#去除elcand NA列

elcand[,16] <- NULL
elcand$政黨代號 <- as.character(elcand$政黨代號)
elpaty$政黨代號 <- as.character(elpaty$政黨代號)  


##清除分區編號中的'
elbase <- data.frame(lapply(elbase, function(x) {
  gsub("'", "", x)
}))

elcand <- data.frame(lapply(elcand, function(x) {
  gsub("'", "", x)
}))

elctks <- data.frame(lapply(elctks, function(x) {
  gsub("'", "", x)
}))

elprof <- data.frame(lapply(elprof, function(x) {
  gsub("'", "", x)
}))


congress.2020.an <- elctks %>%
  filter(村里=="0000") %>%
  left_join(
    select(
      filter(elbase, 選區=="00" & 鄉鎮市區=="000"&村里=="0000"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elbase, 鄉鎮市區!="000"&村里=="0000"),
      c(1,2,4,6)
    ), by = c("縣市","省市","鄉鎮市區")) %>%
  left_join(
    select(elcand,c(6:8,9,11,14)), by = c("號次")) %>%
  left_join(
    select(
      filter(elprof, 村里=="0000"),
      c(1:4,9,10)
    ), by = c("縣市","省市","選區","鄉鎮市區")) %>%
  left_join(elpaty,by ="政黨代號") %>%
  select(-c(1,2,4,5,6)) %>%
  mutate(年份 = "2020", 類別 = "立委",分類 = "山地") %>%
  select(16:18,6,7,1,2,8,10,11,15,12,5,3,4,13,14) %>%
  rename("縣市" = 4, "鄉鎮區域" = 5) %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無" ~ "無黨籍",
    政黨名稱!= c("中國國民黨","民主進步黨",
             "台灣民眾黨","時代力量","無") ~ "其他政黨"))



#合併資料

congress.2020 <- rbind(congress.2020.all,congress.2020.am,congress.2020.an) %>%
  filter(is.na(鄉鎮區域)) %>%
  subset(縣市!="全國") %>%
  mutate()

congress.2020[14:17] <- sapply(congress.2020[14:17],as.numeric)

congress.partyVote.2020 <- congress.2020 %>%
  group_by(政黨分類) %>%
  summarise(政黨得票數 = sum(得票數))

congress.VoteDetail.2020 <- congress.2020 %>%
  select(4,16,17) %>%
  unique() %>%
  summarise(總投票人數 = sum(投票數),
            總選舉人數 = sum(選舉人數))

congress.partyVote.2020 %>%
  mutate(總投票人數 = congress.VoteDetail.2020$總投票人數,
         總選舉人數 = congress.VoteDetail.2020$總選舉人數) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票率 = round(100*政黨得票數/總投票人數,2),
            政黨催票率 = round(100*政黨得票數/總選舉人數,2)) %>%
  mutate(年份 = "2020-立委") %>%
  select(4,1:3) -> congress.party.2020

#2020政黨票 政黨得票率/催票率

party.tks.2020 %>%
  group_by(政黨分類) %>%
  summarise(政黨得票數 = sum(得票數)) -> party.vote.2020

party.VoteDetail.2020 <- party.tks.2020 %>%
  filter(縣市 == "全國") %>%
  select(3,10,11) %>%
  unique() %>%
  summarise(總投票人數 = 投票數,
            總選舉人數 = 選舉人數)

party.vote.2020 %>%
  mutate(總投票人數 = party.VoteDetail.2020$總投票人數,
         總選舉人數 = party.VoteDetail.2020$總選舉人數) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票率 = round(100*政黨得票數/總投票人數,2),
            政黨催票率 = round(100*政黨得票數/總選舉人數,2)) %>%
  mutate(年份 = "2020-政黨票") %>%
  select(4,1:3) -> party.2020

##2022年

citycons.partyVote.2022 <- citycons.nameData %>%
  group_by(政黨分類) %>%
  summarise(政黨得票數 = sum(tks))

citycons.VoteDetail.2022 <- citycons.nameData %>%
  select(15,11,12) %>%
  unique() %>%
  summarise(總投票人數 = sum(投票數),
            總選舉人數 = sum(選舉人數))

citycons.partyVote.2022 %>%
  mutate(總投票人數 = citycons.VoteDetail.2022$總投票人數,
         總選舉人數 = citycons.VoteDetail.2022$總選舉人數) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票率 = round(100*政黨得票數/總投票人數,2),
            政黨催票率 = round(100*政黨得票數/總選舉人數,2)) %>%
  filter(!is.na(政黨分類))%>%
  mutate(年份 = "2022") %>%
  select(4,1:3) -> citycons.party.2022


#合併結果

party.rate.history <- rbind(voteRate.citycons.party,congress.party.2020,party.2020,citycons.party.2022) %>%
  arrange(年份)

write_csv(party.rate.history, file.path(analysis.path,"2018-2022年各政黨得票率、催票率.csv"))

write_sheet(party.rate.history,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "2018-2022年各政黨得票率、催票率")

#### 時力選區 ####

newparty.dept.2022 <- read_sheet(ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
                                 sheet = "時代力量議員提名選區") %>%
  select(2,1) %>%
  mutate(時力選區 = str_sub(時力選區, 4,9)) 

# 2020政黨票

#需重新整理選區資料
newparty.dept.2022 %>%
  mutate(選區 = sprintf("%02d",parse_number(時力選區))) %>%
  left_join(select(election_area_mapping,c(6,7,3)), by = c("縣市" = "COUNTYNAME", "選區" = "areaCode")) %>%
  unique()-> parse.newparty


party.countyVoteDetail.2020 <- party.tks.2020 %>%
  filter(縣市 != "全國") %>%
  select(3,10,11) %>%
  unique() %>%
  group_by(縣市) %>%
  summarise(總投票人數 = sum(投票數),
            總選舉人數 = sum(選舉人數))

thirdparty.tks.2020.dept <- party.tks.2020 %>%
  inner_join(select(parse.newparty, c(1,3,4)), by = c("縣市","選區","鄉鎮市區"="TOWNNAME")) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨"))

thirdparty.tks.2020.county <- thirdparty.tks.2020.dept %>%
  group_by(縣市,政黨分類) %>%
  summarise(縣市得票數 = sum(得票數)) %>%
  left_join(party.countyVoteDetail.2020, by = "縣市") %>%
  summarise(縣市得票率 = round(100*縣市得票數/總投票人數,2),
            縣市催票率 = round(100*縣市得票數/總選舉人數,2))
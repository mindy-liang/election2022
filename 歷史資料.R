
#### 2012起歷屆選舉藍綠催票率 ####

## 整理總統大選資料 ###

#2012

elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2012/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2012/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2012/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2012/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2012/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

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

# 清理與合併

president_12 <- elcand %>%
  select(1:10,14,15) %>%
  left_join(
    select(
      filter(elbase, 省市=="00"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elctks, 省市=="00"),
      c(1,7:9)
    ), by = c("省市","號次")) %>%
  left_join(
    select(
      filter(elprof, 省市=="00"),
      c(1,10)
    ), by = c("省市")) %>%
  mutate(催票率 = sprintf("%5.2f",(as.numeric(得票數)/as.numeric(選舉人數)*100))) %>%
  select(-c(1:5)) %>%
  left_join(elpaty,by ="政黨代號") %>%
  mutate(年份 = "2012", 類別 = "全國",分類 = "總統大選") %>%
  mutate(參選年齡 = (as.numeric(年份)-1911)-as.numeric(substr(出生日期,1,3))) %>%
  filter(row_number() %% 2 == 1) %>%
  select(14:16,1,2,13,17,4,6,7,9,10,12)


#2016

elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2016/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2016/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2016/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2016/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2016/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

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

# 清理與合併

president_16 <- elcand %>%
  select(1:10,14,15) %>%
  left_join(
    select(
      filter(elbase, 省市=="00"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elctks, 省市=="00"),
      c(1,7:9)
    ), by = c("省市","號次")) %>%
  left_join(
    select(
      filter(elprof, 省市=="00"),
      c(1,10)
    ), by = c("省市")) %>%
  mutate(催票率 = sprintf("%5.2f",(as.numeric(得票數)/as.numeric(選舉人數)*100))) %>%
  select(-c(1:5)) %>%
  left_join(elpaty,by ="政黨代號") %>%
  mutate(年份 = "2016", 類別 = "全國",分類 = "總統大選") %>%
  mutate(參選年齡 = (as.numeric(年份)-1911)-as.numeric(substr(出生日期,1,3))) %>%
  filter(row_number() %% 2 == 1) %>%
  select(14:16,1,2,13,17,4,6,7,9,10,12)

#2020

elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2020/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2020/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2020/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2020/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/總統/2020/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

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

# 清理與合併

president_20 <- elcand %>%
  select(1:10,14,15) %>%
  left_join(
    select(
      filter(elbase, 省市=="00"),
      c(1,2,6)
    ), by = c("縣市","省市")) %>%
  left_join(
    select(
      filter(elctks, 省市=="00"),
      c(1,7:9)
    ), by = c("省市","號次")) %>%
  left_join(
    select(
      filter(elprof, 省市=="00"),
      c(1,10)
    ), by = c("省市")) %>%
  mutate(催票率 = sprintf("%5.2f",(as.numeric(得票數)/as.numeric(選舉人數)*100))) %>%
  select(-c(1:5)) %>%
  left_join(elpaty,by ="政黨代號") %>%
  mutate(年份 = "2020", 類別 = "全國",分類 = "總統大選") %>%
  mutate(參選年齡 = (as.numeric(年份)-1911)-as.numeric(substr(出生日期,1,3))) %>%
  filter(row_number() %% 2 == 1) %>%
  select(14:16,1,2,13,17,4,6,7,9,10,12)


## 整理縣市長資料 ###

PartyVotes_citymayors <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                                      sheet = "RAW-歷屆縣市長參選人資訊") %>%
  filter(年份>=2014) %>% 
  select(1,4,6,7,11,12,16,17)


#### 比較過去兩屆議員總席次變化 ####

#歷屆縣市議員參選資料

history.citycons.list <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                                    sheet = "2002-2018 縣市議員參選名單") %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無黨籍及未經政黨推薦" ~ "無黨籍",
    政黨名稱== "無" ~ "無黨籍",
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

win.citycons.2022 <- citycons.nameData %>%
  filter(!is.na(公職類別))%>%
  group_by(政黨分類) %>%
  summarise(當選人數 = length(candVictor[ which(candVictor=="*", candVictor=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2022")

win.citycons <- rbind(win.citycons.2014,win.citycons.2018,win.citycons.2022) %>%
  select(1,6,5) %>%
  spread(政黨分類,席次佔比)

write_sheet(win.citycons,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "全台議員總席次佔比變化")


write_csv(win.citycons,file.path(analysis.path,"2014-2022全台議員總席次佔比變化.csv"))

#### 比較過去兩屆議員各政黨得票率變化 ####

voteRate.citycons <- history.citycons.list %>% filter(grepl("2014|2018",年份))
  

voteRate.citycons[11:14] <- sapply(voteRate.citycons[11:14],as.numeric)

#計算各縣市各選區總投票數跟總選舉數

voteRate.citycons %>%
  select(1,4,13,14) %>%
  unique() %>%
  group_by(年份) %>%
  summarise(總投票數 = sum(投票數),
            總選舉人數 = sum(選舉人數)) -> vote.detail

voteRate.citycons %>% 
  group_by(年份,政黨分類) %>%
  summarise(總得票數 = sum(得票數)) %>%
  left_join(vote.detail) %>%
  group_by(年份,政黨分類) %>%
  summarise(政黨得票率 = round(總得票數/總投票數*100,2),
            政黨催票率 = round(總得票數/總選舉人數*100,2)) -> voteRate.citycons.party

write_csv(voteRate.citycons.party,file.path(analysis.path,"2014-2022議員各政黨得票率、催票率變化.csv"))

write_sheet(voteRate.citycons.party,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "2014-2022議員各政黨得票率、催票率變化")

#### 2020政黨票 與 2022 議員選舉 得票率/催票率 比較 ####

## 2020政黨票 資料清理

elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表
elrepm <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elrepm.csv", col_names = FALSE) #不分區政黨代表人
elretks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elretks.csv", col_names = FALSE) #不分區政黨得票檔

colnames(elbase) <- c("省市","縣市","選區","鄉鎮市區","村里","名稱")
colnames(elcand) <- c("省市","縣市","選區","鄉鎮市區","村里","號次","名字","政黨代號","性別","出生日期","年齡","出生地","學歷","現任","當選註記")
colnames(elctks) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","號次","得票數","得票率","當選註記")
colnames(elpaty) <- c("政黨代號","政黨名稱")
colnames(elprof) <- c("省市","縣市","選區","鄉鎮市區","村里","投開票所","有效票","無效票","投票數","選舉人數","人口數","候選人數合計","當選人數合計",
                      "候選人數男","候選人數女","當選人數男","當選人數女","選舉人數對人口數","投票數對選舉人數","當選人數對候選人數")
colnames(elrepm) <- c("政黨代號","排名","名字","性別","出生日期","年齡","出生地","學歷","現任","當選註記")
colnames(elretks) <- c("政黨代號","第一階段得票率","第二階段得票率","候選人數","當選人數")

#去除elcand NA列

elcand[,16] <- NULL
elcand$政黨代號 <- as.character(elcand$政黨代號)
elpaty$政黨代號 <- as.character(elpaty$政黨代號)  

# 清理與合併


party.tks.2020 <- elctks %>%
  filter(村里 == "0000"&投開票所 =="0000") %>%
  left_join(select(elcand, c(6,7))) %>%
  left_join(
    select(
      filter(elbase, 鄉鎮市區=="000"&選區=="00"),
      c(1,2,6))) %>%
  left_join(
    select(
      filter(elbase, 鄉鎮市區!="000"&村里=="0000"),
      -c(3,5)), by = c("省市","縣市","鄉鎮市區")) %>%
  left_join(
    select(filter(elprof, 村里 == "0000"&投開票所 =="0000"),c(1:4,9,10))) %>%
  mutate(年份 = "2020", 類別 = "政黨票") %>%
  ungroup() %>%
  select(16,17,12,13,3,7,11,8,9,14,15) %>%
  rename("縣市" = 3, "鄉鎮市區" = 4 ) %>%
  mutate(政黨分類 = case_when(
    名字== "中國國民黨" ~ "中國國民黨",
    名字== "民主進步黨" ~ "民主進步黨",
    名字== "台灣民眾黨" ~ "台灣民眾黨",
    名字== "時代力量" ~ "時代力量",
    名字== "無" ~ "無黨籍",
    名字!= c("中國國民黨","民主進步黨",
             "台灣民眾黨","時代力量","無") ~ "其他政黨"))

##2020 政黨票 & 2022 議員席次佔比

# 2020 政黨票（全國）

voteRate.party.tks.2020 <- elretks %>%
  mutate(政黨代號 = as.character(政黨代號)) %>%
  left_join(elpaty) %>%
  mutate(政黨分類 = case_when(
    政黨名稱== "中國國民黨" ~ "中國國民黨",
    政黨名稱== "民主進步黨" ~ "民主進步黨",
    政黨名稱== "台灣民眾黨" ~ "台灣民眾黨",
    政黨名稱== "時代力量" ~ "時代力量",
    政黨名稱== "無" ~ "無黨籍",
    政黨名稱!= c("中國國民黨","民主進步黨",
              "台灣民眾黨","時代力量","無") ~ "其他政黨")) %>%
  select(6,7,2:5)

thirdparty.tks.2020 <- voteRate.party.tks.2020 %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨","無黨籍")) %>%
  select(2,3) %>%
  rename("第一階段政黨票得票率" = 2) %>% 
  mutate(分類 = "全國")

# 2022 議員席次（全國）

thirdparty.citycons.2022 <- citycons.nameData %>%
  group_by(政黨分類) %>%
  summarise(當選人數 = length(candVictor[ which(candVictor=="*", candVictor=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(議員席次佔比 = round(當選人數/sum(當選人數)*100,2)) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨")) %>%
  select(1,5) %>%
  mutate(分類 = "全國")


#時力選區

newparty.dept.2022 <- read_sheet(ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
                                 sheet = "時代力量議員提名選區") %>%
  select(2,1) %>%
  mutate(時力選區 = str_sub(時力選區, 4,9)) 


# 2020 政黨票（時力選區）


newparty.dept.2022 %>%
  mutate(選區 = sprintf("%02d",parse_number(時力選區))) -> parse.newparty

thirdparty.tks.2020.dept <- party.tks.2020 %>%
  inner_join(select(parse.newparty, c(1,3)), by = c("縣市","選區")) %>%
  group_by(政黨分類) %>%
  summarise(政黨得票數 = sum(得票數)) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨"))

newparty.tks <- elprof %>%
  filter(省市 == "00") %>%
  


# 2022 議員席次（時力選區）
thirdparty.citycons.2022.dept <- citycons.nameData %>%
  inner_join(newparty.dept.2022, by = c("縣市","選區" = "時力選區")) %>%
  group_by(政黨分類) %>%
  summarise(當選人數 = length(candVictor[ which(candVictor=="*", candVictor=="!")]),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(議員席次佔比 = round(當選人數/sum(當選人數)*100,2)) %>%
  filter(政黨分類 %in% c("時代力量","台灣民眾黨")) %>%
  select(1,5) %>%
  mutate(分類 = "時力選區")

#合併資料
thirdparty.rate <- cbind(thirdparty.tks.2020,thirdparty.citycons.2022)
  
write_sheet(thirdparty.rate,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "全國-時力與民眾黨佔比")




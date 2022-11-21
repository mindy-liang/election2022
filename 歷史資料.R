#### 歷屆各縣市首長與議會最大黨 ####

leadingParty_citycons <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
           sheet = "2002-2018各縣市議會最大黨") %>%
  filter(年份>=2014) %>%
  select(1:3) %>%
  unique() %>%
  rename("議會最大黨" = 3)

leadingParty_citymayors <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                                      sheet = "RAW-歷屆縣市長選舉資訊") %>%
  filter(年份>=2014) %>%
  select(1,4,7) %>%
  unique() %>%
  rename("縣市長政黨"=3)

leadingParty_diff_byCounty <- left_join(leadingParty_citymayors,leadingParty_citycons) %>%
  mutate(是否分裂 =  ifelse(縣市長政黨 == 議會最大黨, "N", "Y"))

write_csv(leadingParty_diff_byCounty,file.path(analysis.path,"各縣市首長與議會最大黨分裂情形.csv"))

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

goVoteRate_president <- rbind(president_12,president_16,president_20) %>%
  filter(政黨名稱 %in% c("民主進步黨","中國國民黨"))%>%
  group_by(年份,政黨名稱)%>%
  summarise(政黨得票率 = 得票率, 政黨催票率 = 催票率) %>%
  mutate(選舉分類 = "總統") %>%
  select(1,5,2,3,4)

## 整理縣市長資料 ###

PartyVotes_citymayors <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                                      sheet = "RAW-歷屆縣市長選舉資訊") %>%
  filter(年份>=2014) %>% 
  select(1,4,6,7,11,12,16,17)

goVoteRate_citymayors <- PartyVotes_citymayors%>%
  filter(政黨名稱 %in% c("民主進步黨","中國國民黨")) %>%
  group_by(年份,政黨名稱) %>%
  mutate(政黨得票數 = sum(得票數),政黨投票數 = sum(投票數)) %>%
  group_by(年份) %>%
  mutate(當年選舉人數 = sum(選舉人數)) %>%
  select(1,4,9:11) %>%
  unique() %>%
  group_by(年份,政黨名稱) %>%
  summarise(政黨得票率 = sprintf("%5.2f",(as.numeric(政黨得票數)/as.numeric(政黨投票數)*100)),
            政黨催票率 = sprintf("%5.2f",(as.numeric(政黨得票數)/as.numeric(當年選舉人數)*100))) %>%
  mutate(選舉分類 = "縣市長") %>%
  select(1,5,2,3,4)

goVoteRate_byParty <- rbind(goVoteRate_president,goVoteRate_citymayors) %>%
  arrange(年份,政黨名稱)

write_csv(goVoteRate_byParty,file.path(analysis.path,"2012至今藍綠選舉投票率與催票率變化.csv"))

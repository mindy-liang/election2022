#### 競選連任比例 ####

ccm_behind_all <- read_sheet(ss = "1oWgoobgYUT8josvZuCShhe7kdw-gdowF3RIXRLiCEI8",
                             sheet = "2022公職參選人暗公報_六都")

#### 2018 現任 ##

ccm_behind_all %>%
  filter(`2018當選` == "y") %>%
  mutate(選舉區 = sprintf("%02d",選舉區),
         號次 = as.character(號次))->win.2018

win.2018$縣市 <- gsub("台","臺",win.2018$縣市)

run_again <- ccm_behind_all %>%
  group_by(縣市) %>%
  summarise(爭取連任 = sum(`2018當選`=="y"),
            參選人數 = n(),
            爭取連任rate  = 爭取連任/參選人數)

write_sheet(run_again,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "11.六都爭取連任議員人數及比例")

## 比對最終當選名單

win.2022 <- citycons.all %>%
  filter(is.na(deptCode)& !is.na(政黨分類))%>%
  filter(candVictor %in% c("*","!"))

win.2018 %>%
  select(1:4,8,10) %>%
  left_join(select(win.2022, c(2,3,5,6,9,11)),
            by = c("縣市", "選舉區"="areaCode",
                   "號次"= "candNo","姓名", "推薦之政黨")) -> win_again.2022

win_again.2022 %>%
  group_by(縣市) %>%
  summarise(成功連任 = length(candVictor[ which(candVictor=="*")]),
            爭取連任人數 = n(),
            未成功連任 = 爭取連任人數-成功連任,
            成功連任rate  = 成功連任/爭取連任人數,
            連任失敗rate = 未成功連任/爭取連任人數) %>%
  select(1,2,4,3,5,6)-> win_again_Rate.2022

lose_2022 <- win_again.2022 %>%
  filter(is.na(candVictor))

write_sheet(win_again_Rate.2022,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "11-1.六都爭取連任議員成功/失敗人數比例")

write_sheet(lose_2022,
            ss = "1JDiHbk4jORtrUoWBHdIELakqQMMVygs-I8xKvTo7v9M",
            sheet = "11-2.六都議員連任失敗清單")




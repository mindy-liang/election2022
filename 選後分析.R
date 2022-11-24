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

#取用readr資料
congress.2020.all <- read_csv(curl("https://raw.githubusercontent.com/readr-media/readr-data/master/election_history/congress_elctks_2020_all.csv")) %>%
  mutate(type = "區域立委")
congress.2020.am <- read_csv(curl("https://raw.githubusercontent.com/readr-media/readr-data/master/election_history/congress_elctks_2020_am.csv")) %>%
  mutate(type = "山地原住民")
congress.2020.an <- read_csv(curl("https://raw.githubusercontent.com/readr-media/readr-data/master/election_history/congress_elctks_2020_an.csv"))%>%
  mutate(type = "平地原住民")

#取得政黨資料
elbase <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elbase.csv", col_names = FALSE) #總表
elcand <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elcand.csv", col_names = FALSE) #候選人基本資訊
elctks <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elctks.csv", col_names = FALSE) #得票狀況
elpaty <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elpaty.csv", col_names = FALSE) #參選政黨
elprof <- read_csv("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/2022 九合一大選/voteData/voteData/2020總統立委/不分區政黨/elprof.csv", col_names = FALSE) #各縣市鄉鎮得票統計表

#合併資料

congress.2020 <- rbind(congress.2020.all,congress.2020.am,congress.2020.an) %>%
  group_by()

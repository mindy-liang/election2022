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

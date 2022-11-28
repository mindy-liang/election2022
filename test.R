#final

win.citycons.2022 <- citycons.all %>%
  filter(is.na(deptCode)& !is.na(政黨分類))%>%
  group_by(政黨分類) %>%
  summarise(當選人數 = sum(candVictor=="*",candVictor=="!"),
            參選人數 = n(),
            當選比例 = round(當選人數/參選人數*100,2)) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2022")


#running

citycons_r <- rbind(runningRaw$T1,runningRaw$T2,runningRaw$T3) %>%
  unnest(cols = c(candTksInfo)) %>%
  select(1:10,13,17) %>%
  rename("投票數" = 11,"選舉人數" = 12)%>%
  mutate(candNo = as.character(candNo))

citycons.all_r <- citycons_r %>%
  filter(is.na(tboxNo)) %>%
  left_join(select(citycons.list, -"deptCode"),
            by = c("prvCode", "cityCode","areaCode",
                   "candNo" = "抽籤號次")) %>%
  select(13,14,3,4,7,16:19,8:12) %>%
  mutate(政黨分類 = case_when(
    推薦之政黨== "中國國民黨" ~ "中國國民黨",
    推薦之政黨== "民主進步黨" ~ "民主進步黨",
    推薦之政黨== "台灣民眾黨" ~ "台灣民眾黨",
    推薦之政黨== "時代力量" ~ "時代力量",
    推薦之政黨== "無" ~ "無黨籍",
    推薦之政黨!= c("中國國民黨","民主進步黨",
              "台灣民眾黨","時代力量","無") ~ "其他政黨")) %>%
  filter(is.na(deptCode))


win.citycons.2022_r <- citycons.all_r %>%
  filter(candVictor %in% c("*","!")) %>%
  group_by(政黨分類) %>%
  summarise(當選人數 = n()) %>%
  mutate(席次佔比 = round(當選人數/sum(當選人數)*100,2), 年份 = "2022")
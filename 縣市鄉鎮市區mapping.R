mapping.path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/election2022/mapping data"

#### import ####

#檔案位置
county <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年憲法修正案公民複決縣市及鄉鎮市區代碼表.xlsx"
village_name <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年村里名對照表.xlsx"

#讀取縣市名稱excel
sheetNames <- excel_sheets(county)

county.list <- lapply(sheetNames, function(x){
  read_excel(county, sheet = x, skip = 1, col_types = "text")
})

#讀取村里名稱excel
sheetNames <- excel_sheets(village_name)

village.list <- lapply(sheetNames, function(x){
  read_excel(village_name, sheet = x, col_types = "text")
})


#整合list為dataframe
election_county_mapping <- do.call(rbind,county.list) %>%
  rename(prvCode  = 1,
         cityCode = 2,
         deptCode = 3,
         name = 4)

election_village_mapping <- do.call(rbind,village.list) %>%
  rename(prvCode  = 1,
         cityCode = 2,
         deptCode = 3,
         liCode = 4,
         VILLNAME = 5)


#define資料樣式 輸出為mapping用檔案

#縣市、鄉鎮市區層級
election_city_mapping <- election_county_mapping %>%
  mutate(COUNTYNAME = ifelse(deptCode=="000",name, NA),
         TOWNNAME = ifelse(deptCode=="000",NA, name)) %>%
  mutate(COUNTYNAME = ifelse(prvCode %in% c("63","64","65","66","67","68")|
                             prvCode %in% c("09","10")&cityCode!="000",COUNTYNAME,NA)) %>%
  filter(prvCode!="00") %>%
  filter(!(name %in% c("臺灣省","福建省"))) %>%
  arrange(prvCode,cityCode) %>%
  select(-4) %>%
  fill(COUNTYNAME, .direction = "down") 


write_csv(election_city_mapping, file.path(mapping.path,"election_city_mapping.csv"))

#縣市、鄉鎮市區、村里層級
election_all_mapping <- election_city_mapping %>%
  left_join(election_village_mapping) %>%
  select(1:3,6,4,5,7)

write_csv(election_all_mapping, file.path(mapping.path,"election_village_mapping.csv"))

#### 政黨資訊 ####

#輸入2022年政黨excel表

party <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年政黨代碼表.xlsx"

party.name <- read_excel(party, col_types = "text")

write_csv(party.name, file.path(mapping.path,"party_mapping.csv"))


#### 各公職候選人名單 ####

#讀取檔案夾內所有excel檔

file.list <- list.files(path = "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/各公職候選人", 
                        pattern='*.xls')

candidate.list <- lapply(file.list, function(x) {
  read_excel(path = paste("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/各公職候選人",x,sep = "/"),
             skip = 4)
})

file.name <- sub(".xls","",file.list)

candidate.list <- mapply(cbind, candidate.list, "公職類別"=file.name, SIMPLIFY=F)

#去除出生地column以取得資料一致
candidate.list <- lapply(candidate.list, function(x) x[!(names(x) %in% c("出生地"))])

#合併所有公職候選人資料
candidate <- do.call(rbind, candidate.list)

write_csv(candidate, file.path(mapping.path,"2022九合一選舉各公職候選人清單.csv"))

#依公職分類(只看縣市長、縣市議員、村里長)
village.list <- candidate.list[[1]] %>%
  mutate(參選年齡 = 111- as.numeric(substr(出生年月日, 1, 3))) %>%
  select(1:3,8,5:7)
  
citymayor.list <- rbind(candidate.list[[5]],candidate.list[[7]]) %>%
  mutate(參選年齡 = 111- as.numeric(substr(出生年月日, 1, 3))) %>%
  select(1:3,8,5:7)

citycons.list <- rbind(candidate.list[[4]],candidate.list[[6]]) %>%
  mutate(參選年齡 = 111- as.numeric(substr(出生年月日, 1, 3))) %>%
  select(1:3,8,5:7)

#將上述公職候選人資料加入縣市村里編碼

#縣市長
citymayor.list %>%
  left_join(filter(election_county_mapping,deptCode=="000"),by = c("選舉區" = "name")) %>%
  select(7:10,1,6,2:5) %>%
  rename("縣市" =5)-> citymayor.list

write_csv(citymayor.list, file.path(mapping.path,"candidate_citymayor_mapping.csv"))

#縣市議員

citycons.list %>%
  separate(col = 選舉區, into = c("縣市","選區"),sep = 3) %>%
  mutate(areaCode = stringr::str_extract(選區, "\\d+")) %>%
  left_join(filter(election_county_mapping,deptCode=="000"),by = c("縣市" = "name")) %>%
  select(8,10:12,9,1,2,7,3:6) %>%
  mutate(areaCode = sprintf("%02d",as.numeric(areaCode)))-> citycons.list
  
write_csv(citycons.list, file.path(mapping.path,"candidate_citycons_mapping.csv"))

#村里長

map_village <- election_all_mapping %>%
  filter(!is.na(liCode)) %>%
  unite("合併NAME",5:7,sep = "") 

village.list %>%
  left_join(filter(map_village,!is.na(liCode)),by = c("選舉區" = "合併NAME")) %>%
  separate(col = 選舉區, into = c("縣市","鄉鎮市區","村里"),sep = c(3,6)) %>%
  select(9:13,1:3,8,4:7) ->village.list
  
write_csv(village.list, file.path(mapping.path,"candidate_village_mapping.csv"))

#### 選區 mapping ####

area_raw <- read_xlsx("~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/選區劃分-中選務字第1113150255號公告.xlsx")

area_raw$行政區 <- sub("及","、",area_raw$行政區)

area_mapping <- area_raw %>%
  separate_rows(行政區, sep = "、")%>%
  mutate(行政區 = str_trim(行政區), 選區 = as.character(parse_number(選區)))

write_csv(area_mapping, file.path(mapping.path, "中選會公告2022年選區劃分.csv"))

## 以本屆中選會資料反推選舉區範圍 ##

area_2022 <- citycons %>%
  select(1:4) %>%
  filter(!is.na(deptCode)) %>%
  unique()

election_all_mapping %>%
  left_join(area_2022) %>%
  select(1,2,8,3:7) -> election_area_mapping

write_csv(election_area_mapping, file.path(mapping.path, "election_city_mapping.csv"))



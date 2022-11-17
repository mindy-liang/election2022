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

election_all_mapping <- election_city_mapping %>%
  left_join(election_village_mapping) %>%
  select(1:3,6,4,5,7)

write_csv(election_all_mapping, file.path(mapping.path,"election_village_mapping.csv"))

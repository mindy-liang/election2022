mapping.path <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/election2022/mapping data"

#### import ####

county <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年憲法修正案公民複決縣市及鄉鎮市區代碼表.xlsx"
village_name <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年村里名對照表.xlsx"

sheetNames <- excel_sheets(county)

county.list <- lapply(sheetNames, function(x){
  read_excel(county, sheet = x, skip = 1, col_types = "text")
})

sheetNames <- excel_sheets(village_name)

village.list <- lapply(sheetNames, function(x){
  read_excel(village_name, sheet = x, col_types = "text")
})

election_county_mapping <- do.call(rbind,county.list) %>%
  rename(prvCode  = 1,
         cityCode = 2,
         deptCode = 3,
         name = 4)

election_city_mapping <- election_county_mapping %>%
  mutate(COUNTYNAME = ifelse(deptCode=="000",name, ""),
         TOWNNAME = ifelse(deptCode=="000","NA", name)) %>%
  mutate(COUNTYNAME = ifelse(prvCode %in% c("63","64","65","66","67","68")|
                             prvCode %in% c("09","10")&cityCode!="000",COUNTYNAME,"")) %>%
  arrange(prvCode,cityCode) 

test <- election_city_mapping %>%
  filter(cityCode!="000") %>%
  fill(COUNTYNAME, .direction = "down")

write_csv(election_city_mapping, file.path(mapping.path,"election_city_mapping.csv"))

election_village_mapping <- do.call(rbind,village.list) %>%
  rename(prvCode  = 1,
         cityCode = 2,
         deptCode = 3,
         liCode = 4,
         VILLNAME = 5)

test <- election_city_mapping %>%
  left_join(election_village_mapping)

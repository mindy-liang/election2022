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

election_village_mapping <- do.call(rbind,village.list) %>%
  rename(prvCode  = 1,
         cityCode = 2,
         deptCode = 3,
         liCode = 4,
         VILLNAME = 5)

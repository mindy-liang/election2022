#### import ####

county <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年憲法修正案公民複決縣市及鄉鎮市區代碼表.xlsx"
village_name <- "~/Library/Mobile Documents/com~apple~CloudDocs/Documents/九合一選舉-中選會/mapping/111年村里名對照表.xlsx"

county.list <- lapply(county,function(x) {
  sheets <- excel_sheets(x)
  dfs <- lapply(sheets, function(y) {
    read_excel(x, sheet = y,skip=1)
  })
  names(dfs) <- sheets
  dfs
})



village.list <- lapply(village_name,function(x) {
  sheets <- excel_sheets(x)
  dfs <- lapply(sheets, function(y) {
    read_excel(x, sheet = y,skip=0)
  })
  names(dfs) <- sheets
  dfs
})
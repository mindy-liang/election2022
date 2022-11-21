# election2022

存放2022 九合一選舉 R工作檔

----------

mapping data <- 與中選會串接資料做比對

voteData <- 中選會api串接 原始資料 & 特定公職資料

選後分析圖表用data <- 選後分析用，當天會使用R跑完程式後直接倒入，可進Google sheet進一步處理

----------
前綴

- citymayor <- 直轄市長/縣市長
- citycons <- 直轄市議員／縣市議員
- village <- 村里長
- election <- 選舉區域資料
- candidate <- 本屆各公職候選人資訊

後綴
- voteRate <- 得票率
- GoVoteRate <- 催票率
- mapping <- 本屆比對資料
- city <- 省市、縣市、鄉鎮市區層級資料
- all <- 省市、縣市、鄉鎮市區、村里層級資料


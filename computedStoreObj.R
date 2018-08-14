# The packages loading
library(data.table)
library(magrittr)
library(RMySQL)
library(plotly)


# 全站這半年內上架狀況
### 鎖定今年(2018)內的車子上架的物件數
bidb_car_2018 <- subset(bidb_car, as.POSIXct(bidb_car$CreateDate, '%Y-%m-%d %H:%M:%S', tz = "GMT") > "2018-01-01 00:00:00" )
### 只取日期
bidb_car_2018$addDate <- as.Date(bidb_car_2018$CreateDate, "%Y-%m-%d")
### 計算每個日期的上架數
countObjEveryDay <- data.frame(date = table(bidb_car_2018$addDate) %>% names() %>% as.Date(),
                               count = table(bidb_car_2018$addDate) %>% as.numeric()
)
### 全站畫圖唷～
plot_ly(data = countObjEveryDay, x = countObjEveryDay$date, y = countObjEveryDay$count, type = "scatter", mode = "lines")

# ---------------
# 各個車商的上架時間序列
# 做一個Function

computedStoreObj <- function(oder){
  ## Inner join two data frame
  subCar <- merge(x = subset(bidb_car, bidb_car$Email == newCerMember$email[oder]), y = bidb_mappingBrandID,
                  by.x = "BrandID", by.y = "BrandID", all.x = TRUE, all.y = FALSE)
  
  store_car_2018 <- subset(subCar, as.POSIXct(subCar$CreateDate, '%Y-%m-%d %H:%M:%S', tz = "GMT") > "2018-01-01 00:00:00" )
  ### 只取日期
  store_car_2018$addDate <- as.Date(store_car_2018$CreateDate, "%Y-%m-%d")
  ### 計算每個日期的上架數
  storeCountObjEveryDay <- data.frame(date = table(store_car_2018$addDate) %>% names() %>% as.Date(),
                                      count = table(store_car_2018$addDate) %>% as.numeric()
  )
  ### 設定半年的時間序列
  timeSeries <- seq.Date(from = as.Date('2017-12-31'), to = as.Date('2018-06-30'), by = 'day') %>% 
    data.frame(allTime=.) 
  ### 比對半年內的上架物件日期
  storeCountObj <- merge(x = storeCountObjEveryDay, y = timeSeries, by.x = 'date', 
                         by.y = 'allTime', all.x = TRUE, all.y = TRUE) 
  ### NA值補0
  storeCountObj$countPlot <- ifelse(is.na(storeCountObj$count)==TRUE, 0, storeCountObj$count)
  
  ### 針對各車商畫圖
  plotScatter <- plot_ly(data = storeCountObj, 
                         x = storeCountObj$date, y = as.numeric(storeCountObj$countPlot), 
                         type = "scatter", mode = "lines+markers")
  
  return(plotScatter)
}

# 日期



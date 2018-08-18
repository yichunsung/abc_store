# Project main enterance
print(paste("啟動時間", Sys.time()))
source('computedPrice.R')
dataOutput <- outputPriceTag()
print(paste("完成時間", Sys.time()))

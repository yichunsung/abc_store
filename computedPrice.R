# 算價格
computedPrice <- function(order){
  # loading loadingData.R function
  source('loadingData.R')
  # get data from loadingData.R -> combineStoreCar() ,MySQLCarDataLoading(), MySQLBrandIDLoading()
  bidb_car <- MySQLCarDataLoading()
  bidb_mappingBrandID <- MySQLBrandIDLoading()
  newCerMember <- combineStoreCar()
  subCar <- merge(x = subset(bidb_car, bidb_car$Email == newCerMember$email[order]), y = bidb_mappingBrandID,
                  by.x = "BrandID", by.y = "BrandID", all.x = TRUE, all.y = FALSE)
  
  storePrice <- data.frame(price = subCar$Price)
  
  storePrice$priceInv <- ifelse(as.numeric(as.vector(storePrice$price)) <50 , 49, as.numeric(as.vector(storePrice$price)))
  storePrice$priceInv <- ifelse(as.numeric(as.vector(storePrice$priceInv)) >=50 & as.numeric(as.vector(storePrice$priceInv)) <100 , 99, as.numeric(as.vector(storePrice$priceInv)))
  storePrice$priceInv <- ifelse(as.numeric(as.vector(storePrice$priceInv)) >=100 & as.numeric(as.vector(storePrice$priceInv)) <150 , 149, as.numeric(as.vector(storePrice$priceInv)))
  storePrice$priceInv <- ifelse(as.numeric(as.vector(storePrice$priceInv)) >=150 & as.numeric(as.vector(storePrice$priceInv)) <200 , 199, as.numeric(as.vector(storePrice$priceInv)))
  storePrice$priceInv <- ifelse(as.numeric(as.vector(storePrice$priceInv)) >=200, 201, as.numeric(as.vector(storePrice$priceInv)))
  
  storePrice$priceInv <- sub(49, "under50", storePrice$priceInv)
  storePrice$priceInv <- sub(99, "50to100", storePrice$priceInv)
  storePrice$priceInv <- sub(149, "100to150", storePrice$priceInv)
  storePrice$priceInv <- sub(199, "150to200", storePrice$priceInv)
  storePrice$priceInv <- sub(201, "over200", storePrice$priceInv)
  storePrice$priceInv <- sub("1under50", "100to150", storePrice$priceInv)
  storePrice$priceInv <- sub("150to100", "150to200", storePrice$priceInv)
  
  table(storePrice$priceInv)
  
}

# output 
outputPriceTag <- function(){
  source('loadingData.R')
  newCerMember <- combineStoreCar()
  # create a new data frame for output data frame
  storePriceTable <- data.frame()
  # define the tag
  priceType <- c('under50', '50to100', '100to150', '150to200', 'over200')
  for(i in 1:nrow(newCerMember)){
    loop_forStore <- computedPrice(i) 
    addData <- c()
    for(j in 1: length(priceType)){
      subType <- subset(loop_forStore, names(loop_forStore) == priceType[j])
      if(length(subType) == 0){
        subType <- 0
        addData <- c(addData, as.numeric(subType))
      }else{
        addData <- c(addData, as.numeric(subType))
      }
    }
    storePriceTable <- rbind(storePriceTable, addData)
  }
  cbindDf <- data.frame(newCerMember$bidbName, newCerMember$cerMemberID, newCerMember$email)
  storeOutputPrice <-  data.frame(storeName = newCerMember$bidbName, storeMId = newCerMember$cerMemberID, storeMail = newCerMember$email,
                                  objPriceType_U50 = storePriceTable[,1],
                                  objPriceType_U100 = storePriceTable[,2],
                                  objPriceType_U150 = storePriceTable[,3],
                                  objPriceType_U200 = storePriceTable[,4],
                                  objPriceType_O200 = storePriceTable[,5]
  )
  # calculate percent of every price interval
  storeOutputPrice$sumObj <- storeOutputPrice$objPriceType_U50+storeOutputPrice$objPriceType_U100+storeOutputPrice$objPriceType_U150+storeOutputPrice$objPriceType_U200+storeOutputPrice$objPriceType_O200
  storeOutputPrice$u50_p <- (storeOutputPrice$objPriceType_U50/storeOutputPrice$sumObj)*100
  storeOutputPrice$u100_p <- (storeOutputPrice$objPriceType_U100/storeOutputPrice$sumObj)*100
  storeOutputPrice$u150_p <- (storeOutputPrice$objPriceType_U150/storeOutputPrice$sumObj)*100
  storeOutputPrice$u200_p <- (storeOutputPrice$objPriceType_U200/storeOutputPrice$sumObj)*100
  storeOutputPrice$o200_p <- (storeOutputPrice$objPriceType_O200/storeOutputPrice$sumObj)*100
  
  # define the tag for each store
  tagPrice <- c()
  for(k in 1:nrow(storeOutputPrice)){
    tag_num <- storeOutputPrice[k, 10:14]
    for(l in 1:5){
      num <- subset(tag_num[l], tag_num[l]>= 50 )
      if(nrow(num) >=1){
        tagName <- names(num)
        break
      }else if(nrow(num)==0){
        tagName <- "NoTag"
      }
    }
    tagPrice <- c(tagPrice, tagName)
  }
  storeOutputPrice$tagPrice <- tagPrice
  # output data frame
  return(storeOutputPrice)
}
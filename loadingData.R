# Loading Data and combine data
## Local data loading

localDataLoading <- function(){
  dataMembers <- read.csv('data/member.csv')
  return(dataMembers)
}
## MySQL data loading
### Members
MySQLMemberDataLoading <- function(){
  source("mysqlConnect.R")
  mysqlConnection()
  # Table reading
  bidb_member <- dbReadTable(mysqlConnection(), "memberphone")
  return(bidb_member)
}
### Cars
MySQLCarDataLoading <- function(){
  source("mysqlConnect.R")
  mysqlConnection()
  # Table reading
  bidb_car <- dbReadTable(mysqlConnection(), "car")
  return(bidb_car)
}
### BrandID
MySQLBrandIDLoading <- function(){
  source("mysqlConnect.R")
  mysqlConnection()
  # Table reading
  bidb_mappingBrandID <- dbReadTable(mysqlConnection(), "mapping_BrandID")
  return(bidb_mappingBrandID)
}


## Data rebuiding
combineStoreCar <- function(){
  source("mysqlConnect.R")
  ## Inner join two data frame
  TrueNameMember <- merge(x = MySQLMemberDataLoading(), y = localDataLoading(), by.x = "MemberID", by.y = "MemberID", all.x = FALSE, all.y = FALSE)
  
  newCerMember <- data.frame(
    cerMemberID = TrueNameMember$MemberID,
    bidbName = TrueNameMember$Name.x,
    bidbDisplayName = TrueNameMember$DisplayName.x,
    category = TrueNameMember$Category.x,
    type = TrueNameMember$Type,
    email = TrueNameMember$Email.x,
    joinDate = TrueNameMember$JoinDate,
    store = TrueNameMember$Store,
    phone = TrueNameMember$Mobile,
    address = TrueNameMember$Address,
    LastLoginTime = TrueNameMember$LastLoginDate,
    CreateTime = TrueNameMember$CreateDate, # 是否等於註冊時間
    ModifyTime = TrueNameMember$ModifyDate # 修改時間？？？？？
  )
  return(newCerMember)
}


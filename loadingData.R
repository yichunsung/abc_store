# Loading Data and combine data
## Local data loading
localDataLoading <- function(){
  dataMembers <- read.csv('~/data/member.csv')
  return(dataMembers)
}
## MySQL data loading
MySQLDataLoading <- function(){
  source("mysqlConnect.R")
  #mysqlConnection()
  # Table reading
  bidb_member <- dbReadTable(mysqlConnection(), "memberphone")
  return(bidb_member)
}

## Data rebuiding

## Inner join two data frame
TrueNameMember <- merge(x = bidb_member, y = dataMembers, by.x = "MemberID", by.y = "MemberID", all.x = FALSE, all.y = FALSE)

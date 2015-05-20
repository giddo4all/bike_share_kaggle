
#Creating dataframes and Assigning types
bikeshare.path <- "https://raw.githubusercontent.com/giddo4all/bike_share_kaggle/master/train.csv"
#bikeshare.path <- "C:/Users/Gideon/OneDrive/Lectures/Kaggle/bike_share_comp/"
train.datafile <- "train.csv"
test.datafile <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('character', # datetime
                        'factor',    # season
                        'factor',    # holiday
                        'factor',    # workingday
                        'factor',    # weather
                        'numeric',   # temp
                        'numeric',   # atemp
                        'numeric',   # humidity
                        'numeric',   # windspeed
                        'integer',   # casual
                        'integer',    # registered
                        'integer'     # count
)
test.column.types <- train.column.types[-12][-11][-10]     # # no Survived column in test.csv



#Function to read data in 
readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types)
}


readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types)
}

##############################Tried this but didnt work for github Import###
install.packages("data.table")
library(data.table)
install.packages("RCurl")
library(RCurl)

read.url <- function(url, ...){
  tmpFile <- tempfile()
  download.file(url, destfile = tmpFile, method = "curl")
  url.data <- fread(tmpFile, ...)
  return(url.data)
}


read.url("https://raw.githubusercontent.com/giddo4all/bike_share_kaggle/master/train.csv")

###############################################################################


library(RCurl)
x <- getURL("https://raw.githubusercontent.com/giddo4all/bike_share_kaggle/master/train.csv")
y <- read.csv(text = x)

#########################

install.packages("devtools")
library("devtools")
install_github("leeper/rio")
library("rio")
install.packages("Rtools")

tr.data <- import("https://raw.githubusercontent.com/giddo4all/bike_share_kaggle/master/train.csv", header = TRUE, colClasses=column.types, na.strings=missing.types)
te.data <- import("https://raw.githubusercontent.com/giddo4all/bike_share_kaggle/master/test.csv")

readData <- function(data.name, column.types, missing.types) {
  read.csv( data.name, colClasses=column.types, na.strings=missing.types)
}

############################

train.raw <- readData(tr.data, train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(bikeshare, test.datafile, 
                     test.column.types, missing.types)
df.infer <- test.raw


############################
setwd("C:/Users/Gideon/OneDrive/Lectures/Kaggle/bike_share_comp")

df.train <- read.csv("train.csv", header = TRUE, sep=",", colClasses=train.column.types, 
                       na.strings="NA")
df.test <- read.csv("test.csv", header = TRUE, sep=",", colClasses=test.column.types, 
                       na.strings="NA")


##http://stackoverflow.com/questions/14441729/read-a-csv-from-github-into-r


#convert to date

#df$DateTime <- as.POSIXct(df.train$datetime, format="%Y%m%d %H%M%S")
train.date <- df.train
test.date <- df.test


#################################Scrapp the below ######

train.date$datetime <- strptime(x = as.charcter(train.date$datetime), 
                                format = "%Y-%m-%d %H:%M:%S")

test.date$datetime <- strptime(x = as.character(test.date$datetime), 
                               format = "%Y-%m-%d %H:%M:%S")

colSums(is.na(df.train))

df.train[2225,]
nrow(df.train)

#################################Scrapp the above######

#WEEKDAY ....Extracting Weekday and make a factor

train.date$weekday<- strftime(train.date$datetime,'%A')
train.date$weekday <- factor(train.date$weekday)

test.date$weekday<- strftime(test.date$datetime,'%A')
test.date$weekday <- factor(test.date$weekday)


#Extracting timestamp and make a factor
train.date$timestamp<- strftime(train.date$datetime,'%R')
train.date$timestamp <- factor(train.date$timestamp)

test.date$timestamp<- strftime(test.date$datetime,'%R')
test.date$timestamp <- factor(test.date$timestamp)


######## TO SCRAPP#######Make weekday factor
#train.timefactor$weekday <- as.factor(train.timefactor[["weekday"]])
#test.weekfactor <- factor(test.timefactor$weekday)
train.timefactor$weekday <- weekdays(as.Date(train.date$datetime))
train.timefactor$weekday <- as.factor(train.date$day)

test.timefactor$weekday <- weekdays(as.Date(train.date$datetime))
test.timefactor$weekday <- as.factor(test.date$day)
######## TO SCRAPP#######Make weekday factor

aggregate(train.date[,"count"],list(train.date$weekday), mean)
aggregate(train.date[,"count"],list(train.date$season), mean)
aggregate(train.date[,"count"],list(train.date$holiday), mean)
aggregate(train.date[,"count"],list(train.date$timestamp), mean)
aggregate(train.date[,"count"],list(train.date$hourpart), mean)

#######Group Time#########

train.date$hour<- as.numeric(substr(train.date$time,1,2))
test.date$hour<- as.numeric(substr(test.date$time,1,2))

train.date$hourpart<- "7"

#12am-3am
train.date$hourpart[(train.date$hour < 4)] <- 1
test.date$hourpart[(test.date$hour < 4)] <- 1

#4am-7am
train.date$hourpart[(train.date$hour < 8) & (train.date$hour > 3)] <- 2
train.date$hourpart[(train.date$hour < 8) & (train.date$hour > 3)] <- 2

#8am-11am
train.date$hourpart[(train.date$hour < 12) & (train.date$hour > 7)] <- 3
test.date$hourpart[(test.date$hour < 12) & (test.date$hour > 7)] <- 3

#12hrs-15hrs
train.date$hourpart[(train.date$hour < 16) & (train.date$hour > 11)] <- 4
test.date$hourpart[(test.date$hour < 16) & (test.date$hour > 11)] <- 4

#16hrs-19hrs
train.date$hourpart[(train.date$hour < 20 ) & (train.date$hour > 15)] <- 5
test.date$hourpart[(test.date$hour < 20 ) & (test.date$hour > 15)] <- 5

#20hrs-24hrs
train.date$hourpart[(train.date$hour < 24 ) & (train.date$hour > 19)] <- 6
test.date$hourpart[(test.date$hour < 24 ) & (test.date$hour > 19)] <- 6

#convert hourpart to factor
train.date$hourpart <- as.factor(train.date$hourpart)
test.date$hourpart <- as.factor(test.date$hourpart)

#convert hour back to factor
train.date$hour <- as.factor(train.date$hour)
test.date$hour <- as.factor(test.date$hour)

##########################Quartile stuff###################

train.quart<-train.date
test.quart <-test.date

train.quart$aatemp <- cut (train.quart$atemp, 
                     breaks = quantile (train.quart$atemp, c (0, .25, .5, .75, 1)), 
                     include.lowest = TRUE)
aggregate(train.date[,"count"],list(aatemp = train.quart$aatemp), mean)

train.quart$temppart <- "5"

##Group atemp by quartile
train.quart$temppart[(train.quart$atemp <= 16.7) &(train.quart$atemp >= 0.76)] <- 1

train.quart$temppart[(train.quart$atemp <= 24.2) &(train.quart$atemp > 16.7)] <- 2

train.quart$temppart[(train.quart$atemp <= 31.1) &(train.quart$atemp > 24.2)] <- 3

train.quart$temppart[(train.quart$atemp <= 45.46) &(train.quart$atemp > 31.1)] <- 4

train.quart$temppart <- as.factor(train.quart$temppart)

##Group Humidity

train.quart$humpart <- "9"

aggregate(train.quart[,"count"],list(hum = train.quart$humidity), sum)
aggregate(train.quart[,"count"],list(hum = train.quart$windspeed), mean)

cut (train.quart$humidity, breaks = quantile (train.quart$humidity, c (0, .25, .5, .75, 1)), 
     include.lowest = TRUE)

train.quart$humpart[(train.quart$humidity <= 16.7) 
                    &(train.quart$atemp >= 0.76)] <- 1


#q1 <- c(0.76:16.7)

#train.quart$temppart[train.quart$aatemp == "q1"] <- 1

train.quart$temppart

aggregate (swiss$Infant.Mortality, list (qEdu = swiss$qEdu), FUN = mean)

aggregate(train.date[,"count"],list(aatemp = train.quart$aatemp), mean)

summary(train.date$timestamp)
summary(train.date$atemp)
summary(train.date$temp)
summary(train.date$humidity)
summary(train.date$windspeed)



##########Correlogram Time#########

install.packages("corrgram")
library(corrgram)
library(plyr)
corrgram.data <- train.date
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$season <- as.numeric(corrgram.data$season)
corrgram.data$workingday <- as.numeric(corrgram.data$workingday)
corrgram.data$season <- as.numeric(corrgram.data$season)
corrgram.data$weather <- as.numeric(corrgram.data$weather)
corrgram.data$hourpart <- as.numeric(corrgram.data$hourpart)
corrgram.data$hour<- as.numeric(corrgram.data$hour)
corrgram.data$weekday <- revalue(corrgram.data$weekday, 
                                  c("Monday" = 1, "Tuesday" = 2, "Wednesday" = 3, 
                                    "Thursday" = 4, "Friday" = 5, "Saturday" = 6,
                                    "Sunday" = 7))
corrgram.data$weekday<- as.numeric(corrgram.data$weekday)

## ##########Correlogram Time#########generate correlogram

corrgram.vars <- c("count", "hourpart", "windspeed", "weather", 
                   "season", "holiday", "workingday", "temp", "atemp", 
                   "hour", "weekday", "humidity", "registered", "casual")

corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.shade, upper.panel=panel.pie, 
         text.panel=panel.txt, diag.panel=panel.minmax, 
         main="Bike sharing Data")

##########Correlogram Time#########

####RANDOM FOREST#######
install.packages("randomForest")
library(randomForest)

  #Subsetting to take out irrelevant column
train.casual <- subset(train.quart, select= -c(datetime, count, registered))

set.seed(4543)

train.casualfit <- randomForest(casual ~ ., data=train.sub, ntree=500, keep.forest=FALSE,
                          importance=TRUE)
print(train.casualfit)
varImpPlot(train.casualfit)

train.reg <- subset(train.quart, select= -c(datetime, count, casual))

train.regfit <- randomForest(registered ~ ., data=train.sub, ntree=500, keep.forest=FALSE,
                                importance=TRUE)

####Another RF using count

#train.reg <- subset(train.quart, select= c(datetime, registered, casual))
set.seed(223)
train.count <- subset(train.quart, select= -c(datetime, registered, casual, temppart,
                                              holiday, hourpart, atemp, windspeed, timestamp))
train.countfit1 <- randomForest(count ~ ., data=train.count, ntree=500, keep.forest=FALSE,
                             importance=TRUE)
print(train.countfit1)
round(importance(train.countfit1), 3)
varImpPlot(train.countfit1)



train.countfit <- randomForest(count~ humidity+season+weather+temp+hour+weekday+workingday, data=train.quart, 
                               ntree=400, importance=TRUE)

test.quart$count <- predict(train.countfit, test.quart)

test.final <- test.quart

test.final$count <- round(test.final$count)

test.submit <- subset(test.final, select= c(datetime, count))

plot(df.train$count)

plot(test.submit$count)

write.csv(test.submit, file = "RF_Prediction.csv", row.names=FALSE)
         
#########GBM##############

print(names(train.quart))

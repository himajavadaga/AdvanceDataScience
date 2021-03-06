#NOTE: uncomment the lines from 6 to 12 if the packages are not installed and loaded
#      in your system. Installing and loading the libraries everytime you run the
#      program makes the execution time longer. Hence commented those parts.

#installing and importing the library
install.packages('stringr')
library(stringr)
install.packages('weatherData')
library(weatherData)
install.packages('outliers')
library(outliers)

#helps the user to pick the file
x=file.choose(new = FALSE)
 
#importing the file
rawdata<-read.csv(x, header = TRUE)

#removing the unecessary columns
rawdata$Channel=NULL

#removing the blank spaces
rawdata$Units=str_replace_all(rawdata$Units, " ", ".")

#removing the unnecesary records
rawdata <- subset(rawdata, Units=="kWh")

#reshaping the dataframe wide to long 
rawdata$Units=NULL
rawdata <- reshape(rawdata, 
             varying = c(colnames(rawdata[,-1:-2])), 
             v.names = "kWh",
             timevar = "hour", 
             times = sort(c(rep(seq(from=0,to=23,by=1),12))), 
             direction = "long")

# Forming a Subset
Account=rawdata[,1]

#Sorting the data frame by Date
rawdata=rawdata[order(rawdata$Date),]

#To Visualize the outliers for kwh using Box Plot
boxplot(rawdata$kWh,horizontal = TRUE)
boxplot.stats(rawdata$kWh)

#Replacing outliers with NA by Box Plot
outliers = boxplot(rawdata$kWh, plot=FALSE)$out
outliers
rawdata[rawdata$kWh %in% outliers,3]=NA
summary(rawdata)

#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(rawdata$kWh))
{
  if(is.na(rawdata$kWh[i])==TRUE)
  {
    rawdata$kWh[i]=mean(rawdata$kWh[i:(i+2)],na.rm=TRUE)
  }
}
summary(rawdata)

#aggregate by Date and hour to find hourly kWh
rawdata=aggregate(rawdata$kWh,
          list(Date = rawdata$Date, hour = rawdata$hour),
          sum)

#renaming the columns
colnames(rawdata)=c("Date","hour","kWh")

#Adding the column Account
rawdata$Account=c(rep(Account[1],length(nrow(rawdata))))

#Formatting Date column and Adding day,month, year, Dayofweek, weekday and Peakhour Columns
rawdata$Date <- as.Date(rawdata$Date, format="%m/%d/%Y")
rawdata$month=format(rawdata$Date, format = "%m")
rawdata$day=format(rawdata$Date, format = "%d")
rawdata$year= format(rawdata$Date, format = "%Y")
rawdata$DayofWeek = weekdays(rawdata$Date, abbreviate = TRUE)
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
rawdata$Weekday = factor((weekdays(rawdata$Date) %in% weekdays1), levels=c(FALSE, TRUE), labels=c(0, 1))                           
rawdata$Peakhour = with(rawdata, ifelse(hour > 6 & hour < 19, 1, 0))                            

#Sorting the data frame by Date and hour column
rawdata=rawdata[order(rawdata$Date,rawdata$hour),]                           

rawdata$DayofWeek <- with(rawdata, ifelse(DayofWeek=="Mon", 1,ifelse(DayofWeek=="Tue", 2,ifelse(DayofWeek=="Wed", 3, ifelse(DayofWeek=="Thu", 4, ifelse(DayofWeek=="Fri", 5, ifelse(DayofWeek=="Sat", 6, ifelse(DayofWeek=="Sun", 0,7))))))))

#rearranging the order of columns 
rawdata=rawdata[,c(4,1,3,5,6,7,2,8,9,10)]


#getting the hourly temperature data using R package
weather <- getWeatherForDate("KBOS", rawdata$Date[1], end_date=rawdata$Date[length(rawdata$Date)], opt_detailed = TRUE, opt_all_columns = TRUE)

#write.csv(weather,"Boston Weather.csv",row.names=FALSE)

#bon=file.choose(new = FALSE)
#weather<-read.csv(bon, header = TRUE)


#Binning and removing unnecessary columns
weather=weather[,c(1,3)]

#renaming the columns and Dealing with Date Time format
colnames(weather)=c("Date","Temperature")
weather$Date=as.POSIXct(weather$Date, tz="America/New_York")
weather$hour=format(weather$Date,format="%H")
weather$hour=as.numeric(weather$hour)
weather$Date=as.Date(as.character(as.POSIXct(weather$Date, tz="America/New_York")))

#Transforming the temperature data 

summary(weather)
weather=weather[,c(1,3,2)]
summary(weather)

weather=weather[order(weather$Date,weather$hour),]

#To Visualize the outliers for temperature using Box Plot
boxplot(weather$Temperature,horizontal = TRUE)
boxplot.stats(weather$Temperature)

#Replacing outliers with NA by Box Plot
outliers = boxplot(weather$Temperature, plot=FALSE)$out
outliers
weather[weather$Temperature %in% outliers,3]=NA
summary(weather)

#Replacing NAs with mean of next 2 observations for temperature
for(i in 1:length(weather$Temperature))
{
  if(is.na(weather$Temperature[i])==TRUE)
  {
    weather$Temperature[i]=mean(weather$Temperature[i:(i+2)],na.rm=TRUE)
  }
}
summary(weather)

#aggregating temperature by Date and hour
weather=weather[order(weather$Date,weather$hour),]
weather=aggregate(weather$Temperature,
                       list(Date = weather$Date, hour = weather$hour),
                       mean)
summary(weather)

#renaming the columuns after aggregation
colnames(weather)=c("Date","hour","Temperature")
colnames(weather)

# rounding the decimal points in Temperature
weather$Temperature <- round(weather$Temperature,digits=0)

#merging the two data frames by left outer join
sampleformat=merge(rawdata, weather,by=c("Date","hour"),all.x=TRUE)
summary(sampleformat)

#rearranging the order of columns for the desired output
sampleformat=sampleformat[,c(3,1,4,5,6,7,2,8,9,10,11)]

#Sorting the merged data by Date and hour
sampleformat=sampleformat[order(sampleformat$Date,sampleformat$hour),]

#To Visualize the outliers for merged data using Box Plot
boxplot(sampleformat$Temperature,horizontal = TRUE)
boxplot.stats(sampleformat$Temperature)

#checking for outliers in merged data and replacing them with NA
outliers = boxplot(sampleformat$Temperature, plot=FALSE)$out
outliers
sampleformat[sampleformat$Temperature %in% outliers,3]=NA
summary(sampleformat)

#checking for NA's in merged data and replacing them with mean of 2 consecutive observations 
for(i in 1:length(sampleformat$Temperature))
{
  if(is.na(sampleformat$Temperature[i])==TRUE)
  {
    sampleformat$Temperature[i]=mean(sampleformat$Temperature[i:(i+2)],na.rm=TRUE)
  }
}
for(i in 1:length(sampleformat$Temperature))
{
  if(is.na(sampleformat$Temperature[i])==TRUE)
  {
    sampleformat$Temperature[i]=mean(sampleformat$Temperature[i:(i+2)],na.rm=TRUE)
  }
}

#Replacing NAs with mean of next 2 observations for kwh
for(i in 1:length(sampleformat$kWh))
{
  if(is.na(sampleformat$kWh[i])==TRUE)
  {
    sampleformat$kWh[i]=mean(sampleformat$kWh[i:(i+2)],na.rm=TRUE)
  }
}

summary(sampleformat)

#rounding the decimal values in Temperature
sampleformat$Temperature <- round(sampleformat$Temperature,digits=0)

sampleformat$month <- as.numeric(sampleformat$month)
sampleformat$day <- as.numeric(sampleformat$day)
sampleformat$year <- as.numeric(sampleformat$year)
sampleformat$Weekday <- as.numeric(sampleformat$Weekday)

summary(sampleformat)
write.csv(sampleformat, "sample format.csv",row.names = FALSE)

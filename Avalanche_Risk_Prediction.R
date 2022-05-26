#============================ PACKAGES =============================#


# package for wrangling data(contains dplyr,tidyr,ggplot2,readr,tibble,stringr,forcats,purrr,)
library(tidyverse)

# package for date manipulation
library(lubridate)

# package for GAM
library(mgcv)

#creating  NA values
#install.packages("naniar")
library(naniar)



# read the data in csv file
data <- read_csv("Any_snowprofiles_2007-12-01_to_2022-05-02.csv",
                 col_types = cols(Grid = col_number(),
                                  Aspect = col_number(),
                                  `Wind Dir` = col_number(),
                                  `Summit Wind Dir` = col_number(),
                                  `Avalanche Code` = col_number()))

#==========================checking all the variable names and data type=================================================
glimpse(data)


#======================================OAH and FAH==============================================================
# Create new variables for Observed aval. hazard
data$OAH <- data$`Observed aval. hazard`

# Create new variables for Forecasted aval. hazard
data$FAH <- data$`Forecast aval. hazard`

#Checking if it worked
data$`Observed aval. hazard`
data$OAH
data$FAH

#=============================filter by area to select the required location and remove na =================================================

#FP<-function(OAH){sum(is.na(OAH))/length(OAH)*100}
#apply(data,2,FP)
#summary(data)

data = filter(data, data$Area == "Lochaber")

data <- data %>% filter(!is.na(FAH), !is.na(OAH))


#================================ Variable selection =========================#
data=data %>% select(c(Date,Area,X,Y,Lat,Long,Location,`Air Temp`,`Wind Dir`,
                       `Wind Speed`,Cloud,`Precip Code`,`Rain at 900`,
                       OAH,FAH))

FP<-function(OAH){sum(is.na(OAH))/length(OAH)*100}
apply(data,2,FP)
summary(data)
#====================================create new variables=========================================================================


# renaming variables to get rid of white space
names(data) <- str_replace_all(names(data), " ", "_")
#names(data)



#Date

#Seperate year and month
data$day_ = str_c(as.character(day(data$Date)),
                  as.character(month(data$Date, label = T)), sep = " ")
#data$date = str_c(as.character(day(data$Date)),
#                  as.character(month(data$Date, label = T)),
#                  as.character(year(data$Date)), sep = " ")

# what's the earliest date that's in the later months of the year?
data <- data %>% mutate( year = year(Date),day_in_year = yday(Date))
data_temp <- data %>% filter(day_in_year > 180)
data_temp %>% group_by(year) %>% summarize(earliest_forecast_date = min(Date),
                                           earliest_forecast_date_yday = min(day_in_year)) %>% ungroup()

# looking through table, earliest forecast looks like 19 Nov, day 323 of year,
# so this should be day 1
data <- data %>% mutate(days_since_19Nov = day_in_year - 323)
hist(data$days_since_19Nov)

# problem is now any dates before 19 Nov are negative
# for example: 1 Jan is -322
data %>% dplyr::select(Date, day_in_year, days_since_19Nov) %>%
  filter(year(Date) == 2010, day(Date) == 1, month(Date) == 1)
# what should it be? How many days are there between 1 Jan and 19 Nov
ymd("2011-01-01") - ymd("2010-11-19")
# this is 365 - 322
#365-322

# another example: 31 Mar is -233
data %>% dplyr::select(Date, day_in_year, days_since_19Nov) %>%
  filter(year(Date) == 2010, day(Date) == 31, month(Date) == 3)
# what should it be? How many days are there between 31 Mar and 19 Nov
ymd("2011-03-31") - ymd("2010-11-19")
# this is 365 - 233
#365-233

# so any negative value, we just need to take 365 + that value to get the right answer
data <- data %>% mutate(days_since_19Nov = ifelse(days_since_19Nov < 0, 365 + days_since_19Nov,
                                                  days_since_19Nov))


#data %>% filter (!month(Date)==02 , !day(Date)==29)


hist(data$days_since_19Nov)

# days_since_19Nov is your "time" variable for the GAM
#==================================================================================
#to work out how many forecast days there are per month, on average over all the years

data %>% count(year(Date), month(Date)) %>% # number of obs per month per year
  rename(year = `year(Date)`, month = `month(Date)`) %>% # renaming variables
  complete(year, month, fill = list(n = 0)) %>% # months with no observations should get a 0
  filter(!((year == 2007) & (month < 6))) %>% # data only starts winter 2012
  filter((!((year == 2022) & (month > 6)))) %>% # winter 2022 is in the future
  group_by(month) %>% # now work out avg number of forecasts per month, over all years
  summarize(mean_days_forecast = mean(n))




#================= checking the distribution of data =================#

# spatial locations are saved in lat-long coordinates or UTM (x,y)
# there are some erroneous locations so this just removes them manually
plot(data$X, data$Y)
# there are some outliers
centre_Lochaber <- c(220000, 770000)
data <- data %>% 
  mutate(d2centre = sqrt((X - centre_Lochaber[1])^2 + (Y - centre_Lochaber[2])^2)/1000)
hist(data$d2centre)

# remove any locations more than 12km from centre, this gets rid of the worst outliers
data <- data %>% filter(d2centre <= 12)

plot(data$X, data$Y)

# remove any observations with missing coordinates
data <- data %>% filter(!is.na(Lat), !is.na(Long))

par(mfrow=c(2,3))

# distribution of air temps
hist(data$Air_Temp,col="green",xlab="Temperature",ylab="Frequencies",
     main="Histogram of Air Temperature")

# distribution of Wind Speed
hist(data$Wind_Speed,col="green",xlab="Wind speed",ylab="Frequencies",
     main="Histogram of Wind Speed") 

# distribution of Wind direction
hist(data$Wind_Dir,col="green",xlab="Wind Dir",ylab="Frequencies",
     main="Histogram of Wind Direction") 

# distribution of Cloud
hist(data$Cloud,col="green",xlab="Cloud",ylab="Frequencies",
     main="Histogram of Cloud") 


barplot(table(data$Precip_Code), las=2,col="green",ylab="Frequencies",
        main="Bar plot Preciptation Code")

barplot(table(data$Rain_at_900), las=2,col="green",ylab="Frequencies",
        main="Bar plot of Rain")


#  change -9999 to NA
data <- data %>% replace_with_na_all(condition = ~.x == -9999)

#=============================PLOT THE HISTOGRAMS AGAIN==================================#
par(mfrow=c(3,3))
hist(data$Air_Temp,col="blue",xlab="Temperature",ylab="Frequencies",main="Histogram of Air Temperature") # distribution of air temps

#hist(data$Wind_Speed,col="blue",xlab="Wind speed",ylab="Frequencies",main="Histogram of Wind Speed") # distribution of Wind Speed

hist(data$Wind_Dir,col="blue",xlab="Wind Dir",ylab="Frequencies",main="Histogram of Wind Direction") # distribution of Wind direction

hist(data$Cloud,col="blue",xlab="Cloud",ylab="Frequencies",main="Histogram of Cloud") # distribution of Cloud

hist(data$d2centre, col="blue",xlab="Location",ylab="Frequencies",main="Histogram of location") # distribution of locations)

barplot(table(data$Precip_Code), las=2,col="blue",ylab="Frequencies",main="Barplot of Preciptation Code")


barplot(table(data$Rain_at_900), las=2,col="blue",ylab="Frequencies",main="Barplot of Rain")

plot(data$X, data$Y)
#====================check for outliers in Wind speed=========================#
library(outliers)
testWS <- grubbs.test(data$Wind_Speed)
testWS


#=====================     Removing outliers      ====================#
data <- subset(data, Wind_Speed <=70 )

hist(data$Wind_Speed,col="blue",xlab="Wind speed",ylab="Frequencies",main="Histogram of Wind Speed") # distribution of Wind Speed

#===========================================map===========================
# plot on nice map
library(sf)
library(leaflet)

data <- st_as_sf(data, coords = c("Long", "Lat"), crs = 4326)
pal <- colorFactor("YlOrRd", domain = data$Area)
leaflet() %>%
  addProviderTiles(provider= "OpenStreetMap") %>%
  addCircles(data = data %>% st_transform(4326), color = ~pal(Area), fillOpacity = 0.7, weight = 1, group = "editable")

#=======================================RESPONSE VARIABLE===================================

#checking how the levels in Observed aval. hazard  are ordered 
table(data$OAH)
class(data$OAH)

# re-coding these as factor variables, this tells R what the order of the levels is
data$OAH <- factor(data$OAH, levels = c("Low", "Moderate", "Considerable -", "Considerable +", "High"))



#checking if the levels are factors
table(data$OAH)
class(data$OAH)

#creating a new variable containing "numeric" versions of the levels
data$OAH_num = case_when(
  data$OAH == "Low" ~ 1,
  data$OAH == "Moderate" ~ 2,
  data$OAH == "Considerable -" ~ 3,
  data$OAH == "Considerable +" ~ 4,
  data$OAH == "High" ~ 5,
)

table(data$OAH_num)
class(data$OAH_num)

# re-coding these as factor variables
data$FAH <- factor(data$FAH, levels = c("Low", "Moderate", "Considerable -", "Considerable +", "High"))


#checking if the levels are factors
table(data$FAH)
class(data$FAH)

#creating a new variable containing "numeric" versions of the levels
data$FAH_num = case_when(
  data$FAH == "Low" ~ 1,
  data$FAH == "Moderate" ~ 2,
  data$FAH == "Considerable -" ~ 3,
  data$FAH == "Considerable +" ~ 4,
  data$FAH == "High" ~ 5,
)

table(data$FAH_num)
class(data$FAH_num)

############convert characters to factors####################

data$Precip_Code <- factor(data$Precip_Code, levels = c("0 - None", "2 - Trace", "4 - Light Showers", "6 - Snow Showers", "8 - Snow" , "10 - Heavy Snow"))


is.factor(data$Precip_Code)


data$Precip_Code1 = case_when(
  data$Precip_Code == "0 - None" ~ 1,
  data$Precip_Code == "2 - Trace" ~ 2,
  data$Precip_Code == "4 - Light Showers" ~ 3,
  data$Precip_Code == "6 - Snow Showers" ~ 4,
  data$Precip_Code == "8 - Snow" ~ 5,
  data$Precip_Code == "10 - Heavy Snow" ~ 6,
)

table(data$Precip_Code1)
class(data$Precip_Code1)

#==============================Checking the relationship between OAH and FAH======================================================############
# cross-table to check how often forecasts match observed levels
table(data$FAH, data$OAH)
# "correct" forecasts are along the diagonal
sum(diag(table(data$FAH, data$OAH)))
# total number of observations
sum(table(data$FAH, data$OAH))
# Alternatively total number of observations
nrow(data)
# proportion of correct forecasts
sum(diag(table(data$FAH, data$OAH))) / nrow(data)
#Alternatively proportion of correct forecasts
sum(diag(table(data$FAH, data$OAH))) /sum(table(data$FAH, data$OAH))


(sum(table(data$FAH, data$OAH)[row(table(data$FAH, data$OAH)) == col(table(data$FAH, data$OAH))-1])+
    sum(table(data$FAH, data$OAH)[row(table(data$FAH, data$OAH)) == col(table(data$FAH, data$OAH))+1])+
    sum(diag(table(data$FAH, data$OAH))))/sum(table(data$FAH, data$OAH))

(sum(table(data$FAH, data$OAH)[row(table(data$FAH, data$OAH)) == col(table(data$FAH, data$OAH))+1])
  +sum(diag(table(data$FAH, data$OAH))))/sum(table(data$FAH, data$OAH))


#COMMENT: The forecasting is aproxiamately 67% accurate. However we can allow one step ahead and below and check the accuracy



#===================== Checking the relationship between air temp and forecasts==============================
data %>% group_by(FAH) %>% summarize(meanAT = mean(Air_Temp, na.rm = T),
                                     n = n())

###COMMENT:Forecasted avalanche hazard is high when it is when it is cold and vice versa. The most recorded level is the middle one

# Checking the relationship between wind speed and forecasts?
data %>% group_by(FAH) %>% summarize(meanWS = mean(Wind_Speed, na.rm = T),
                                     n = n())


###COMMENT: FAH is high when the wind speed is high 

# Checking the relationship between wind direction and forecasts?
data %>% group_by(FAH) %>% summarize(meanWD = mean(Wind_Dir, na.rm = T),
                                     n = n())

###COMMENT: The forecasts risk levels increases as the wind is mainly in the South(South ease and south west)

data %>% group_by(FAH) %>% summarize(meanCloud = mean(Cloud, na.rm = T),
                                     n = n())

data %>% group_by(FAH) %>% summarize(meanRain = mean(Rain_at_900, na.rm = T),
                                     n = n())

data %>% group_by(FAH) %>% summarize(meanPrecipitation = mean(Precip_Code1, na.rm = T),
                                     n = n())


data %>% group_by(FAH) %>% summarize(meanTime = mean(days_since_19Nov, na.rm = T),
                                     n = n())

data %>% group_by(FAH) %>% summarize(meand2 = mean(d2centre, na.rm = T),
                                     n = n())

#===============categorical
table(data$Precip_Code1)/1780

table(data$Rain_at_900)/1780

#=================================================
#data <- data[!(format(data$Date, format = "%m") =="02" &
#                    format(data$Date, format = "%d")=="29"),]


#data[!(month(data$Date)==2 & day(data$Date)==29),]

#data <- data[!(format(data$Date,"%m") == "02" & format(data$Date, "%d") == "29"), , drop = FALSE]
#==============================Data Splitting===========================================================================
dim(data)[1]

0.7*dim(data)[1]

0.3*dim(data)[1]

data_test = data[1:534, ]

data_train = data[535:1780, ]


#==================================Models======================================================
model1 <- gam(OAH_num ~  s(days_since_19Nov,bs="cc")+s(Wind_Speed)+s(Air_Temp)+s(X,Y),  data=data_train,family=ocat(R=5))
model2 <- gam(OAH_num ~  s(Air_Temp)+te(Wind_Speed,days_since_19Nov,bs=c("tp", "cc"))+s(X,Y),  data=data_train,family=ocat(R=5))
model3 <- gam(OAH_num ~  s(days_since_19Nov,bs="cc")+te(Wind_Speed,Air_Temp)+s(X,Y),  data=data_train,family=ocat(R=5))
model4 <- gam(OAH_num ~  s(Air_Temp)+te(Wind_Speed,days_since_19Nov,bs=c("tp", "cc"))+s(X,Y),  data=data_train,family=ocat(R=5))
model5 <- gam(OAH_num ~  s(days_since_19Nov,bs="cc")+te(Wind_Speed,Air_Temp)+s(X,Y),  data=data_train,family=ocat(R=5))
model6 <- gam(OAH_num ~  s(Wind_Speed)+te(Air_Temp,days_since_19Nov,bs=c("tp", "cc"))+s(X,Y),  data=data_train,family=ocat(R=5))
model9 <- gam(OAH_num ~  s(days_since_19Nov,bs="cc")+te(Wind_Speed,Air_Temp)+s(X,Y)+te(Wind_Dir,Cloud,bs=c("cc", "tp")),  data=data_train,family=ocat(R=5))
model8 <- gam(OAH_num ~  s(Wind_Speed,sp=0.01,k=12)+te(Air_Temp,days_since_19Nov,bs=c("tp", "cc"))+Precip_Code1,  data=data_train,family=ocat(R=5))
model7 <- gam(OAH_num ~ s(Air_Temp) + s(X,Y) + s(days_since_19Nov,bs="cc"), data=data,family=ocat(R=5))


AIC(model1,model2,model3,model4,model5,model6,model7,model8,model9)



par(mfrow=c(2,2))
summary(model8)
gam.check(model8)
#par(mfrow=c(1,1))
concurvity(model8, full = TRUE)

#=======================================================
test_prediction = predict.gam(model8, newdata = data_test, type = "response")
test_prediction=as.data.frame(test_prediction)
names(test_prediction)=c("Low","Moderate","Considerable-","Considerable+","High")
data_test$prediction <-max.col(as.matrix(test_prediction))
#View(test_prediction)

#==================================================================
# cross-table to check how often prediction match observed levels
table(data_test$prediction, data_test$OAH_num)
# "correct" forecasts are along the diagonal
sum(diag(table(data_test$prediction, data_test$OAH_num)))
# total number of observations
sum(table(data_test$prediction, data_test$OAH_num))
# Alternatively total number of observations
nrow(data_test)
# proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$OAH_num))) / nrow(data_test)
#Alternatively proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$OAH_num))) /sum(table(data_test$prediction, data_test$OAH_num))


(sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))-1])+
    sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))+1])+
    sum(diag(table(data_test$prediction, data_test$OAH_num))))/sum(table(data_test$prediction, data_test$OAH_num))

#==================================================================================================
# cross-table to check how often prediction match observed levels
table(data_test$prediction, data_test$OAH_num)
# "correct" forecasts are along the diagonal
sum(diag(table(data_test$prediction, data_test$OAH_num)))
# total number of observations
sum(table(data_test$prediction, data_test$OAH_num))
# Alternatively total number of observations
nrow(data_test)
# proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$OAH_num))) / nrow(data_test)
#Alternatively proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$OAH_num))) /sum(table(data_test$prediction, data_test$OAH_num))


(sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))-1])+
    sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))+1])+
    sum(diag(table(data_test$prediction, data_test$OAH_num))))/sum(table(data_test$prediction, data_test$OAH_num))


(sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))-1])+
    +sum(diag(table(data_test$prediction, data_test$OAH_num))))/sum(table(data_test$prediction, data_test$OAH_num))

(sum(table(data_test$prediction, data_test$OAH_num)[row(table(data_test$prediction, data_test$OAH_num)) == col(table(data_test$prediction, data_test$OAH_num))+1])+
    +sum(diag(table(data_test$prediction, data_test$OAH_num))))/sum(table(data_test$prediction, data_test$OAH_num))



#======================================================================
# cross-table to check how often prediction match observed levels
table(data_test$prediction, data_test$FAH_num)
# "correct" forecasts are along the diagonal
sum(diag(table(data_test$prediction, data_test$FAH_num)))
# total number of observations
sum(table(data_test$prediction, data_test$FAH_num))
# Alternatively total number of observations
nrow(data_test)
# proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$FAH_num))) / nrow(data_test)
#Alternatively proportion of correct forecasts
sum(diag(table(data_test$prediction, data_test$FAH_num))) /sum(table(data_test$prediction, data_test$FAH_num))


(sum(table(data_test$prediction, data_test$FAH_num)[row(table(data_test$prediction, data_test$FAH_num)) == col(table(data_test$prediction, data_test$FAH_num))-1])+
    sum(table(data_test$prediction, data_test$FAH_num)[row(table(data_test$prediction, data_test$FAH_num)) == col(table(data_test$prediction, data_test$FAH_num))+1])+
    sum(diag(table(data_test$prediction, data_test$FAH_num))))/sum(table(data_test$prediction, data_test$FAH_num))


(sum(table(data_test$prediction, data_test$FAH_num)[row(table(data_test$prediction, data_test$FAH_num)) == col(table(data_test$prediction, data_test$FAH_num))-1])+
    +sum(diag(table(data_test$prediction, data_test$FAH_num))))/sum(table(data_test$prediction, data_test$FAH_num))

(sum(table(data_test$prediction, data_test$FAH_num)[row(table(data_test$prediction, data_test$FAH_num)) == col(table(data_test$prediction, data_test$FAH_num))+1])+
    +sum(diag(table(data_test$prediction, data_test$FAH_num))))/sum(table(data_test$prediction, data_test$FAH_num))



#==============================================
# Predict ~~~plot of effect of wind speed


dataM <- data_test %>% filter(!is.na(OAH_num), !is.na(X),!is.na(Y),!is.na(Air_Temp),
                              !is.na(Wind_Dir),!is.na(Wind_Speed),!is.na(Precip_Code1),
                              !is.na(Cloud),!is.na(Rain_at_900),!is.na(days_since_19Nov))

mycols <- levels(dataM$OAH)
newdata1 <- data.frame(Wind_Speed = seq(from = min(dataM$Wind_Speed), to = max(dataM$Wind_Speed), length.out = 500),
                       Air_Temp = mean(dataM$Air_Temp),Precip_Code1 = mean(dataM$Precip_Code1),days_since_19Nov = mean(dataM$days_since_19Nov))


ocat_model <- model8
ocat_predict = predict(ocat_model, newdata = newdata1, type = "response")
ocat_predict = as.data.frame(ocat_predict)
colnames(ocat_predict) <- mycols
ocat_predict <- ocat_predict %>%
  mutate(x= newdata1$Wind_Speed) %>%
  pivot_longer(1:5, names_to = "pred_level", values_to = "obs_val") 
ocat_predict$pred_level <- factor(ocat_predict$pred_level, levels = mycols)

ggplot(aes(x= x, y= obs_val),data=ocat_predict)+
  facet_grid(.~pred_level)+
  geom_line()+
  xlab("Wind Speed") + ylab("Probability") + 
  theme_dark() 

#======================================

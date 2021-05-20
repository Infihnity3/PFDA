#Chow Zi Xuan
#TP051264

#Assigning variable to CSV File called weather

weather = read.csv(file = "C:/Users/Owner/Desktop/Degree Year 2/PFDA/Assignment/weather.csv", header = TRUE, na.strings = "")
weather_data = c("Min Temperature", "Max Temperature", "Rainfall", "Evaporation", "Sunshine", "Wind Gust Direction", "Wind Gust Speed",
                 "Wind Direction 9am", "Wind Direction 3pm", "Wind Speed 9am", "Wind Speed 3pm", "Humidity 9am", "Humidity 3pm",
                 "Pressure 9am", "Pressure 3pm", "Cloud 9am", "Cloud 3pm", "Temperature 9am", "Temperature 3pm", "Rains Today", "Risk MM",
                 "Rains Tomorrow")

library(dplyr)
library(ggplot2)
View(weather)
#***********************************************************************************************************************
#Question 1: Does the Rainfall affect the weather?
#1.1: Evaporation to Rainfall (Does not Affect)

ggplot(weather, aes(x = Evaporation, y = Rainfall)) + 
  geom_line(aes(color = Rainfall)) +
  ggtitle("Evaporation VS Rainfall")

#1.2: Rainfall to Sunshine (Does not Affect)

ggplot(weather, aes(x = Rainfall, y = Sunshine)) + 
  geom_point(aes(color = Sunshine)) + 
  ggtitle("Sunshine VS Rainfall") +
  facet_wrap(~Rainfall)

#1.3: Wind speed 3am vs 9am to R (Does Affect)

Rainfall = as.data.frame(weather$Rainfall)
Wind9 = as.data.frame(weather$WindSpeed9am)
Wind3 = as.data.frame(weather$WindSpeed3pm)
df1 = data.frame(Rainfall, Wind9, Wind3)
names(df1) = c("Rainfall", "Wind9", "Wind3")


ggplot(data = df1, aes(x = Rainfall,  y = Wind_Speed)) + 
  geom_line(aes(x = Rainfall, y = Wind9), color = "blue") +
  geom_line(aes(x = Rainfall, y = Wind3), color = "red") +
  ggtitle("Wind Speed 9am and Wind Speed 3pm VS Rainfall")

#1.4: Humidity to Rainfall (Does Affects)

Rainfall = as.data.frame(weather$Rainfall)
Humid9 = as.data.frame(weather$Humidity9am)
Humid3 = as.data.frame(weather$Humidity3pm)
df2 = data.frame(Rainfall, Humid9, Humid3)
names(df2) = c("Rainfall", "Humidity_9am", "Humidity_3pm")

ggplot(data = df2, aes(x = Rainfall,  y = Humidity)) + 
  geom_line(aes(x = Rainfall, y = Humidity_9am), color = "Pink") +
  geom_line(aes(x = Rainfall, y = Humidity_3pm), color = "Yellow") +
  ggtitle("Humidity 9am and Humidity 3pm VS Rainfall")

#1.5: Temperature 9am/3pm to Rainfall (Does not)

Rainfall = as.data.frame(weather$Rainfall)
Temp9 = as.data.frame(weather$Temp9am)
Temp3 = as.data.frame(weather$Temp3pm)
df2 = data.frame(Rainfall, Temp9, Temp3)
names(df2) = c("Rainfall", "Temperature9am", "Temperature3pm")

ggplot(data = df2, aes(x = Rainfall,  y = Temperature)) + 
  geom_line(aes(x = Rainfall, y = Temperature9am), color = "blue") +
  geom_line(aes(x = Rainfall, y = Temperature3pm), color = "red") +
  ggtitle("Temperature 9am and Temperature 3pm VS Rainfall")

#1.6: Cloud 9am/3pm to Rainfall

RainF = as.data.frame(weather$Rainfall)
Cloud9 = as.data.frame(weather$Cloud9am)
Cloud3 = as.data.frame(weather$Cloud3pm)
df2 = data.frame(WGS, Temp9, Temp3)
names(df2) = c("RainF", "Cloud9", "Cloud3")

ggplot(data = df2, aes(x = Cloud,  y = Rainfall)) + 
  geom_point(aes(y = RainF, x = Cloud9), color = "blue") +
  geom_point(aes(y = RainF, x = Cloud3), color = "red") +
  ggtitle("Cloud 9am and Cloud 3pm VS Rainfall")


#**********************************************************************************************************************
#Question 2: The wind Gust direction and Speed affects the Rain today?
#2.1: Rain today

ggplot(weather, aes(x = RainToday)) + 
  geom_bar(aes(fill = RainToday)) +
  ggtitle("Rain Today")

#2.2 Wind Gust direction

ggplot(weather, aes(x = WindGustDir)) + 
  geom_bar(aes(fill = WindGustDir)) +
  ggtitle("Wind Gust Directions")

#2.3 Wind Gust Speed to Wind Gust Direction

ggplot(weather, aes(x = WindGustSpeed, y = WindGustDir)) + 
  geom_point(aes(color = WindGustSpeed)) + 
  ggtitle("Wind Gust Speed vs Wind Gust Direction")

#2.4 Wind Gust Direction to Rain Today (Does not Matter)

ggplot(weather, aes(x = WindGustDir, y = RainToday)) +
  geom_point()

#2.5 Wind Gust Speed to Rain Today

ggplot(weather, aes(y = WindGustSpeed, x = RainToday)) + 
  geom_point(aes(color = Rainfall)) +
  ggtitle("Wind Gust Speed VS Rain Today")

#*************************************************************************************************************************
#Question 3: Does Temperature Affect the Weather?
#3.1: Temperature to Wind Gust Speed (Higher, higher)

WGS = as.data.frame(weather$WindGustSpeed)
Temp9 = as.data.frame(weather$Temp9am)
Temp3 = as.data.frame(weather$Temp3pm)
df2 = data.frame(WGS, Temp9, Temp3)
names(df2) = c("WindGustSpeed", "Temperature9am", "Temperature3pm")

ggplot(data = df2, aes(x = Temperature,  y = Wind_Gust_Speed)) + 
  geom_point(aes(y = WindGustSpeed, x = Temperature9am), color = "blue") +
  geom_point(aes(y = WindGustSpeed, x = Temperature3pm), color = "red") +
  ggtitle("Temperature 9am and Temperature 3pm VS Wind Gust Speed")

#3.2: Temperature 9am to evaporation

Eveporation = as.data.frame(weather$Evaporation)
Temp9 = as.data.frame(weather$Temp9am)
df2 = data.frame(Evaporation, Temp9, Temp3)
names(df2) = c("Evaporation", "Temperature9am", "Temperature3pm")

ggplot(data = df2, aes(x = Evaporation,  y = Temperature)) + 
  geom_point(aes(y = Temperature9am), color = "blue") +
  ggtitle("Temperature 9am VS Evaporation")

#3.3: Temperature 3pm to Evaporation

ggplot(weather, aes(x = Evaporation, y = Temp3pm)) + 
  geom_smooth(se = F) +
  ggtitle("Temperature 3pm vs Evaporation")

#3.4: Temperature 3pm to Sunshine

ggplot(weather, aes(y = Temp3pm, X = Sunshine, fill = Sunshine)) +
  geom_boxplot() + 
  ggtitle("Temperature 3pm VS Sunshine")

#3.5: Temperature 9am to Sunshine

ggplot(weather, aes(x = Sunshine, y = Temp3pm)) + 
  geom_point(aes(color = Temp3pm)) +
  ggtitle("Temperature 3pm vs Sunshine")

#3.6: Wind Speed 9am to Temperature 9 am

ggplot(weather, aes(x = WindSpeed9am, y = Temp9am)) + 
  geom_point(aes(color = WindSpeed9am)) +
  ggtitle("Wind Speed 9am VS Temperature 9am")

#3.7: Wind Speed 3pm to Temperature 3pm

ggplot(weather, aes(x = WindSpeed3pm, y = Temp3pm)) + 
  geom_point(aes(color = WindSpeed3pm)) +
  ggtitle("Wind Speed 3pm VS Temperature 3pm")

#3.8: Humidity 9am to Temperature 9am

ggplot(weather, aes(x = Humidity9am, y = Temp3pm)) + 
  geom_point(aes(color = Humidity9am)) +
  ggtitle("Humidity 9am VS Temperature 3pm")

#3.9: Humidity 3pm to Temperature 3pm

ggplot(weather, aes(x = Humidity3pm, y = Temp3pm)) + 
  geom_point(aes(color = Humidity3pm)) +
  ggtitle("Humidity 3pm VS Temperature 3pm")

#3.10: Pressure 9am to Temperature 9am

ggplot(weather, aes(x = Pressure9am, y = Temp9am)) + 
  geom_line(aes(color = Pressure9am)) +
  ggtitle("Pressure 9am VS Temperature 9am")

#3.11: Pressure 3pm to Temperature 3pm

ggplot(weather, aes(x = Pressure3pm, y = Temp3pm)) + 
  geom_line(aes(color = Pressure3pm)) +
  ggtitle("Pressure 3pm VS Temperature 3pm")

#3.12: Cloud 9am to Temperature 9am

ggplot(weather, aes(x = Cloud9am, y = Temp9am)) + 
  geom_point(aes(color = Cloud9am)) +
  ggtitle("Cloud 9am VS Temperature 9am")

#3.13: Cloud 3pm to Temperature 3pm

ggplot(weather, aes(x = Cloud3pm, y = Temp3pm)) + 
  geom_point(aes(color = Cloud3pm)) +
  ggtitle("Cloud 3pm VS Temperature 3pm")

#**************************************************************************************************
#Question 4: Does the weather affects Raining Tomorrow
#4.1: Wind Speed to Rain tomorrow

ggplot(weather, aes(y = WindSpeed9am, x = RainTomorrow)) + 
  geom_point(aes(color = WindSpeed9am)) +
  ggtitle("Wind Speed 9am VS Rain Tomorrow") 


#4.2: Rain tomorrow and Rain Today

ggplot(weather, aes(x = RainToday)) + 
  geom_bar(aes(fill = RainToday)) +
  ggtitle("Rain Today")

ggplot(weather, aes(x = RainTomorrow)) + 
  geom_bar(aes(fill = RainTomorrow)) +
  ggtitle("Rain Tomorrow") 

#4.3: Wind gust speed to rain tomorrow

ggplot(weather, aes(x = WindGustSpeed, y = RainTomorrow)) + 
  geom_point() +
  ggtitle("Wind Gust Speed to Rain Tomorrow") 

#4.4: wind gust direction to rain tomorrow

ggplot(weather, aes(x = WindGustDir, y = RainTomorrow)) +
  geom_point()

#4.5: Cloud 9am and 3pm to rain tomorrow

RainT = as.data.frame(weather$RainTomorrow)
Cloud9 = as.data.frame(weather$Cloud9am)
Cloud3 = as.data.frame(weather$Cloud3pm)
df2 = data.frame(RainT, Cloud9, Cloud3)
names(df2) = c("RainT", "Cloud9", "Cloud3")

ggplot(data = df2, aes(x = Rain_Tomorrow,  y = Cloud)) + 
  geom_point(aes(y = Cloud9, x = RainT), color = "blue") +
  geom_point(aes(y = Cloud3, x = RainT), color = "red") +
  ggtitle("Cloud 9am and Cloud 3pm VS RainTomorrow")

a = count(weather, weather$Cloud3pm)
ggplot(weather, aes(x = RainTomorrow, y = Cloud9am)) + 
  geom_point(aes(color = Cloud9am)) +
  ggtitle("Cloud 3pm VS Temperature 3pm") +
  facet_wrap(~Cloud9am)
#4.6: evaporation to rain tomorrow

ggplot(weather, D)


#***********************************************************************************
#Question 5: When is the best time to travel
#5.1: What temperature range do 80% of the days fall between? 


#5.2: Find the Dry Season



#5.3: Find Summer (highest)



#5.4: Find Winter (lowest)



#5.5: Find Autumn 



#5.6: Find Spring

#WHen is the best time to play badminton?
#5.7: Wind speed > 10 (cannot play badminton) outdoors

#5.8: wind speed < 10 (cant play badminton) outdoors

#5.9: Temperature > 35 (Cant not play badminton) outdoors

#5.10: Temperature  <= 35 Can play outdoors
#**************************************************************
#Extra Feature
#1) Mean maximum temperature


#2) Mean Minimum Temperature


#3) Assigning New Col for Mean Temperature


#4) Assigning New Col for Average Wind speed


#5) Assigning New Col for Average Humidity


#6) Assigning New Col for Average Pressure






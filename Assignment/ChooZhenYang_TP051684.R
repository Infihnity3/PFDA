data <- read.csv("C:/Users/Owner/Desktop/Degree Year 2/PFDA/Assignment/Hourlyweatherdata.csv")
View(data)
library(dplyr)
as.POSIXct(data$time_hour)
data$time_hour<-strptime(data$time_hour, "%d/%m/%Y %H:%M")
library(weathermetrics)
data$temp<-fahrenheit.to.celsius(data$temp)
data$dewp<-fahrenheit.to.celsius(data$dewp)
library(ggplot2)
library(measurements)
data$precip<-conv_unit(data$precip,"inch","cm")
data$visib<-conv_unit(data$visib,"mile","m")
table(data$wind_dir == 0)
avgP = mean(data$pressure, na.rm=TRUE)
data$pressure<- ifelse(is.na(data$pressure), avgP, data$pressure)
data$wind_gust<- ifelse(is.na(data$wind_gust), 0, data$wind_gust)
data$wind_dir<- ifelse(is.na(data$wind_dir), 0, data$wind_dir)
data$wind_speed<- ifelse(is.na(data$wind_speed), 0, data$wind_speed)
#1MeanTempEachMonth
df<-data.frame(origin=c(data$origin),month=c(data$month), temp=c(data$temp))
df<-df%>%group_by(origin,month)%>%summarise(meanTemp=mean(temp))
#RECTs sequence 0 - 9 dotted line)
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p1<-ggplot() +
  geom_rect(data = rects, aes(xmin = xstart, 
                              xmax = xend, ymin = 0, 
                              ymax = 30, fill = season), 
            alpha = 0.4) +
  geom_line(df, mapping = aes(x=month,y=meanTemp,col=origin))+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(0,30,by=5))+
  geom_vline(xintercept=3, linetype="dashed")+
  geom_vline(xintercept=6,linetype="dashed")+
  geom_vline(xintercept=9,linetype="dashed")+
  geom_vline(xintercept=12,linetype="dashed")+
  ggtitle("Mean Temperature of Each Month")+
  geom_text(aes(x=12,y=16),label="winter",angle=90,vjust=0.8)+
  scale_color_manual(values = c("royalblue", "tomato1"))
p1
#2 MeanPrecipitationOverMonth
df<-data.frame(origin=c(data$origin),month=c(data$month), precip=c(data$precip))
df<-df%>%group_by(origin,month)%>%summarise(meanPrecip=mean(precip))
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p2<-ggplot() +  
geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                            ymin = 0, ymax = 0.06, 
                            fill = season), alpha = 0.4) +
  geom_area(df, mapping = aes(y=meanPrecip, x=month,col=origin))+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  geom_vline(xintercept=3, linetype="dashed")+
  geom_vline(xintercept=6,linetype="dashed")+
  geom_vline(xintercept=9,linetype="dashed")+
  geom_vline(xintercept=12,linetype="dashed")+
  ggtitle("Mean Precipitation of Each Month")+
  geom_text(aes(x=12,y=0.03),label="winter",angle=90,vjust=0.8)
p2
#3MinTemperatureAccordingToMonth
df<-data.frame(origin=c(data$origin),month=c(data$month), temp=c(data$temp))
df<-df%>%group_by(origin,month)%>%summarise(minTemp=min(temp))
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p3<-ggplot() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = -15, ymax = 30, 
                              fill = season), alpha = 0.4) +
  geom_line(df, mapping = aes(x=month,y=minTemp,col=origin),size=2)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(-15,30,by=5))+
  geom_vline(xintercept=3, linetype="dashed")+
  geom_vline(xintercept=6,linetype="dashed")+
  geom_vline(xintercept=9,linetype="dashed")+
  geom_vline(xintercept=12,linetype="dashed")+
  ggtitle("Minimum Temperature of Each Month")+
  geom_text(aes(x=12,y=10),label="winter",angle=90,vjust=0.8)+
  geom_hline(yintercept=-1, color="orangered")+
  scale_color_manual(values = c("royalblue", "gold"))
p3
#4MinVisibilityOverMonth
df<-data.frame(origin=c(data$origin),month=c(data$month), visib=c(data$visib))
df<-df%>%group_by(origin,month)%>%summarise(visib=min(visib))
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p4<-ggplot() +
  geom_rect(data = rects, aes(xmin = xstart, xmax = xend, 
                              ymin = 0, ymax = 6000, 
                              fill = season), alpha = 0.4) +
  geom_line(df, mapping = aes(x=month,y=visib,col=origin),size=2)+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(0,6000,by=1000))+
  geom_vline(xintercept=3, linetype="dashed")+
  geom_vline(xintercept=6,linetype="dashed")+
  geom_vline(xintercept=9,linetype="dashed")+
  geom_vline(xintercept=12,linetype="dashed")+
  geom_hline(yintercept=201,color="orangered")+
  ggtitle("Minimum Visibility of Each Month")+
  ylab("Minimum Visibility")+
  geom_text(aes(x=12,y=3000),label="winter",angle=90,vjust=0.8)+
  scale_color_manual(values = c("royalblue", "gold"))
p4
#5Frequency of Visibility lower than 201
df<-data.frame(origin=c(data$origin),month=c(data$month), visib=c(data$visib))
df<-df%>%group_by(origin,month)%>%summarise(visib=sum(visib<=201))
p5<-ggplot(df, aes(x=month,y=visib)) + 
  geom_histogram(stat="identity",fill="indianred1",col="lavenderblush")+
  ylab("Frequency of Visibility Lower\n than Takeoff Value")+
  facet_grid(~origin)+
  ggtitle("Frequency of Minimum Visibility \n for Take Off Occur Each Month")+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(1,10,by=1))+
  geom_text(aes(label=visib),vjust=0)+
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=8))
p5
#6WindDirectionOverMonth
df<-data.frame(origin=c(data$origin),day=c(data$day),month=c(data$month), 
               winddir=c(data$wind_dir))
df<-df%>%group_by(origin,day,month)%>%summarise(winddir=mean(winddir))
pdf("rpWindDir.pdf")
month<-unique(df$month)

monthName<-c("January","February","March","April","May"
             ,"June","July","August","September","October"
             ,"November","December")
p6<-for(i in seq_along(month)){
  print(ggplot(subset(df,df$month==month[i]),aes(x=day,y=winddir,col=origin)) + 
          geom_line()+
          geom_point()+
          ylab("Mean Wind Direction")+
          ggtitle(paste("Mean Wind Direction of Each Day on",monthName[i]))+
          scale_x_continuous(breaks=seq(1,31,by=1))+
          scale_y_continuous(breaks=seq(0,360,by=60))+
          theme_bw())
}
p6
dev.off()
#7FrequencyOfWindSpeed>34mph
df<-data.frame(speed=c(data$wind_speed),month=c(data$month))
df<-df%>%group_by(month)%>%summarise(speed=sum(speed>=34))
p7<-ggplot(df,aes(x=month,y=speed)) + 
  geom_bar(stat = 'identity',fill=("antiquewhite"))+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(1,10,by=1))+
  ylab("Frequency of Wind Speed \nGreater than 34mph")+
  ggtitle("Frequency of Wind Speed Greater than \n 34mph Based on Month")+
  geom_text(aes(label=speed))+
  theme_bw()
p7
#12HumidityDifferenceBetweenAirport
df<-data.frame(humid=c(data$humid),origin=c(data$origin))
df<-df%>%group_by(origin)%>%summarise(humid)
p8<-ggplot() +
  geom_boxplot(df, mapping = aes(x=origin,y=humid))+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  ylab("Relative Humidity")+
  ggtitle("Relative Humidity Based on Origin")+
  theme_bw()
p8
#9FrequencyOfPrecipitationNotEqual0
df<-data.frame(precip=c(data$precip),month=c(data$month),origin=c(data$origin))
df<-df%>%group_by(origin,month)%>%summarise(precip=sum(precip!=0))
p9<-ggplot(df, aes(x=month,y=precip)) + 
  geom_histogram(stat="identity",fill="indianred1",col="lavenderblush")+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  ggtitle("Frequency of Presence of Precipitation\n in Each Month")+
  ylab("Frequency of \n Presence of Precipitation")+
  geom_text(aes(label=precip))+
  facet_grid(~origin)
p9
#10FrequencyOfWindGustEachMonth
df<-data.frame(gust=c(data$wind_gust),month=c(data$month),origin=c(data$origin))
df<-df%>%group_by(origin,month)%>%summarise(gust=sum(gust!=0))
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p10<-ggplot(df, aes(x=month,y=gust)) + 
  geom_histogram(stat="identity",fill="indianred1",col="lavenderblush")+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  scale_y_continuous(breaks=seq(0,300,by=50))+
  ggtitle("Frequency of Occurence of Wind Gust")+
  ylab("Frequency")+
  geom_text(aes(label=gust))+
  facet_grid(~origin)
p10
#11DewpointTemp/TempRelationVsHumid
Tdpt<-(data$temp-data$dewp)
p11<-ggplot(data, aes(y=humid, x=Tdpt)) + 
  geom_smooth(se=F)+
  ggtitle("Relationship between Difference in
  Temperature and Dewpoint Temperature 
  and Humidity")+
  xlab("Difference between Temperature \nand Dewpoint Temperature")+
  ylab("Humidity")+
  scale_x_continuous(breaks=seq(0,30,by=5))+
  scale_y_continuous(breaks=seq(0,100,by=10))+
  theme_classic()
p11
#12MaxWindGustOfEachMonth
df<-data.frame(gust=c(data$wind_gust),month=c(data$month))
df<-df%>%group_by(month)%>%summarise(gust=max(gust))
p12<-ggplot(df,aes(x=month,y=gust)) + 
  geom_line(colour='deepskyblue')+
  geom_point(colour='deepskyblue')+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  scale_y_continuous(breaks=seq(40,60,by=5))+
  ggtitle("Max Wind Gust of Each Month")+
  ylab("Wind Gust")+
  xlab("Month")+
  theme_bw()
p12
#13WindSpeedVsWindGust
df<-data.frame(speed=c(data$wind_speed),gust=c(data$wind_gust),month=c(data$month))
df<-df%>%group_by(month)%>%summarise(speed=mean(speed),gust=mean(gust))
p13<-ggplot(df,aes(month)) + 
        geom_line(aes(y=speed),color="darkred")+
        geom_line(aes(y=gust),color="darkblue")+
        geom_point(aes(y=speed),color="darkred")+
        geom_point(aes(y=gust),color="darkblue")+
        scale_x_continuous(breaks=seq(1,31,by=1))+
        scale_y_continuous(breaks=seq(1,14,by=1))+
        ggtitle("Relationship between \nWind Speed and Wind Gust")+
        ylab("Miles per hour")+
        theme_bw()
p13
#14FrostOccurrence
df<-data.frame(month=c(data$month), dewp=c(data$dewp), temp=c(data$temp))
df<-df%>%group_by(month)%>%summarise(condition=sum((dewp>temp)&sum(temp<0)))
p14<-ggplot(df,aes(x=month,y=condition)) + 
  geom_histogram(stat='identity')+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  geom_text(aes(label=condition))+
  ggtitle("Frequency of Frost Occur Each Month")+
  ylab("Frequency")+
  xlab("Month")+
  theme_bw()
p14
#15MeanHumidityofEachMonth
df<-data.frame(month=c(data$month), humid=c(data$humid))
df<-df%>%group_by(month)%>%summarise(humid=mean(humid))
p15<-ggplot(df,aes(x=month,y=humid)) + 
  geom_line(color='darkslategray1')+
  scale_x_continuous(breaks=seq(0,12,by=1))+
  geom_point(color='darkturquoise')+
  ggtitle("Mean Humidity of Each Month")+
  ylab("Humidity")+
  xlab("Month")+
  theme_dark()
p15
#16MaxTemperatureofEachMonth
df<-data.frame(origin=c(data$origin),month=c(data$month), temp=c(data$temp))
df<-df%>%group_by(origin,month)%>%summarise(Temp=max(temp))
rects <- data.frame(xstart = seq(0,9,3), xend = seq(3,12,3), 
                    season = c('winter','spring','summer','autumn'))
p16<-ggplot() +
  geom_rect(data = rects, aes(xmin = xstart, 
                              xmax = xend, ymin = 0, 
                              ymax = 40, fill = season), 
                              alpha = 0.4) +
  geom_line(df, mapping = aes(x=month,y=Temp,col=origin))+
  scale_x_continuous(breaks=seq(1,12,by=1))+
  scale_y_continuous(breaks=seq(0,45,by=5))+
  geom_vline(xintercept=3, linetype="dashed")+
  geom_vline(xintercept=6,linetype="dashed")+
  geom_vline(xintercept=9,linetype="dashed")+
  geom_vline(xintercept=12,linetype="dashed")+
  geom_hline(yintercept=40, color='orangered')+
  ggtitle("Max Temperature of Each Month")+
  ylab("Temperature")+
  geom_text(aes(x=12,y=16),label="winter",angle=90,vjust=0.8)+
  scale_color_manual(values = c("royalblue", "tomato1"))
p16
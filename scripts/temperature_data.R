library(ggplot2)
library(reshape2)
library(imputeTS)

#beginning and ending time

timeBegin <- as.numeric(strptime("01.01.2016 00:00:00", format = "%d.%m.%Y %H:%M:%S",tz="Etc/GMT"))

timeEnd <- as.numeric(strptime("31.12.2016 23:55:00", format = "%d.%m.%Y %H:%M:%S",tz="Etc/GMT"))

#daily data
#----

#time

timeDaily <- as.POSIXlt(seq(timeBegin, timeEnd, 86400), origin="1970-01-01", tz="Etc/GMT")

tempDaily <- data.frame(timeDaily)

#importing and arranging data

tempX <- read.table(file.choose(), header=F)
tempX <- tempX$V9
tempX[tempX==-9999] <- NA
tempX <- na.kalman(tempX)

tempDaily <- cbind(tempDaily, tempX)

colnames(tempDaily) <- c("time", "temp1", "temp2", "temp3", "temp4", "temp5", "temp6", "temp7")

tempDailyMelt <- melt(subset(tempDaily, select=c(time, temp1, temp2, temp3, temp4, temp5, temp6, temp7)), id.var="time")

#plots

ggplot(tempDailyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 1, aes(color=variable)) +
  facet_grid(variable ~ .) +
  theme(legend.position="none") +
  labs(x="time (month)", expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

ggplot(tempDailyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

#hourly data
#----

#time

timeHourly <- as.POSIXlt(seq(timeBegin, timeEnd, 3600), origin="1970-01-01", tz="Etc/GMT")

tempHourly <- data.frame(timeHourly)

#importing and arranging data

tempX <- read.table(file.choose(), header=F)
tempX <- tempX$V9
tempX[tempX==-9999] <- NA
tempX <- na.kalman(tempX)

tempHourly <- cbind(tempHourly, tempX)

colnames(tempHourly) <- c("time", "temp1", "temp2", "temp3", "temp4", "temp5", "temp6", "temp7")

tempHourlyMelt <- melt(subset(tempHourly, select=c(time, temp1, temp2, temp3, temp4, temp5, temp6, temp7)), id.var="time")

#plots

ggplot(tempHourlyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 1, aes(color=variable)) +
  facet_grid(variable ~ .) +
  theme(legend.position="none") +
  labs(x="time (month)", expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels = "%m", date_breaks = "1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

ggplot(tempHourlyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

#subhourly data
#----

#time

timeSubhourly <- as.POSIXlt(seq(timeBegin,timeEnd,300), origin="1970-01-01", tz="Etc/GMT")

tempSubhourly <- data.frame(timeSubhourly)

#importing and arranging data

tempX <- read.table(file.choose(), header=F)
tempX <- tempX$V9
tempX[tempX==-9999] <- NA
tempX <- na.kalman(tempX)

tempSubhourly <- cbind(tempSubhourly, tempX)

toDelete <- seq(0, nrow(tempSubhourly), 2)

tempSubhourly <-  tempSubhourly[-toDelete, ]

colnames(tempSubhourly) <- c("time", "temp1", "temp2", "temp3", "temp4", "temp5", "temp6", "temp7")

tempSubhourlyMelt <- melt(subset(tempSubhourly, select=c(time, temp1, temp2, temp3, temp4, temp5, temp6, temp7)), id.var="time")

#plots

ggplot(tempSubhourlyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 1, aes(color=variable)) +
  facet_grid(variable ~ .) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

ggplot(tempSubhourlyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

#----
#chosen temperature

tempX <- read.table(file.choose(),header=F)

tempDailyMax <- tempX$V6
tempDailyMax[tempDailyMax==-9999] <- NA
tempDailyMax <- na.kalman(tempDailyMax)

tempDailyMin <- tempX$V7
tempDailyMin[tempDailyMin==-9999] <- NA
tempDailyMin <- na.kalman(tempDailyMin)

tempDailyChosen <- data.frame(time=timeDaily,average=tempDaily$temp7,minimum=tempDailyMin,maximum=tempDailyMax)

#plots

tempDailyChosenMelt <- melt(subset(tempDailyChosen, select=c(time,average,minimum,maximum)), id.var="time")

ggplot(tempDailyChosenMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

tempX <- data.frame(timeDaily,tempX)

ggplot(tempX, aes(x=timeDaily, y=tempX)) +
  theme_bw() +
  geom_line(size=1) +
  labs(x="time (month)", y=expression(ambient~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels = "%m", date_breaks = "1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

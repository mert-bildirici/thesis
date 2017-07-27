library(ggplot2)
library(reshape2)
library(imputeTS)

#beginning and ending time

timeBegin <- as.numeric(strptime("01.01.2016 00:00:00",format = "%d.%m.%Y %H:%M:%S",tz="Etc/GMT"))

timeEnd <- as.numeric(strptime("31.12.2016 23:55:00",format = "%d.%m.%Y %H:%M:%S",tz="Etc/GMT"))

#daily data
#----

#time

timeDaily <- as.POSIXlt(seq(timeBegin,timeEnd,86400),origin="1970-01-01",tz="Etc/GMT")

tempDaily <- data.frame(timeDaily)

#importing and arranging data

tempX <- read.table(file.choose(),header=F)

tempX <- tempX$V9

tempX[tempX==-9999] <- NA

tempX <- na.kalman(tempX)

tempDaily <- cbind(tempDaily,tempX)


colnames(tempDaily) <- c("time","temp1","temp2","temp3","temp4","temp5","temp6","temp7")

#plots

tempDailyMelt <- melt(subset(tempDaily, select=c(time,temp1,temp2,temp3,temp4,temp5,temp6,temp7)), id.var="time")

ggplot(tempDaily, aes(x=time, y=temp7)) +
  theme_bw() +
  geom_line(size=1) +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels = "%m", date_breaks = "1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

ggplot(tempDailyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 1, aes(color=variable)) +
  facet_grid(variable ~ .) +
  theme(legend.position="none") +
  labs(x="time (month)", y="temperature (C)") +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels = "%m", date_breaks = "1 month") +
  scale_y_continuous(breaks=seq(0, 40, 5))

ggplot(tempDailyMelt, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")


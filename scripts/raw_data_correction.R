library(reshape2)
library(imputeTS)
library(ggplot2)
library(sdcMicro)
library(psych)

#---
#importing data

dataRead <- read.table(file.choose(), header = T, sep = "\t")

#----
#organizing time

timePeriod <- 10

timeBegin <- as.numeric(strptime("01.01.2016 00:00:00", format = "%d.%m.%Y %H:%M:%S", tz="Etc/GMT"))

timeEnd <- as.numeric(strptime("31.12.2016 23:50:00", format = "%d.%m.%Y %H:%M:%S", tz="Etc/GMT"))


timeWithMissing <- round(as.numeric(strptime(dataRead$Zaman.GÃ.Ã..,"%d.%m.%Y %H:%M:%S", tz="Etc/GMT"))/(60*timePeriod))*(60*timePeriod)


timeAll <- seq(timeBegin, timeEnd, 60*timePeriod)

timeFinal <- as.POSIXlt(timeAll, origin="1970-01-01", tz="Etc/GMT")

#----
#filling spaces with NAs

temp1 <- data.frame(dataRead$Toplam.Aktif[c(which(timeWithMissing==timeBegin):which(timeWithMissing==timeEnd))], dataRead$Toplam.Reaktif[c(which(timeWithMissing==timeBegin):which(timeWithMissing==timeEnd))], dataRead$Toplam.Gorunen[c(which(timeWithMissing==timeBegin):which(timeWithMissing==timeEnd))])

timeWithMissing <- timeWithMissing[c(which(timeWithMissing==timeBegin):which(timeWithMissing==timeEnd))]

temp2 <- as.data.frame(matrix(NA, nrow=length(timeAll), ncol=3))

l <- 1

for(i in 1:length(timeAll)){
  
  for(k in l:length(timeWithMissing)){
    
    if(timeAll[i] == timeWithMissing[k]){
      
      temp2[i, ] <- temp1[k, ]
      
      l <- k+1
      
      break
      
    }
    
  }

}

#----
#imputting missing data

temp3 <- data.frame(na.seadec(temp2$V1, algorithm = "kalman"), na.seadec(temp2$V2, algorithm = "kalman"), na.seadec(temp2$V3, algorithm = "kalman"))

#----
#creating data frame

generationData <- data.frame(time=timeFinal, activePower=-temp3[ ,1]/1000, reactivePower=-temp3[ ,2]/1000, apparentPower=temp3[ ,3]/1000)

#----
#plotting graph

ggplot(generationData, aes(x=time, y=activePower)) +
  geom_line(size=0.1,color = "#56B1F7") +
  labs(x ="time (month)", y="power (MW)") +
  theme(text = element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks = seq(-50, 50, 10))

ggplot(generationData, aes(x=time, y=reactivePower)) +
  geom_line(size=0.1,color = "#56B1F7") +
  labs(x ="time (month)", y="power (MW)") +
  theme(text = element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks = seq(-50, 50, 10))

ggplot(generationData, aes(x=time, y=apparentPower)) +
  geom_line(size=0.1,color = "#56B1F7") +
  labs(x ="time (month)", y="power (MW)") +
  theme(text = element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks = seq(-50, 50, 10))

ggplot(generationData, aes(x=time, y=activePower/apparentPower)) +
  geom_line(size=0.1,color = "#56B1F7") +
  labs(x ="time (month)", y="power (MW)") +
  theme(text = element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

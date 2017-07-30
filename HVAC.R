library(ggplot2)

#HVAC inputs

numberHVAC <- 500
controlNumberHVAC <- 50

powerHVAC <- 2

heatingCoP <- 3
coolingCoP <- 2.5

thermCap <- 9200
thermRes <- 50

tempOut <- tempSubhourly$temp4

tempRoomWanted <- 23
tempWindow <- 2

tempRoomMin <- tempRoomWanted-(tempWindow/2)
tempRoomMax <- tempRoomWanted+(tempWindow/2)
tempRoomInitial <- tempRoomWanted

#HVAC load without DSM

tempOutSum <- 0
tempOutAverage <- 0

modeHVAC <- logical(0)

for(i in 1:length(setPower)){
  
  tempOutSum <- tempOutSum+tempOut[i]
  
  if(i%%144 == 0){

    tempOutAverage <- tempOutSum/144
    
    if(tempOutAverage >= tempRoomWanted){
      
      modeHVAC[i/144] <- 0
    }
    
    else if(tempOutAverage < tempRoomWanted){
      
      modeHVAC[i/144] <- 1 
    }
    
    tempOutSum <- 0
  }
}

modeHVAC <- rep(modeHVAC, each = 144)


tableHVAC1 <- data.frame(id=1, tempRoom=tempRoomWanted, uCl=logical(length=1), uHt=logical(1))

resultHVAC1 <- data.frame(step=integer(0), id=integer(0), tempRoom=numeric(0), uCl=logical(0), uHt=logical(0))

totalPowerHVAC1 <- numeric(length=length(setPower))

for(i in 1:length(setPower)){
  
  if(i%%144 ==0){
    
    tableHVAC1[1,3] <- 0
    tableHVAC1[1,4] <- 0
  }

  if(modeHVAC[i] == 0){
    
    if(tableHVAC1[1,2] > tempRoomMax){
      
      tableHVAC1[1,3] <- 1
      tableHVAC1[1,4] <- 0
    }
    
    else if(tableHVAC1[1,2] < tempRoomMin){
      
      tableHVAC1[1,3] <- 0
      tableHVAC1[1,4] <- 0     
    }
    
    else{
      
      if(tableHVAC1[1,3] == 1){
        
        tableHVAC1[1,3] <- 1
        tableHVAC1[1,4] <- 0
      }
      
      else{
        
        tableHVAC1[1,3] <- 0
        tableHVAC1[1,4] <- 0
      }
    }
  }

  else{
    
    if(tableHVAC1[1,2] > tempRoomMax){
      
      tableHVAC1[1,3] <- 0
      tableHVAC1[1,4] <- 0
    }
    
    else if(tableHVAC1[1,2] < tempRoomMin){
      
      tableHVAC1[1,3] <- 0
      tableHVAC1[1,4] <- 1     
    }
    
    else{
      
      if(tableHVAC1[1,4] == 1){
        
        tableHVAC1[1,3] <- 0
        tableHVAC1[1,4] <- 1
      }
      
      else{
        
        tableHVAC1[1,3] <- 0
        tableHVAC1[1,4] <- 0
      }
    }
  }
  
  tableHVAC1[1,2] <- tableHVAC1[1,2]+((((tempOut[i]-tableHVAC1[1,2])/thermRes)+((tableHVAC1[1,4]*heatingCoP-tableHVAC1[1,3]*coolingCoP)*powerHVAC))*(600/thermCap)) 
  
  totalPowerHVAC1[i] <- ((tableHVAC1[1,3]+tableHVAC1[1,4])*numberHVAC*powerHVAC)/1000
  
  resultHVAC1 <- rbind(resultHVAC1, data.frame(step=i, id=tableHVAC1[1,1], RoomTemperature=tableHVAC1[1,2], CoolingMode=tableHVAC1[1,3], HeatingMode=tableHVAC1[1,4]))  
}

powerHVAC1 <- data.frame(time=timeFinal, totalPower=totalPowerHVAC1)

resultHVAC1 <- cbind(time=timeFinal, resultHVAC1[order(resultHVAC1$id), ])

ggplot(powerHVAC1, aes(x=time, y=totalPower)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(0, 2, 0.5))

ggplot(resultHVAC1, aes(x=time, y=RoomTemperature)) +
  theme_bw() +
  geom_line(size=0.1, aes(color="red")) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC1, aes(x=time, y=CoolingMode)) +
  theme_bw() +
  geom_line(size=0.1, aes(color="#56B1F7")) +
  theme(legend.position="none") +
  labs(x="time (month)", y="cooling mode") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC1, aes(x=time, y=HeatingMode)) +
  theme_bw() +
  geom_line(size=0.1, aes(color="#56B1F7")) +
  theme(legend.position="none") +
  labs(x="time (month)", y="heating mode") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

#HVAC load with DSM

capacityLine <- 35

increaseGeneration <- 0.1
increaseLoad <- 0.02

gridPower <- (dataAll$generation*(1+increaseGeneration))-(dataAll$load*(1+increaseLoad))

setPower <- gridPower-capacityLine

tableHVAC2 <- data.frame(id=1:(numberHVAC/controlNumberHVAC), tempRoom=tempRoomWanted, uHeating=logical(length=(numberHVAC/controlNumberHVAC)), uCooling=logical(length=(numberHVAC/controlNumberHVAC)))

resultHVAC2 <- data.frame(step=integer(0), id=integer(0), tempRoom=numeric(0))

totalPowerHVAC2 <- numeric(length=length(setPower))

for(j in 1:length(setPower)){

  setPowerRemain <- setPower[j]
  
  for(i in 1:(numberHVAC/controlNumberHVAC)){
    
    if(tableHVAC2[i,2] < tempRoomMin){
      
      tableHVAC2[i,3] <- 1
      tableHVAC2[i,4] <- 0
    }
    
    else if(tableHVAC2[i,2] > tempRoomMax){
      
      tableHVAC2[i,3] <- 0
      tableHVAC2[i,4] <- 1
    }
    
    else{
      
      tableHVAC2[i,3] <- 0
      tableHVAC2[i,4] <- 0
    }
  }
  
  tableHVAC2 <- tableHVAC2[order(-tableHVAC2$uHeating, -tableHVAC2$uCooling), ]
  
  for(i in 1:(numberHVAC/controlNumberHVAC)){
    
    if((tableHVAC2[i,3]+tableHVAC2[i,4]) == 1){
      
      setPowerRemain <- setPowerRemain-((powerHVAC*controlNumberHVAC)/1000)
    }
    
    else{
      
      if(setPowerRemain > 0){
        
        if(tableHVAC2[i,2] < tempRoomWanted){
          
          tableHVAC2[i,3] <- 1
          tableHVAC2[i,4] <- 0
        }
        
        else{
          
          tableHVAC2[i,3] <- 0
          tableHVAC2[i,4] <- 1
        }
        
        setPowerRemain <- setPowerRemain-((powerHVAC*controlNumberHVAC)/1000)
      }
    }
    
    tableHVAC2[i,2] <- tableHVAC2[i,2]+((((tempOut[j]-tableHVAC2[i,2])/thermRes)+((tableHVAC2[i,3]*heatingCoP-tableHVAC2[i,4]*coolingCoP)*powerHVAC))*(600/thermCap))
  }

  totalPowerHVAC2[j] <-  setPower[j]-setPowerRemain
  
  resultHVAC2 <- rbind(resultHVAC2, data.frame(step=j, id=tableHVAC2[ ,1], tempRoom=tableHVAC2[ ,2]))
  
  if(j %% 1008 ==0){
    
    print(j/1008)
  }
}

resultHVAC2 <- cbind(time=timeFinal, resultHVAC2[order(resultHVAC2$id), ])

powerHVAC2 <- data.frame(time=timeFinal, totalPower=totalPowerHVAC2)

ggplot(powerHVAC2, aes(x=time, y=totalPower)) +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")+
  scale_y_continuous(breaks=seq(0, 2, 0.5))

ggplot(resultHVAC2, aes(x=time, y=tempRoom)) +
  geom_line(size=0.1, aes(color=id)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")


resultGridPower <- data.frame(time=timeFinal, withoutDSM=(gridPower-totalPowerHVAC1), withDSM=(gridPower-totalPowerHVAC2))
resultGridPower <- melt(subset(resultGridPower, select=c(time, withoutDSM, withDSM)), id.var="time")

ggplot(resultGridPower, aes(x=time, y=value)) +
  geom_line(size=1, aes(color=variable)) +
  facet_grid(variable~.) +
  theme(legend.position="none") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(-50, 50, 10)) +
  geom_line(aes(y=capacityLine), size=1, color="red4") +
  geom_line(aes(y=-capacityLine), size=1, color="red4")

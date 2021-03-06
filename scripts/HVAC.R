library(ggplot2)
library(reshape2)

#----
#HVAC mode for 12 hour periods(7am-7pm, 7pm-7am)

modeHVAC <- logical()

tempOutSum <- 0

for(i in 43:52674){
  
  tempOutSum <- tempOutSum+tempOut[i]
  
  if((i-42)%%72 == 0){
    
    if((tempOutSum/72) > tempRoomWanted){
      
      modeHVAC[(i-7)/72] <- 0
    }
    
    else{
      
      modeHVAC[(i-7)/72] <- 1
    }
    
    tempOutSum <- 0
  }
}

modeHVAC <- rep(modeHVAC, each=72)

modeHVAC <- c(rep(1,42), modeHVAC, rep(1,30))

#----
#HVAC load without DSM

tableHVAC1 <- data.frame(id=1, roomTemperature=tempRoomWanted, coolingMode=logical(length=1), heatingMode=logical(1))

resultHVAC1 <- data.frame(integer(0), integer(0), numeric(0))

totalPowerHVAC1 <- numeric(length(grid))

for(i in 1:length(grid)){
  
  if(i > 1){
    
    if(modeHVAC[i] != modeHVAC[i-1]){
      
      tableHVAC1[1,3] <- 0
      tableHVAC1[1,4] <- 0
    }
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
  
  resultHVAC1 <- rbind(resultHVAC1, data.frame(step=i, id=tableHVAC1[1,1], roomTemperature=tableHVAC1[1,2]))  
}

powerHVAC1 <- data.frame(time=timeFinal, totalPower=totalPowerHVAC1)

resultHVAC1 <- cbind(time=timeFinal, resultHVAC1[order(resultHVAC1$id), ], row.names=NULL)

ggplot(resultHVAC1, aes(x=time, y=roomTemperature)) +
  theme_bw() +
  geom_line(size=0.1, aes(color="red")) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%d.%m", date_breaks="1 month") +
  scale_y_continuous(limits=c((tempRoomMin-1), (tempRoomMax+1)), breaks=seq((tempRoomMin-1), (tempRoomMax+1), 1))

ggplot(powerHVAC1, aes(x=time, y=totalPower)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%d.%m", date_breaks="1 month")

#----
#HVAC load with DSM

for(i in 1:length(grid)){
  
  if((grid[i] <= (lineCapacity*(1-lineSafetyMargin))) && (grid[i] >= -(lineCapacity*(1-lineSafetyMargin)))){
   
    setPowerHVAC[i] <- 0 
  }
  
  else if(grid[i] > (lineCapacity*(1-lineSafetyMargin))){
    
    setPowerHVAC[i] <- grid[i]-(lineCapacity*(1-lineSafetyMargin))
  }
  
  else if(grid[i] < -(lineCapacity*(1-lineSafetyMargin))){
    
    setPowerHVAC[i] <- grid[i]+(lineCapacity*(1-lineSafetyMargin))
  }
}

plot(setPowerHVAC, type="l")


tableHVAC2 <- data.frame(id=1:(numberHVAC/controlNumberHVAC), roomTemperature=tempRoomWanted, coolingMode=logical(length=(numberHVAC/controlNumberHVAC)), heatingMode=logical(length=(numberHVAC/controlNumberHVAC)))

resultHVAC2 <- data.frame(integer(0), integer(0), numeric(0), logical(0), logical(0))

totalPowerHVAC2 <- numeric(length=length(setPowerHVAC))

for(j in 1:length(setPowerHVAC)){

  setPowerHVACRemain <- setPowerHVAC[j]
  
  for(i in 1:(numberHVAC/controlNumberHVAC)){
    
    tableHVAC2[i,3] <- 0
    tableHVAC2[i,4] <- 0
    
    if(tableHVAC2[i,2] > tempRoomMax){
      
      if(modeHVAC[j] == 0){
        
        tableHVAC2[i,3] <- 1
      }
    }
    
    else if(tableHVAC2[i,2] < tempRoomMin){
      
      if(modeHVAC[j] == 1){
        
        tableHVAC2[i,4] <- 1
      }
    }
    
    else{
      
      if(setPowerHVACRemain > 0){
        
        if(modeHVAC[j] == 0){
          
          if(tableHVAC2[i,2] > (tempRoomMin+0.5)){
            
            tableHVAC2[i,3] <- 1
          }
        }
        
        else if(modeHVAC[j] == 1){
          
          if(tableHVAC2[i,2] > (tempRoomMax-0.5)){
            
            tableHVAC2[i,4] <- 1
          }
        }
      }
    }
    
    setPowerHVACRemain <- setPowerHVACRemain-((tableHVAC2[i,3]+tableHVAC2[i,4])*((powerHVAC*controlNumberHVAC)/1000))
    
    tableHVAC2[i,2] <- tableHVAC2[i,2]+((((tempOut[j]-tableHVAC2[i,2])/thermRes)+((tableHVAC2[i,4]*heatingCoP-tableHVAC2[i,3]*coolingCoP)*powerHVAC))*(600/thermCap))
  
  }
  
  totalPowerHVAC2[j] <-  setPowerHVAC[j]-setPowerHVACRemain
  
  resultHVAC2 <- rbind(resultHVAC2, data.frame(step=j, id=tableHVAC2[ ,1], roomTemperature=tableHVAC2[ ,2]))
  
  tableHVAC2 <- tableHVAC2[order(-tableHVAC2$coolingMode, -tableHVAC2$heatingMode), ]
  
  if(j %% 144 ==0){
    
    print(j/144)
  }
}

resultHVAC2 <- cbind(time=timeFinal, resultHVAC2[order(resultHVAC2$id), ])

powerHVAC2 <- data.frame(time=timeFinal, totalPower=totalPowerHVAC2)

ggplot(powerHVAC2, aes(x=time, y=totalPower)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC2, aes(x=time, y=roomTemperature)) +
  theme_bw() +
  geom_line(size=0.1, aes(color=id)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

resultGridPower <- data.frame(time=timeFinal, withoutDSM=(gridPower-totalPowerHVAC1), withDSM=(gridPower-totalPowerHVAC2))

ggplot(resultGridPower, aes(x=time, y=withoutDSM)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(-50, 50, 10)) +
  geom_line(aes(y=lineCapacity), size=1, color="red4") +
  geom_line(aes(y=-lineCapacity), size=1, color="red4")

ggplot(resultGridPower, aes(x=time, y=withDSM)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(-50, 50, 10)) +
  geom_line(aes(y=lineCapacity), size=1, color="red4") +
  geom_line(aes(y=-lineCapacity), size=1, color="red4")

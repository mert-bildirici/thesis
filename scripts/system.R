#----

library(ggplot2)
library(reshape2)

#----

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

for(i in 1:length(grid)){
  
  if(i == 1){
    
    tableHVAC1 <- data.frame(group=1, tempRoom=tempRoomWanted, modeCooling=logical(1), modeHeating=logical(1))    
    
    tempRoom1 <- as.data.frame(matrix(nrow=length(grid), ncol=1))
    
    outputPowerHVAC1 <- numeric(length(grid))
  }
  
  else{
    
    if(modeHVAC[i] != modeHVAC[i-1]){
      
      tableHVAC1[1,3] <- 0
      tableHVAC1[1,4] <- 0
    }
    
    else{
      
      if(modeHVAC[i] == 0){
        
        if(tableHVAC1[1,2] > tempRoomMax){
          
          tableHVAC1[1,3] <- 1
          tableHVAC1[1,4] <- 0
        }
        
        else if(tableHVAC1[1,2] < tempRoomMin+((coolingCoP*coolingPower)*(600/thermCap))){
          
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
        
        if(tableHVAC1[1,2] > tempRoomMax-((heatingCoP*heatingPower)*(600/thermCap))){
          
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
    }
  }
  
  tempRoom1[i,1] <- tableHVAC1[1,2]
  
  tableHVAC1[1,2] <- tableHVAC1[1,2]+((((tempOut[i]-tableHVAC1[1,2])/thermRes)+(tableHVAC1[1,4]*heatingCoP*heatingPower-tableHVAC1[1,3]*coolingCoP*coolingPower))*(600/thermCap)) 
  
  outputPowerHVAC1[i] <- ((tableHVAC1[1,3]*coolingPower+tableHVAC1[1,4]*heatingPower)*numberHVAC)/1000
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
}


#----

for(i in 1:length(grid)){
  
  #----
  #initialization
  
  if(i == 1){
    
    tableHVAC2 <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeCooling=logical(groupHVAC), modeHeating=logical(groupHVAC))
    
    tempRoom2 <- as.data.frame(matrix(nrow=length(grid), ncol=groupHVAC))
    
    setPowerHVAC <- numeric(length(grid))
    
    outputPowerHVAC2 <- numeric(length(grid))
    
    
    tableBESS <- data.frame(SOC=maxSOC, modeDischarging=logical(1), modeCharging=logical(1))
    
    SOC <- numeric(length(grid))
    
    setPowerBESS <- numeric(length(grid))
    
    outputPowerBESS <- numeric(length(grid))
  }
  
  #----
  #HVAC
  
  if(grid[i]-outputPowerHVAC1[i] > (lineCapacity*(1-lineSafetyMargin))){
    
    setPowerHVAC[i] <- (grid[i]-outputPowerHVAC1[i])-(lineCapacity*(1-lineSafetyMargin))
  }
  
  else{
    
    setPowerHVAC[i] <- outputPowerHVAC1[i]
  }
  
  
  tempRoom2[i, ] <- as.numeric(tableHVAC2[ ,2])
  
  if(modeHVAC[i] == 0){
    
    tableHVAC2 <- tableHVAC2[order(-tableHVAC2[ ,2]), ]
  }
  
  else if(modeHVAC[i] == 1){
    
    tableHVAC2 <- tableHVAC2[order(tableHVAC2[ ,2]), ]
  }
  
  for(j in 1:groupHVAC){
    
    if(modeHVAC[i] == 0){
      
      if(tableHVAC2[j,2] > tempRoomMax){
        
        tableHVAC2[j,3] <- 1
        tableHVAC2[j,4] <- 0
      }
      
      else if(tableHVAC2[j,2] < tempRoomMin+((coolingCoP*coolingPower)*(600/thermCap))){
        
        tableHVAC2[j,3] <- 0
        tableHVAC2[j,4] <- 0
      }
      
      else{
        
        if(setPowerHVAC[i] > 0){
          
          tableHVAC2[j,3] <- 1
          tableHVAC2[j,4] <- 0
        }
        
        else{
          
          tableHVAC2[j,3] <- 0
          tableHVAC2[j,4] <- 0
        }
      }
    }
    
    else if(modeHVAC[i] == 1){
      
      if(tableHVAC2[j,2] < tempRoomMin){
        
        tableHVAC2[j,3] <- 0
        tableHVAC2[j,4] <- 1
      }
      
      else if(tableHVAC2[j,2] > tempRoomMax-((heatingCoP*heatingPower)*(600/thermCap))){
        
        tableHVAC2[j,3] <- 0
        tableHVAC2[j,4] <- 0
      }
      
      else{
        
        if(setPowerHVAC[i] > 0){
          
          tableHVAC2[j,3] <- 0
          tableHVAC2[j,4] <- 1
        }
        
        else{
          
          tableHVAC2[j,3] <- 0
          tableHVAC2[j,4] <- 0
        }
      }
    }
    
    tableHVAC2[j,2] <- tableHVAC2[j,2]+((((tempOut[i]-tableHVAC2[j,2])/thermRes)+(tableHVAC2[j,4]*heatingCoP*heatingPower-tableHVAC2[j,3]*coolingCoP*coolingPower))*(600/thermCap))
    
    setPowerHVAC[i] <- setPowerHVAC[i]-(((tableHVAC2[j,3]*coolingPower+tableHVAC2[j,4]*heatingPower)*(numberHVAC/groupHVAC))/1000)
    
    outputPowerHVAC2[i] <- outputPowerHVAC2[i]+(((tableHVAC2[j,3]*coolingPower+tableHVAC2[j,4]*heatingPower)*(numberHVAC/groupHVAC))/1000)
  }
  
  tableHVAC2 <- tableHVAC2[order(tableHVAC2[ ,1]), ]
  
  
  #----
  #BESS
  
  setPowerBESS[i] <- (lineCapacity*(1-lineSafetyMargin))-(grid[i]+outputPowerHVAC1[i]-outputPowerHVAC2[i])
  
  
  if(setPowerBESS[i] >= 0){
    
    tableBESS[1,2] <- 1
    tableBESS[1,3] <- 0
    
    if(tableBESS[1,1] > minSOC){
      
      outputPowerBESS[i] <- ((tableBESS[1,1]-minSOC)*energyBESS)*6
      
      if(outputPowerBESS[i] > powerBESS){
        
        outputPowerBESS[i] <- powerBESS
      }
      
      if(outputPowerBESS[i] > setPowerBESS[i]){
        
        outputPowerBESS[i] <- setPowerBESS[i]
      }
      
      outputPowerBESS[i] <- outputPowerBESS[i]*dschEff
    }
    
    else{
      
      outputPowerBESS[i] <- 0
    }
  }
  
  else{
    
    tableBESS[1,2] <- 0
    tableBESS[1,3] <- 1
    
    if(tableBESS[1,1] < maxSOC){
      
      outputPowerBESS[i] <- ((tableBESS[1,1]-maxSOC)*energyBESS)*6
      
      if(outputPowerBESS[i] < -powerBESS){
        
        outputPowerBESS[i] <- -powerBESS
      }
      
      if(outputPowerBESS[i] < setPowerBESS[i]){
        
        outputPowerBESS[i] <- setPowerBESS[i]
      }
    }
    
    else{
      
      outputPowerBESS[i] <- 0
    }
  }
  
  SOC[i] <- tableBESS[1,1]
  
  tableBESS[1,1] <- tableBESS[1,1]-((((tableBESS[1,2]/dschEff)+(tableBESS[1,3]*chEff))*(outputPowerBESS[i]/6))/energyBESS)
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
  
  #finalization
  
  
  if(i == length(grid)){
    
    resultHVAC <- data.frame(timeFinal, tempRoom2, outputPowerHVAC2)
    colnames(resultHVAC) <- c("time", 1:groupHVAC, "output")
    
    resultBESS <- data.frame(timeFinal, SOC, outputPowerBESS)
    colnames(resultBESS) <- c("time", "SOC", "output")
  }
}

resultGeneration <- data.frame(timeFinal, generation, generation+outputPowerBESS)
colnames(resultGeneration) <- c("time", "before", "after")

resultLoad <- data.frame(timeFinal, load, load-outputPowerHVAC1+outputPowerHVAC2)
colnames(resultLoad) <- c("time", "before", "after")

resultGrid <- data.frame(timeFinal, grid, resultGeneration$after-resultLoad$after)
colnames(resultGrid) <- c("time", "before", "after")

meltedResultHVAC <- melt(subset(resultHVAC, select=c(-output)), id.var="time")


#graphs

ggplot(resultGeneration, aes(x=time, y=before)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultGeneration, aes(x=time, y=after)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultLoad, aes(x=time, y=before)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultLoad, aes(x=time, y=after)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultGrid, aes(x=time, y=before)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultGrid, aes(x=time, y=after)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC, aes(x=time, y=output)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,2))


meltedResultHVAC <- melt(subset(resultHVAC, select=c(-output)), id.var="time")

ggplot(meltedResultHVAC, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=variable)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(21, 23, 0.5))


ggplot(resultBESS, aes(x=time, y=output)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(-2.2,2.2), breaks=seq(-2.2,2.2,0.2))

ggplot(resultBESS, aes(x=time, y=SOC)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(0.25,1), breaks=seq(0,1,0.1))

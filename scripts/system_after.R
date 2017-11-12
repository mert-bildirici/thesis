#library----

library(ggplot2)
library(reshape2)

#simulation----

for(i in 1:length(grid)){
  
  #initialization----
  
  if(i == 1){
    
    #HVAC initialization----
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeCooling=logical(groupHVAC), modeHeating=logical(groupHVAC))

    tempRoom <- as.data.frame(matrix(nrow=length(grid), ncol=groupHVAC))
    
    setPowerHVAC <- numeric(length(grid))
    
    outputPowerHVAC_after <- numeric(length(grid))
    
    #BESS initialization----
    
    tableBESS <- data.frame(SOC=maxSOC, modeDischarging=logical(1), modeCharging=logical(1))
    
    SOC <- numeric(length(grid))
    
    setPowerBESS <- numeric(length(grid))
    
    outputPowerBESS <- numeric(length(grid))
  }
  
  #HVAC control----

  tempRoom[i, ] <- as.numeric(tableHVAC[ ,2])
  
  #cooling mode----
  
  if(modeHVAC[i] == 0){
    
    if((grid[i]+outputPowerHVAC_before[i]) > (lineCapacity*(1-lineSafetyMargin))){
      
      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC_before[i])-(lineCapacity*(1-lineSafetyMargin))

      controlHVAC <- ceiling(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(coolingPower/1000)))
    }
    
    else{
      
      setPowerHVAC[i] <- 0

      controlHVAC <- 0
    }
    
    tableHVAC[ ,4] <- 0
    
    tableHVAC <- tableHVAC[order(-tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] >= tempRoomMax){
        
        tableHVAC[j,3] <- 1
        
        controlHVAC <- controlHVAC-1
      }
      
      else  if((tableHVAC[j,2] < tempRoomMax) && (tableHVAC[j,2] > tempRoomMin)){
        
        if(controlHVAC > 0){
          
          tableHVAC[j,3] <- 1
          
          controlHVAC <- controlHVAC-1
        }
        
        else{
          
          tableHVAC[j,3] <- tableHVAC[j,3]
        }
      }
      
      else{
        
        tableHVAC[j,3] <- 0
      }
    }
  }
  
  
  #heating mode----
  
  else if(modeHVAC[i] == 1){
    
    if((grid[i]+outputPowerHVAC_before[i]) > (lineCapacity*(1-lineSafetyMargin))){
      
      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC_before[i])-(lineCapacity*(1-lineSafetyMargin))
      
      controlHVAC <- ceiling(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(heatingPower/1000)))
    }
    
    else{
      
      setPowerHVAC[i] <- 0
      
      controlHVAC <- 0
    }
    
    tableHVAC[ ,3] <- 0
    
    tableHVAC <- tableHVAC[order(tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] <= tempRoomMin){
        
        tableHVAC[j,4] <- 1
        
        controlHVAC <- controlHVAC-1
      }
      
      else  if((tableHVAC[j,2] > tempRoomMin) && (tableHVAC[j,2] < tempRoomMax)){
        
        if(controlHVAC > 0){
          
          tableHVAC[j,4] <- 1
          
          controlHVAC <- controlHVAC-1
        }
        
        else{
          
          tableHVAC[j,4] <- tableHVAC[j,4]
        }
      }
      
      else{
        
        tableHVAC[j,4] <- 0
      }
    }
  }
  
  #step calculations----

  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  outputPowerHVAC_after[i] <- (sum(tableHVAC[ ,3])*(coolingPower/1000)+sum(tableHVAC[ ,4])*(heatingPower/1000))*(numberHVAC/groupHVAC)
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+(tableHVAC[j,4]*heatingCoP*heatingPower-tableHVAC[j,3]*coolingCoP*coolingPower))*(600/thermCap)) 
  }

  
  #BESS control----

  # setPowerBESS[i] <- 0
  # 
  # 
  # if(setPowerBESS[i] >= 0){
  #   
  #   tableBESS[1,2] <- 1
  #   tableBESS[1,3] <- 0
  #   
  #   if(tableBESS[1,1] > minSOC){
  #     
  #     outputPowerBESS[i] <- ((tableBESS[1,1]-minSOC)*energyBESS)*6
  #     
  #     if(outputPowerBESS[i] > powerBESS){
  #       
  #       outputPowerBESS[i] <- powerBESS
  #     }
  #     
  #     if(outputPowerBESS[i] > setPowerBESS[i]){
  #       
  #       outputPowerBESS[i] <- setPowerBESS[i]
  #     }
  #     
  #     outputPowerBESS[i] <- outputPowerBESS[i]*dschEff
  #   }
  #   
  #   else{
  #     
  #     outputPowerBESS[i] <- 0
  #   }
  # }
  # 
  # else{
  #   
  #   tableBESS[1,2] <- 0
  #   tableBESS[1,3] <- 1
  #   
  #   if(tableBESS[1,1] < maxSOC){
  #     
  #     outputPowerBESS[i] <- ((tableBESS[1,1]-maxSOC)*energyBESS)*6
  #     
  #     if(outputPowerBESS[i] < -powerBESS){
  #       
  #       outputPowerBESS[i] <- -powerBESS
  #     }
  #     
  #     if(outputPowerBESS[i] < setPowerBESS[i]){
  #       
  #       outputPowerBESS[i] <- setPowerBESS[i]
  #     }
  #   }
  #   
  #   else{
  #     
  #     outputPowerBESS[i] <- 0
  #   }
  # }
  # 
  # SOC[i] <- tableBESS[1,1]
  # 
  # tableBESS[1,1] <- tableBESS[1,1]-((((tableBESS[1,2]/dschEff)+(tableBESS[1,3]*chEff))*(outputPowerBESS[i]/6))/energyBESS)
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
  
  #finalization
  
  if(i == length(grid)){
    
    resultHVAC_after <- data.frame(timeFinal, tempRoom, outputPowerHVAC_after)
    colnames(resultHVAC_after) <- c("time", 1:groupHVAC, "power")
    
    resultBESS_after <- data.frame(timeFinal, SOC, outputPowerBESS)
    colnames(resultBESS_after) <- c("time", "SOC", "power")
  }
}

resultGeneration_after <- data.frame(timeFinal, generation+outputPowerBESS)
colnames(resultGeneration_after) <- c("time", "power")

resultLoad_after <- data.frame(timeFinal, load-outputPowerHVAC_before+outputPowerHVAC_after)
colnames(resultLoad_after) <- c("time", "power")

resultGrid_after <- data.frame(timeFinal, resultGeneration_after$power-resultLoad_after$power)
colnames(resultGrid_after) <- c("time", "power")

meltedResultHVAC_after <- melt(subset(resultHVAC_after, select=c(-power)), id.var="time")
colnames(meltedResultHVAC_after) <- c("time", "group", "tempRoom")


#graphs----

ggplot(resultGrid_after, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC_after, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(meltedResultHVAC_after, aes(x=time, y=tempRoom)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=group)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(tempRoomWanted-(tempWindow/2), tempRoomWanted+(tempWindow/2), 0.5))



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
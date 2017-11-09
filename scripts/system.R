#----

library(ggplot2)
library(reshape2)

#----

for(i in 1:length(grid)){
  
  #----
  #initialization
  
  if(i == 1){
    
    #----
    #HVAC
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeCooling=logical(groupHVAC), modeHeating=logical(groupHVAC))

    tempRoom <- as.data.frame(matrix(nrow=length(grid), ncol=groupHVAC))
    
    setPowerHVAC <- numeric(length(grid))
    
    outputPowerHVAC2 <- numeric(length(grid))
    
    #----
    #BESS
    
    tableBESS <- data.frame(SOC=maxSOC, modeDischarging=logical(1), modeCharging=logical(1))
    
    SOC <- numeric(length(grid))
    
    setPowerBESS <- numeric(length(grid))
    
    outputPowerBESS <- numeric(length(grid))
  }
  
  #----
  #HVAC control

  tempRoom[i, ] <- as.numeric(tableHVAC[ ,2])
  
  #----
  #cooling mode
  
  if(modeHVAC[i] == 0){
    
    tableHVAC[ ,4] <- 0
    
    #----
    #limit check
    
    tableHVAC <- tableHVAC[order(-tableHVAC[ ,3], tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] < tempRoomMin){
        
        tableHVAC[j,3] <- 0
      }
      
      else{
        
        break
      }
    }
    
    for(j in groupHVAC:1){
      
      if(tableHVAC[j,2] > tempRoomMax){
        
        tableHVAC[j,3] <- 1
      }
      
      else{
        
        break
      }
    }
    
    k <- sum(tableHVAC[ ,3])

    #----
    #power set

    tableHVAC <- tableHVAC[order(-tableHVAC[ ,3], tableHVAC[ ,2]), ]

    if((grid[i]+outputPowerHVAC1[i]-k*coolingPower) > (lineCapacity*(1-lineSafetyMargin))){

      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC1[i]-k*coolingPower)-(lineCapacity*(1-lineSafetyMargin))

      controlHVAC_ON <- 0
      controlHVAC_OFF <- ceiling(abs(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(coolingPower/1000))))
      
      if(controlHVAC_OFF > groupHVAC-k){
        
        controlHVAC_OFF <- groupHVAC-k
      }
    }

    else if((grid[i]+outputPowerHVAC1[i]-k*coolingPower) < -(lineCapacity*(1-lineSafetyMargin))){

      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC1[i]-k*coolingPower)+(lineCapacity*(1-lineSafetyMargin))

      controlHVAC_ON <- ceiling(abs(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(coolingPower/1000))))
      controlHVAC_OFF <- 0
      
      if(controlHVAC_ON > k){
        
        controlHVAC_ON <- k
      }
    }

    else{

      setPowerHVAC[i] <- 0

      controlHVAC_ON <- 0
      controlHVAC_OFF <- 0
    }


    if(controlHVAC_OFF > 0){

      for(j in groupHVAC:(groupHVAC-controlHVAC_OFF+1)){
        
        tableHVAC[j,3] <- 1
      }
    }

    if(controlHVAC_ON > 0){

      for(j in 1:controlHVAC_ON){
        
        tableHVAC[j,3] <- 0
      }
    }
  }
  
  #----
  #heating mode
  
  if(modeHVAC[i] == 1){
    
    tableHVAC[ ,3] <- 0
    
    #----
    #limit check    
    
    tableHVAC <- tableHVAC[order(-tableHVAC[ ,4], -tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] > tempRoomMax){
        
        tableHVAC[j,4] <- 0
      }
      
      else{
        
        break
      }
    }
    
    for(j in groupHVAC:1){
      
      if(tableHVAC[j,2] < tempRoomMin){
        
        tableHVAC[j,4] <- 1
      }
      
      else{
        
        break
      }
    }
    
    k <- sum(tableHVAC[ ,4])
    
    #----
    #power set

    tableHVAC <- tableHVAC[order(-tableHVAC[ ,4], -tableHVAC[ ,2]), ]

    if((grid[i]+outputPowerHVAC1[i]-k*heatingPower) > (lineCapacity*(1-lineSafetyMargin))){

      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC1[i]-k*heatingPower)-(lineCapacity*(1-lineSafetyMargin))

      controlHVAC_ON <- 0
      controlHVAC_OFF <- ceiling(abs(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(heatingPower/1000))))
      
      if(controlHVAC_OFF > groupHVAC-k){
        
        controlHVAC_OFF <- groupHVAC-k
      }
    }

    else if((grid[i]+outputPowerHVAC1[i]-k*heatingPower) < -(lineCapacity*(1-lineSafetyMargin))){

      setPowerHVAC[i] <- (grid[i]+outputPowerHVAC1[i]-k*heatingPower)+(lineCapacity*(1-lineSafetyMargin))

      controlHVAC_ON <- ceiling(abs(setPowerHVAC[i]/((numberHVAC/groupHVAC)*(heatingPower/1000))))
      controlHVAC_OFF <- 0
      
      if(controlHVAC_ON > k){
        
        controlHVAC_ON <- k
      }
    }

    else{

      setPowerHVAC[i] <- 0

      controlHVAC_ON <- 0
      controlHVAC_OFF <- 0
    }


    if(controlHVAC_OFF > 0){
      
      for(j in groupHVAC:(groupHVAC-controlHVAC_OFF+1)){
        
        tableHVAC[j,4] <- 1
      }
    }
    
    if(controlHVAC_ON > 0){
      
      for(j in 1:controlHVAC_ON){
        
        tableHVAC[j,4] <- 0
      }
    }
  }
  
  #----
  #step calculations for HVAC
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  outputPowerHVAC2[i] <- (sum(tableHVAC[ ,3]*(coolingPower/1000))+(sum(tableHVAC[ ,4])*(heatingPower/1000)))*(numberHVAC/groupHVAC)
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+(tableHVAC[j,4]*heatingCoP*heatingPower-tableHVAC[j,3]*coolingCoP*coolingPower))*(600/thermCap)) 
  }

  
  #----
  #BESS
  
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
    
    resultHVAC <- data.frame(timeFinal, tempRoom, outputPowerHVAC2)
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
  scale_x_datetime(date_labels="%m", date_breaks="1 month") 
  scale_y_continuous(limits=c(0,20), breaks=seq(0,20,2))


ggplot(meltedResultHVAC, aes(x=time, y=value)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=variable)) +
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
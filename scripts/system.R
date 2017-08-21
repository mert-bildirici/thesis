#----

for(i in 1:length(grid)){

  #initialization
  
  if(i == 1){
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeCooling=logical(groupHVAC), modeHeating=logical(groupHVAC))
    
    tempRoom <- as.data.frame(matrix(nrow=length(grid), ncol=groupHVAC))
    
    setPowerHVAC <- numeric(length(grid))
    setPowerBESS <- numeric(length(grid))
    
    outputPowerHVAC <- numeric(length(grid))
    outputPowerBESS <- numeric(length(grid))
    
    SOC <- numeric(length(grid)+1)
    SOC[1] <- 0.9
  }
  
  #HVAC power set
  
  if(grid[i] > (lineCapacity*(1-lineSafetyMargin))){
    
    setPowerHVAC[i] <- grid[i]-(lineCapacity*(1-lineSafetyMargin))
  }
  
  #HVAC operation
  
  for(j in 1:groupHVAC){
    
    if(tableHVAC[j,2] > tempRoomMax){
      
      if(modeHVAC[i] == 0){
        
        tableHVAC[j,3] <- 1
        tableHVAC[j,4] <- 0
        
        setPowerHVAC[i] <- setPowerHVAC[i]-((powerHVAC*(numberHVAC/groupHVAC))/1000)
        outputPowerHVAC[i] <- outputPowerHVAC[i]+((powerHVAC*(numberHVAC/groupHVAC))/1000)
      }
    }
    
    else if(tableHVAC[j,2] < tempRoomMin){
      
      if(modeHVAC[i] == 1){
        
        tableHVAC[j,3] <- 0
        tableHVAC[j,4] <- 1
        
        setPowerHVAC[i] <- setPowerHVAC[i]-((powerHVAC*(numberHVAC/groupHVAC))/1000)
        outputPowerHVAC[i] <- outputPowerHVAC[i]+((powerHVAC*(numberHVAC/groupHVAC))/1000)
      }
    }
    
    else{
      
      tableHVAC[j,3] <- 0
      tableHVAC[j,4] <- 0
    }
  }
  
  for(j in 1:groupHVAC){
    
    if(setPowerHVAC[i] > 0){
      
      if(tableHVAC[j,3]+tableHVAC[j,4] == 0){
        
        if(tableHVAC[j,2] > (tempRoomMin+0.5)){
          
          if(modeHVAC[i] == 0){
            
            tableHVAC[j,3] <- 1
            tableHVAC[j,4] <- 0
            
            setPowerHVAC[i] <- setPowerHVAC[i]-((powerHVAC*(numberHVAC/groupHVAC))/1000)
            outputPowerHVAC[i] <- outputPowerHVAC[i]+((powerHVAC*(numberHVAC/groupHVAC))/1000)
          }
        }
        
        else if(tableHVAC[j,2] < (tempRoomMax-0.5)){
          
          if(modeHVAC[i] == 1){
            
            tableHVAC[j,3] <- 0
            tableHVAC[j,4] <- 1
            
            setPowerHVAC[i] <- setPowerHVAC[i]-((powerHVAC*(numberHVAC/groupHVAC))/1000)
            outputPowerHVAC[i] <- outputPowerHVAC[i]+((powerHVAC*(numberHVAC/groupHVAC))/1000)
          }
        }
      }
    }
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+((tableHVAC[j,4]*heatingCoP-tableHVAC[j,3]*coolingCoP)*powerHVAC))*(600/thermCap))
  }
  
  tableHVAC <- tableHVAC[order(-tableHVAC[ ,3],-tableHVAC[ ,4]), ]
  
  tempRoom[i, ] <- as.numeric(tableHVAC[order(tableHVAC[ ,1]),2])
  
  #BESS power set
  
  setPowerBESS[i] <- -((grid[i]-outputPowerHVAC[i])-(lineCapacity*(1-lineSafetyMargin)))
    
  #BESS operation
  
  if(setPowerBESS[i] >= 0){
    
    outputPowerBESS[i] <- ((SOC[i]-minSOC)*energyBESS)*6
    
    if(outputPowerBESS[i] > powerBESS){
      
      outputPowerBESS[i] <- powerBESS
    }
    
    if(outputPowerBESS[i] > setPowerBESS[i]){
      
      outputPowerBESS[i] <- setPowerBESS[i]
    }
    
    SOC[i] <- SOC[i]-((outputPowerBESS[i]/6)/(energyBESS))
    
    outputPowerBESS[i] <- outputPowerBESS[i]*dschEff
  }
    
  else{
    
    outputPowerBESS[i] <- ((SOC[i]-maxSOC)*energyBESS)*6
    
    if(outputPowerBESS[i] < -powerBESS){
      
      outputPowerBESS[i] <- -powerBESS
    }
    
    if(outputPowerBESS[i] < setPowerBESS[i]){
      
      outputPowerBESS[i] <- setPowerBESS[i]
    }
    
    SOC[i] <- SOC[i]-(((outputPowerBESS[i]*chEff)/6)/(energyBESS))
  }
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
  
  #finalization
  
  if(i == length(grid)){
    
    resultHVAC <- cbind(timeFinal, tempRoom, outputPowerHVAC)
    
    resultBESS <- cbind(timeFinal, SOC[1:length(grid)], outputPowerBESS)
  }
}

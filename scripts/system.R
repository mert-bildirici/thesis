#----

for(i in 1:length(grid)){

  #----
  #initialization
  
  if(i == 1){
    
    #HVAC
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeCooling=logical(groupHVAC), modeHeating=logical(groupHVAC))
    
    tempRoom <- as.data.frame(matrix(nrow=length(grid), ncol=groupHVAC))

    resultHVAC <- data.frame()
    
    setPowerHVAC <- numeric(length(grid))
    
    outputPowerHVAC <- numeric(length(grid))
    
    #BESS
    
    tableBESS <- data.frame(SOC=maxSOC, modeDischarging=logical(1), modeCharging=logical(1))
    
    SOC <- numeric(length(grid))

    resultBESS <- data.frame()
    
    setPowerBESS <- numeric(length(grid))
    
    outputPowerBESS <- numeric(length(grid))
  }
  
  #----
  #HVAC
  
  #Setting power
  
  if(grid[i] > (lineCapacity*(1-lineSafetyMargin))){
    
    setPowerHVAC[i] <- grid[i]-(lineCapacity*(1-lineSafetyMargin))
  }
  
  else{
    
    setPowerHVAC[i] <- 0
  }
  
  #Operation
  
  for(j in 1:groupHVAC){
    
    if(tableHVAC[j,2] > tempRoomMax){
      
      if(modeHVAC[i] == 0){
        
        tableHVAC[j,3] <- 1
        tableHVAC[j,4] <- 0
      }
      
      else{
        
        tableHVAC[j,3] <- 0
        tableHVAC[j,4] <- 0
      }
    }
    
    else if(tableHVAC[j,2] < tempRoomMin){
      
      if(modeHVAC[i] == 1){
        
        tableHVAC[j,3] <- 0
        tableHVAC[j,4] <- 1
      }
      
      else{
        
        tableHVAC[j,3] <- 0
        tableHVAC[j,4] <- 0
      }
    }
    
    else{
      
      tableHVAC[j,3] <- 0
      tableHVAC[j,4] <- 0
    }
  }
  
  tableHVAC <- tableHVAC[order(-tableHVAC[ ,3],-tableHVAC[ ,4]), ]
  
  for(j in 1:groupHVAC){
    
    if(setPowerHVAC[i] > 0){
      
      if(tableHVAC[j,3]+tableHVAC[j,4] == 0){
        
        if(tableHVAC[j,2] > (tempRoomMin+0.5)){
          
          if(modeHVAC[i] == 0){
            
            tableHVAC[j,3] <- 1
            tableHVAC[j,4] <- 0
          }
        }
        
        else if(tableHVAC[j,2] < (tempRoomMax-0.5)){
          
          if(modeHVAC[i] == 1){
            
            tableHVAC[j,3] <- 0
            tableHVAC[j,4] <- 1
          }
        }
      }
    }
    
    setPowerHVAC[i] <- setPowerHVAC[i]-((tableHVAC[j,3]+tableHVAC[j,4])*((powerHVAC*(numberHVAC/groupHVAC))/1000))
    outputPowerHVAC[i] <- outputPowerHVAC[i]+((tableHVAC[j,3]+tableHVAC[j,4])*((powerHVAC*(numberHVAC/groupHVAC))/1000))
  }
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  tempRoom[i, ] <- as.numeric(tableHVAC[ ,2])
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+((tableHVAC[j,4]*heatingCoP-tableHVAC[j,3]*coolingCoP)*powerHVAC))*(600/thermCap))
  }
  
  tableHVAC <- tableHVAC[order(-tableHVAC[ ,3],-tableHVAC[ ,4]), ]
  
  #----
  #BESS
  
  #Setting power
  
  setPowerBESS[i] <- (lineCapacity*(1-lineSafetyMargin))-(grid[i]-outputPowerHVAC[i])
  
  #Operation
  
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
    
    resultHVAC <- cbind(timeFinal, tempRoom, outputPowerHVAC)
    colnames(resultHVAC) <- c("time", 1:groupHVAC, "output")
    
    resultBESS <- cbind(timeFinal, SOC, outputPowerBESS)
    colnames(resultBESS) <- c("time", "SOC", "output")
  }
}

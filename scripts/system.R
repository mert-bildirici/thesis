#----

library(ggplot2)
library(reshape2)

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
    
    setPowerHVAC_remain <- numeric(length(grid))
    
    outputPowerHVAC2 <- numeric(length(grid))
    
    modeChange <- 0
    
    
    tableBESS <- data.frame(SOC=maxSOC, modeDischarging=logical(1), modeCharging=logical(1))
    
    SOC <- numeric(length(grid))
    
    setPowerBESS <- numeric(length(grid))
    
    outputPowerBESS <- numeric(length(grid))
  }
  
  #----
  #HVAC

  tempRoom2[i, ] <- as.numeric(tableHVAC2[ ,2])
  
  if(i != 1){
    
    if(modeHVAC[i] == modeHVAC[i-1]){
      
      modeChange <- 0
    }
    
    else{
      
      modeChange <- 1
    }
  }
  
  if(modeChange == 1){
    
    tableHVAC2[ ,3] <- 0
    tableHVAC2[ ,4] <- 0
  }

  k <- 0
  l <- 0
  
  #cooling mode
  
  if(modeHVAC[i] == 0){
    
    #limit check
    
    tableHVAC2 <- tableHVAC2[order(-tableHVAC2[ ,3]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC2[j,3] == 1){
        
        k <- k+1
      }
      
      else{
        
        break
      }
    }
    
    if(k == 0){
      
      tableHVAC2_OFF <- as.data.frame(tableHVAC2)
      
      tableHVAC2_OFF <- tableHVAC2_OFF[order(-tableHVAC2[ ,2]), ]
      
      for(j in 1:groupHVAC){
        
        if(tableHVAC2_OFF[j,2] > tempRoomMax){
          
          tableHVAC2_OFF[j,3] <- 1
          
        }
        
        else{
          
          break
        }
      }
      
      tableHVAC2 <- tableHVAC2_OFF
    }
    
    else if(k == groupHVAC){
      
      tableHVAC2_ON <- as.data.frame(tableHVAC2)
      
      tableHVAC2_ON <- tableHVAC2_ON[order(tableHVAC2[ ,2]), ]
      
      for(j in 1:groupHVAC){
        
        if(tableHVAC2_ON[j,2] < tempRoomMin){
          
          tableHVAC2_ON[j,3] <- 0
        }
        
        else{
        
          break
        }
      }
      
      tableHVAC2 <- tableHVAC2_ON
    }
    
    else{
      
      tableHVAC2_ON <- as.data.frame(tableHVAC2[1:k, ])
      
      tableHVAC2_ON <- tableHVAC2_ON[order(tableHVAC2[ ,2]), ]
      
      tableHVAC2_OFF <- as.data.frame(tableHVAC2[(k+1):groupHVAC, ])
      
      tableHVAC2_OFF <- tableHVAC2_OFF[order(-tableHVAC2[ ,2]), ]
      
      for(j in 0:k){
        
        if(tableHVAC2_ON[j,2] < tempRoomMin){
          
          tableHVAC2_ON[j,3] <- 0
        }
        
        else{
          
          break
        }
      }
      
      for(j in 0:(groupHVAC-k)){
        
        if(tableHVAC2_OFF[j,2] > tempRoomMax){
          
          tableHVAC2_OFF[j,3] <- 1
        }
        
        else{
          
          break
        }
      }
      
      tableHVAC2 <- rbind(tableHVAC2_ON, tableHVAC2_OFF)
    }
  }
  
  #heating mode
  
  #limit check
  
  if(modeHVAC[i] == 1){
    
    tableHVAC2 <- tableHVAC2[order(-tableHVAC2[ ,4]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC2[j,4] == 1){
        
        k <- k+1
      }
      
      else{
        
        break
      }
    }
    
    if(k == 0){
      
      tableHVAC2_OFF <- as.data.frame(tableHVAC2)
      
      tableHVAC2_OFF <- tableHVAC2_OFF[order(tableHVAC2[ ,2]), ]
      
      for(j in 1:groupHVAC){
        
        if(tableHVAC2_OFF[j,2] < tempRoomMin){
          
          tableHVAC2_OFF[j,4] <- 1
        }
        
        else{
          
          break
        }
      }
      
      tableHVAC2 <- tableHVAC2_OFF
    }
    
    else if(k == groupHVAC){
      
      tableHVAC2_ON <- as.data.frame(tableHVAC2)
      
      tableHVAC2_ON <- tableHVAC2_ON[order(-tableHVAC2[ ,2]), ]
      
      for(j in 1:groupHVAC){
        
        if(tableHVAC2_ON[j,2] > tempRoomMax){
          
          tableHVAC2_ON[j,4] <- 0
        }
        
        else{
          
          break
        }
      }
      
      tableHVAC2 <- tableHVAC2_ON
    }
    
    else{
      
      tableHVAC2_ON <- as.data.frame(tableHVAC2[1:k, ])
      
      tableHVAC2_ON <- tableHVAC2_ON[order(-tableHVAC2[ ,2]), ]
      
      tableHVAC2_OFF <- as.data.frame(tableHVAC2[((k+1):groupHVAC), ])
      
      tableHVAC2_OFF <- tableHVAC2_OFF[order(tableHVAC2[ ,2]), ]
      
      for(j in 1:k){
        
        if(tableHVAC2_ON[j,2] > tempRoomMax){
          
          tableHVAC2_ON[j,4] <- 0
        }
        
        else{
          
          break
        }
      }
      
      for(j in 1:(groupHVAC-k)){
        
        if(tableHVAC2_OFF[j,2] < tempRoomMin){
          
          tableHVAC2_OFF[j,4] <- 1
        }
        
        else{
          
          break
        }
      }
      
      tableHVAC2 <- rbind(tableHVAC2_ON, tableHVAC2_OFF)
    }
  }

  
  tableHVAC2 <- tableHVAC2[order(tableHVAC2[ ,1]), ]
  
  
  for(j in 1:groupHVAC){
    
    tableHVAC2[j,2] <- tableHVAC2[j,2]+((((tempOut[i]-tableHVAC2[j,2])/thermRes)+(tableHVAC2[j,4]*heatingCoP*heatingPower-tableHVAC2[j,3]*coolingCoP*coolingPower))*(600/thermCap))
    
    outputPowerHVAC2[i] <- outputPowerHVAC2[i]+((tableHVAC2[j,4]*heatingPower+tableHVAC2[j,3]*coolingPower)*((numberHVAC/groupHVAC)/1000))
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
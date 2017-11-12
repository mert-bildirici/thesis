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
    
    outputPowerHVAC_before <- numeric(length(grid))
  }
  
  
  #HVAC control----
  
  tempRoom[i, ] <- as.numeric(tableHVAC[ ,2])
  
  #cooling mode----
  
  if(modeHVAC[i] == 0){

    tableHVAC[ ,4] <- 0
    
    tableHVAC <- tableHVAC[order(-tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] >= tempRoomMax){
        
        tableHVAC[j,3] <- 1
      }
      
      else  if((tableHVAC[j,2] < tempRoomMax) && (tableHVAC[j,2] > tempRoomMin)){
        
        tableHVAC[j,3] <- tableHVAC[j,3]
      }
      
      else{
        
        tableHVAC[j,3] <- 0
      }
    }
  }
  
  #heating mode----
  
  else if(modeHVAC[i] == 1){
    
    tableHVAC[ ,3] <- 0
    
    tableHVAC <- tableHVAC[order(tableHVAC[ ,2]), ]
    
    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,2] <= tempRoomMin){
        
        tableHVAC[j,4] <- 1
      }
      
      else  if((tableHVAC[j,2] > tempRoomMin) && (tableHVAC[j,2] < tempRoomMax)){
        
        tableHVAC[j,4] <- tableHVAC[j,4]
      }
      
      else{
        
        tableHVAC[j,4] <- 0
      }
    }
  }
  
  #----
  #step calculations for HVAC
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  outputPowerHVAC_before[i] <- (sum(tableHVAC[ ,3])*(coolingPower/1000)+sum(tableHVAC[ ,4])*(heatingPower/1000))*(numberHVAC/groupHVAC)
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+(tableHVAC[j,4]*heatingCoP*heatingPower-tableHVAC[j,3]*coolingCoP*coolingPower))*(600/thermCap)) 
  }

  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
  
  #finalization
  
  if(i == length(grid)){
    
    resultHVAC_before <- data.frame(timeFinal, tempRoom, outputPowerHVAC_before)
    colnames(resultHVAC_before) <- c("time", 1:groupHVAC, "power")
  }
}

resultGeneration_before <- data.frame(timeFinal, generation)
colnames(resultGeneration_before) <- c("time", "power")

resultLoad_before <- data.frame(timeFinal, load)
colnames(resultLoad_before) <- c("time", "power")

resultGrid_before <- data.frame(timeFinal, resultGeneration_before$power-resultLoad_before$power)
colnames(resultGrid_before) <- c("time", "power")

meltedResultHVAC_before <- melt(subset(resultHVAC_before, select=c(-power)), id.var="time")
colnames(meltedResultHVAC_before) <- c("time", "group", "tempRoom")


#graphs

ggplot(resultGrid_before, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(resultHVAC_before, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month")

ggplot(meltedResultHVAC_before, aes(x=time, y=tempRoom)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=group)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(breaks=seq(tempRoomWanted-(tempWindow/2), tempRoomWanted+(tempWindow/2), 0.5))

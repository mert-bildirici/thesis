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
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeHVAC=logical(groupHVAC), heatingON=logical(groupHVAC), coolingON=logical(groupHVAC), counter=integer(groupHVAC))
    
    resultHVAC_before <- data.frame(power=numeric(length(grid)), temperature=matrix(nrow=length(grid), ncol=groupHVAC))
  }
  
  #HVAC control----
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,3], tableHVAC[ ,2]), ]
  
  for(j in 1:groupHVAC){
    
    if(tableHVAC[j,3] == 0){
      
      tableHVAC[j,5] <- 0
      
      if(tableHVAC[j,2] <= tempRoomMin){
        
        tableHVAC[j,4] <- 1
      }
      
      else if(tableHVAC[j,2] >= tempRoomMax-0.2){
        
        tableHVAC[j,4] <- 0
      }
      
      
      if(tableHVAC[j,2] >= tempRoomMax+0.2){
        
        tableHVAC[j,3] <- 1
      }
    }
    
    
    else if(tableHVAC[j,3] == 1){
      
      tableHVAC[j,4] <- 0
      
      if(tableHVAC[j,2] >= tempRoomMax){
        
        tableHVAC[j,5] <- 1
      }
      
      else if(tableHVAC[j,2] <= tempRoomMin+0.2){
        
        tableHVAC[j,5] <- 0
      }
      
      
      if(tableHVAC[j,2] <= tempRoomMin-0.2){
        
        tableHVAC[j,3] <- 0
      }
    }
  }
  
  #----
  #step operations for HVAC
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  resultHVAC_before[i,1] <- (sum(tableHVAC[ ,4])*(heatingPower/1000)+sum(tableHVAC[ ,5])*(coolingPower/1000))*(numberHVAC/groupHVAC)
  
  resultHVAC_before[i,2:ncol(resultHVAC_before)] <- tableHVAC[ ,2]
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+(tableHVAC[j,4]*heatingCoP*heatingPower-tableHVAC[j,5]*coolingCoP*coolingPower))*(600/thermCap)) 
  }
  
  #finalization
  
  if(i == length(grid)){
    
    resultHVAC_before <- cbind(time=timeFinal, resultHVAC_before)
  }
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
}

resultGeneration_before <- data.frame(timeFinal, generation)
colnames(resultGeneration_before) <- c("time", "power")

resultLoad_before <- data.frame(timeFinal, load)
colnames(resultLoad_before) <- c("time", "power")

resultGrid_before <- data.frame(timeFinal, resultGeneration_before$power-resultLoad_before$power)
colnames(resultGrid_before) <- c("time", "power")

meltedResultHVAC_before <- melt(subset(resultHVAC_before, select=c(-power)), id.var="time")
colnames(meltedResultHVAC_before) <- c("time", "group", "temperature")


#graphs

ggplot(resultGrid_before, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(-190, 190), breaks=seq(-200, 200, 40))

ggplot(resultHVAC_before, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(0, 20), breaks=seq(0, 20, 5))

ggplot(meltedResultHVAC_before, aes(x=time, y=temperature)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=group)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(20, 26), breaks=seq(20, 26, 1))

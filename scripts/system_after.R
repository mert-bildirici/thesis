#----

library(ggplot2)
library(reshape2)

#----

for(i in 1:length(grid)){
  
  #----
  #initialization
  
  if(i == 1){
    
    tableHVAC <- data.frame(group=1:groupHVAC, tempRoom=tempRoomWanted, modeHVAC=logical(groupHVAC), heatingON=logical(groupHVAC), coolingON=logical(groupHVAC), counter=integer(groupHVAC))
    
    resultHVAC_after <- data.frame(power=numeric(length(grid)), temperature=matrix(nrow=length(grid), ncol=groupHVAC))
  }
  
  #HVAC control----
  
  if(resultGrid_before$power[i]+resultHVAC_before$power[i] > lineCapacity*(1-lineSafetyMargin)){

    controlHVAC_ON <- ceiling((resultGrid_before$power[i]+resultHVAC_before$power[i])-(lineCapacity*(1-lineSafetyMargin))/((numberHVAC/groupHVAC)*(1.85/1000)))
    controlHVAC_OFF <- 0
  }

  else if(resultGrid_before$power[i]+resultHVAC_before$power[i]-(numberHVAC*(1.85/1000)) < -(lineCapacity*(1-lineSafetyMargin))){

    controlHVAC_ON <- 0
    controlHVAC_OFF <- ceiling(abs((resultGrid_before$power[i]+resultHVAC_before$power[i]-(numberHVAC*(1.85/1000)))+(lineCapacity*(1-lineSafetyMargin)))/((numberHVAC/groupHVAC)*(1.85/1000)))
  }

  else{

    controlHVAC_ON <- 0
    controlHVAC_OFF <- 0
  }

  tableHVAC_heating <- tableHVAC[order(tableHVAC[ ,2]), ]
  tableHVAC_cooling <- tableHVAC[order(-tableHVAC[ ,2]), ]
  
  tableHVAC <- rbind(tableHVAC_heating[which(tableHVAC_heating[ ,3]==0), ], tableHVAC_cooling[which(tableHVAC_cooling[ ,3]==1), ])
  

  if(controlHVAC_ON > 0){  

    for(j in 1:groupHVAC){
      
      if(tableHVAC[j,3] == 0){
        
        tableHVAC[j,5] <- 0
        
        if(tableHVAC[j,2] <= tempRoomMin){
          
          tableHVAC[j,4] <- 1
          
          controlHVAC_ON <- controlHVAC_ON-1
        }
        
        else if(tableHVAC[j,2] >= tempRoomMax-0.2){
          
          tableHVAC[j,4] <- 0
        }
        
        else{
          
          if(controlHVAC_ON > 0){
            
            tableHVAC[j,4] <- 1
            
            controlHVAC_ON <- controlHVAC_ON-1
          }
        }
        
        
        if(tableHVAC[j,2] >= tempRoomMax+0.2){
          
          tableHVAC[j,3] <- 1
        }
      }
      
      
      else if(tableHVAC[j,3] == 1){
        
        tableHVAC[j,4] <- 0
        
        if(tableHVAC[j,2] >= tempRoomMax){
          
          tableHVAC[j,5] <- 1
          
          controlHVAC_ON <- controlHVAC_ON-1
        }
        
        else if(tableHVAC[j,2] <= tempRoomMin+0.2){
          
          tableHVAC[j,5] <- 0
        }
        
        else{
          
          if(controlHVAC_ON > 0){
            
            tableHVAC[j,5] <- 1
            
            controlHVAC_ON <- controlHVAC_ON-1
          }
        }
        
        
        if(tableHVAC[j,2] <= tempRoomMin-0.2){
          
          tableHVAC[j,3] <- 0
        }
      }
    }
  }
  
  
  else if(controlHVAC_OFF > 0){  
    
    for(j in groupHVAC:1){
      
      if(tableHVAC[j,3] == 0){
        
        tableHVAC[j,5] <- 0
        
        if(tableHVAC[j,2] >= tempRoomMax-0.2){
          
          tableHVAC[j,4] <- 0
          
          controlHVAC_OFF <- controlHVAC_OFF-1
        }
        
        else if(tableHVAC[j,2] <= tempRoomMin){
          
          tableHVAC[j,4] <- 1
        }
        
        else{
          
          if(controlHVAC_OFF > 0){
            
            tableHVAC[j,4] <- 0
            
            controlHVAC_OFF <- controlHVAC_OFF-1
          }
        }
        
        
        if(tableHVAC[j,2] >= tempRoomMax+0.2){
          
          tableHVAC[j,3] <- 1
        }
      }
      
      
      else if(tableHVAC[j,3] == 1){
        
        tableHVAC[j,4] <- 0
        
        if(tableHVAC[j,2] <= tempRoomMin+0.2){
          
          tableHVAC[j,5] <- 0
          
          controlHVAC_OFF <- controlHVAC_OFF-1
        }
        
        else if(tableHVAC[j,2] >= tempRoomMax){
          
          tableHVAC[j,5] <- 1
        }
        
        else{
          
          if(controlHVAC_OFF > 0){
            
            tableHVAC[j,5] <- 0
            
            controlHVAC_OFF <- controlHVAC_OFF-1
          }
        }
        
        
        if(tableHVAC[j,2] <= tempRoomMin-0.2){
          
          tableHVAC[j,3] <- 0
        }
      }
    }
  }
  
  else{
    
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
  }
  
 
  #step operations for HVAC----
  
  tableHVAC <- tableHVAC[order(tableHVAC[ ,1]), ]
  
  resultHVAC_after[i,1] <- (sum(tableHVAC[ ,4])*(heatingPower/1000)+sum(tableHVAC[ ,5])*(coolingPower/1000))*(numberHVAC/groupHVAC)
  
  resultHVAC_after[i,2:ncol(resultHVAC_after)] <- tableHVAC[ ,2]
  
  for(j in 1:groupHVAC){
    
    tableHVAC[j,2] <- tableHVAC[j,2]+((((tempOut[i]-tableHVAC[j,2])/thermRes)+(tableHVAC[j,4]*heatingCoP*heatingPower-tableHVAC[j,5]*coolingCoP*coolingPower))*(600/thermCap)) 
  }
   
  #finalization
  
  if(i == length(grid)){
    
    resultHVAC_after <- cbind(time=timeFinal, resultHVAC_after)
  }
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
}


for(i in 1:length(grid)){
  
  if(i == 1){
    
    generationSet <- numeric(length(grid)/6)
    
    generationSum <- 0
  }
  
  generationSum <- generationSum + generation[i]
  
  if(i%%6 == 0){
    
    generationSet[i/6] <- generationSum/6
    
    generationSum <- 0
  }
  
  if(i == length(grid)){
    
    generationSet <- rep(generationSet, each = 6)
  }
}


for(i in 1:length(grid)){
  
  if(i == 1){
    
    tableBESS <- data.frame(group=1, SoC=numeric(1), charging=logical(1), discharging=logical(1))
    
    resultBESS_after <- data.frame(power=numeric(length(grid)), SoC=numeric(length(grid)))
    
    setPowerBESS <- numeric(length(grid))
    
    tableBESS[1,2] <- 1.0
  }
  
  
  if(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] < -lineCapacity*(1-lineSafetyMargin)){

    setPowerBESS[i] <- -(lineCapacity*(1-lineSafetyMargin))-(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i])
    
    socMin <- 0.1
    socMax <- 1.0
  }  
  
  else if(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] > lineCapacity*(1-lineSafetyMargin)){

    setPowerBESS[i] <- (lineCapacity*(1-lineSafetyMargin))-(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i])
    
    socMin <- 0.1
    socMax <- 1.0
  } 
  
  else if((resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] < 0.9*lineCapacity*(1-lineSafetyMargin)) && (resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] > 0.9*-lineCapacity*(1-lineSafetyMargin))){
    
    socMin <- 0.3
    socMax <- 0.9
    
    setPowerBESS[i] <- generationSet[i]-generation[i]
  }
  
  else{
    
    socMin <- 0.3
    socMax <- 0.9
    
    if(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] >= 0.9*lineCapacity*(1-lineSafetyMargin)){

      setPowerBESS[i] <- (lineCapacity*(1-lineSafetyMargin))-(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i])
    }
    
    else if(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i] <= 0.9*-lineCapacity*(1-lineSafetyMargin)){
      
      setPowerBESS[i] <- -(lineCapacity*(1-lineSafetyMargin))-(resultGrid_before$power[i]+resultHVAC_before$power[i]-resultHVAC_after$power[i])
    }
  }


  if(setPowerBESS[i] > 0){
    
    tableBESS[1,3] <- 0
    tableBESS[1,4] <- 1
    
    if(tableBESS[1,2] > socMin){
      
      outputPowerBESS <- ((tableBESS[1,2]-socMin)*energyBESS)*6
      
      if(outputPowerBESS > powerBESS){
        
        outputPowerBESS <- powerBESS
      }
      
      if(outputPowerBESS > setPowerBESS[i]/dschEff){
        
        outputPowerBESS <- setPowerBESS[i]/dschEff
      }
      
      outputPowerBESS <- outputPowerBESS*dschEff
    }
    
    else{
      
      outputPowerBESS <- 0
    }
  }
  
  else if(setPowerBESS[i] < 0){
    
    tableBESS[1,3] <- 1
    tableBESS[1,4] <- 0
    
    if(tableBESS[1,2] < socMax){
      
      outputPowerBESS <- ((tableBESS[1,2]-socMax)*energyBESS)*6
      
      if(outputPowerBESS < -powerBESS){
        
        outputPowerBESS <- -powerBESS
      }
      
      if(outputPowerBESS < setPowerBESS[i]*chEff){
        
        outputPowerBESS <- setPowerBESS[i]*chEff
      }
      
      outputPowerBESS <- outputPowerBESS/chEff
    }
    
    else{
      
      outputPowerBESS <- 0
    }
  }
  
  else{
    
    tableBESS[1,3] <- 0
    tableBESS[1,4] <- 0
    
    outputPowerBESS <- 0
  }

  #step operations for BESS----
  
  resultBESS_after[i,1] <- outputPowerBESS
  
  resultBESS_after[i,2] <- tableBESS[1,2]
  
  tableBESS[1,2] <- tableBESS[1,2]-((((tableBESS[1,4]/dschEff)+(tableBESS[1,3]*chEff))*(outputPowerBESS/6))/energyBESS)
  
  #finalization
  
  if(i == length(grid)){
    
    resultBESS_after <- cbind(time=timeFinal, resultBESS_after)
  }
  
  #day count
  
  if(i%%144 == 0){
    
    print(i/144)
  }
}

resultGeneration_after <- data.frame(timeFinal, generation+resultBESS_after$power)
colnames(resultGeneration_after) <- c("time", "power")

resultLoad_after <- data.frame(timeFinal, load-resultHVAC_before$power+resultHVAC_after$power)
colnames(resultLoad_after) <- c("time", "power")

resultGrid_after <- data.frame(timeFinal, resultGeneration_after$power-resultLoad_after$power)
colnames(resultGrid_after) <- c("time", "power")

meltedResultHVAC_after <- melt(subset(resultHVAC_after, select=c(-power)), id.var="time")
colnames(meltedResultHVAC_after) <- c("time", "group", "temperature")

resultGeneration <- cbind(resultGeneration_before[1:36,], resultGeneration_after[1:36,])
resultGeneration <- resultGeneration[,-c(3)]
colnames(resultGeneration) <- c("time","before complementary usage","after complementary usage")

meltedGeneration <- melt(resultGeneration, id="time")
colnames(meltedGeneration) <- c("time", "group", "power")

#graphs----

ggplot(resultGrid_after, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  geom_hline(yintercept=lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  geom_hline(yintercept=-lineCapacity*(1-lineSafetyMargin), size=0.1, color="red") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(-190, 190), breaks=seq(-200, 200, 40))

ggplot(resultGeneration_before[1:72,], aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=1, color="blue") +
  geom_area(fill="#0000FF", alpha=I(0.5)) +
  labs(x="time (hour)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%H:%M") +
  scale_y_continuous(limits=c(0, 170), breaks=seq(-200, 200, 40))

ggplot(resultGeneration_after[1:72,], aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=1, color="red") +
  geom_area(fill="#FF0000", alpha=I(0.5)) +
  labs(x="time (hour)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%H:%M") +
  scale_y_continuous(limits=c(0, 170), breaks=seq(-200, 200, 40))

ggplot(meltedGeneration, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size = 1.5, aes(color=group)) +
  theme(legend.title=element_blank(),legend.position="top") +
  labs(x="time (hour)", y="power (MW)") +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%H:%M", date_breaks = "1 hour") +
  scale_y_continuous(limits=c(0, 200), breaks=seq(-200, 200, 40))

ggplot(resultHVAC_after, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(0, 25), breaks=seq(0, 25, 5))

ggplot(meltedResultHVAC_after, aes(x=time, y=temperature)) +
  theme_bw() +
  geom_line(size = 0.1, aes(color=group)) +
  theme(legend.position="none") +
  labs(x="time (month)", y=expression(room~temperature~(''^o~C))) +
  theme(text=element_text(size = 20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(21.5, 24.5), breaks=seq(22, 24, 1))

ggplot(resultBESS_after[1:36,], aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=1, color="#56B1F7") +
  geom_hline(yintercept=0, size=1, color="darkred") +
  labs(x="time (hour)", y="Power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%H:%M", date_breaks="1 hour") +
  scale_y_continuous(limits=c(-9,9), breaks=seq(-9,9,1))

ggplot(resultBESS_after[1:36,], aes(x=time, y=SoC)) +
  theme_bw() +
  geom_line(size=0.5, color="#56B1F7") +
  labs(x="time (hour)", y="SoC") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%H:%M", date_breaks="1 hour") +
  scale_y_continuous(limits=c(0.3,0.9), breaks=seq(0.3,0.9,0.1))


ggplot(resultBESS_after, aes(x=time, y=power)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="Power (MW)") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(-4.5,4.5), breaks=seq(-12,12,1))

ggplot(resultBESS_after, aes(x=time, y=SoC)) +
  theme_bw() +
  geom_line(size=0.1, color="#56B1F7") +
  labs(x="time (month)", y="SoC") +
  theme(text=element_text(size=20)) +
  scale_x_datetime(date_labels="%m", date_breaks="1 month") +
  scale_y_continuous(limits=c(0.1,1), breaks=seq(0,1,0.1))


#input

numberHVAC <- 1000

powerHVAC <- 1

heatingCoP <- 4
coolingCoP <- 5

thermCap <- 9200
thermRes <- 50

setPower <-

tempOut <- rep(10, length(setPower))

tempRoomWanted <- 23
tempWindow <- 2

tempRoomMin <- tempRoomWanted-(tempWindow/2)
tempRoomMax <- tempRoomWanted+(tempWindow/2)
tempRoomInitial <- tempRoomWanted

#information of HVACs for one time step

tableHVAC <- data.frame(id=1:numberHVAC, tempRoom=tempRoomWanted, uHeating=logical(length=numberHVAC), uCooling=logical(length=numberHVAC))

resultHVAC <- data.frame(step=integer(0), id=integer(0), tempRoom=numeric(0))

#simulation

for(j in 1:1008){

  setPowerRemain <- setPower[j]
  
  for(i in 1:numberHVAC){
    
    if(tableHVAC[i,2] < tempRoomMin){
      
      tableHVAC[i,3] <- 1
      tableHVAC[i,4] <- 0
    }
    
    else if(tableHVAC[i,2] > tempRoomMax){
      
      tableHVAC[i,3] <- 0
      tableHVAC[i,4] <- 1
    }
    
    else{
      
      tableHVAC[i,3] <- 0
      tableHVAC[i,4] <- 0
    }
  }
  
  tableHVAC <- tableHVAC[order(-tableHVAC$uHeating, -tableHVAC$uCooling), ]
  
  for(i in 1:numberHVAC){
    
    if((tableHVAC[i,3]+tableHVAC[i,4]) == 1){
      
      setPowerRemain <- setPowerRemain-(powerHVAC/1000)
    }
    
    else{
      
      if(setPowerRemain > 0){
        
        if(tableHVAC[i,2] < tempRoomWanted){
          
          tableHVAC[i,3] <- 1
          tableHVAC[i,4] <- 0
        }
        
        else{
          
          tableHVAC[i,3] <- 0
          tableHVAC[i,4] <- 1
        }
        
        setPowerRemain <- setPowerRemain-(powerHVAC/1000)
      }
    }
    
    tableHVAC[i,2] <- tableHVAC[i,2]+((((tempOut[j]-tableHVAC[i,2])/thermRes)+((tableHVAC[i,3]*heatingCoP-tableHVAC[i,4]*coolingCoP)*powerHVAC))*(600/thermCap))   
  }

  P_out[j] <-  setPower[j]-setPowerRemain
  
  resultHVAC <- rbind(resultHVAC, data.frame(step=j, id=tableHVAC[ ,1], tempRoom=tableHVAC[ ,2]))
  
  if(j %% 1008 ==0){
    
    print(j/1008)
  }
}

resultHVAC <- resultHVAC[order(resultHVAC$id), ]

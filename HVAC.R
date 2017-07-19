#datas

numberHVAC <- 50

powerHVAC <- 0.003

setPower <- x

tempOut <- rep(10, length(setPower))

#information of HVACs for one time step

tableHVAC <- data.frame(id=1:numberHVAC, tempRoom=22, state=integer(length=numberHVAC), control=integer(length=numberHVAC))

resultHVAC <- data.frame(step=integer(0), id=integer(0), tempRoom=numeric(0), state=integer(0))

#simulation

for(j in 1:length(tempOut)){

  setPowerRemain <- setPower[j]
  
  for(i in 1:numberHVAC){

    tableHVAC[i,4] <- tempCheck(tableHVAC[i,2])
  }
  
  tableHVAC <- tableHVAC[order(-tableHVAC$control, -tableHVAC$state), ]
  
  for(i in 1:numberHVAC){
    
    if(tableHVAC[i,4] == 1){
      
      tableHVAC[i,3] = 1
      
      setPowerRemain = setPowerRemain - powerHVAC
    }
    
    else if(tableHVAC[i,4] == 2){
      
      tableHVAC[i,3] = 2
      
      setPowerRemain = setPowerRemain - powerHVAC
    }
    
    else if(tableHVAC[i,4] == 0){
      
      if(setPowerRemain > 0){
        
        if(tempOut < 22){
          
          tableHVAC[i,3] = 1
        }
        
        else{
          
          tableHVAC[i,3] = 2
        }
        
        setPowerRemain = setPowerRemain - powerHVAC
      }
      
      else{
        
        tableHVAC[i,3] = 0
      }
    }
    
    tableHVAC[i,2] <- tempDynamics(tempOut[j], tableHVAC[i,2], tableHVAC[i,3])
  }
  
  P_out[j] <-  setPower[j]-setPowerRemain
  
  resultHVAC <- rbind(resultHVAC, data.frame(step=j, id=tableHVAC[ ,1], tempRoom=tableHVAC[ ,2], state=tableHVAC[ ,3]))
  
  if(j %% 1008 ==0){
    
    print(j/1008)
  }
}

resultHVAC <- resultHVAC[order(resultHVAC$id), ]

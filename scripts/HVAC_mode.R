#----

modeHVAC <- logical()

tempOutSum <- 0

for(i in 1:52704){
  
  tempOutSum <- tempOutSum+tempOut[i]
  
  if(i%%6 == 0){
    
    if((tempOutSum/6) > tempRoomWanted){
      
      modeHVAC[i/6] <- 0
    }
    
    else{
      
      modeHVAC[i/6] <- 1
    }
    
    tempOutSum <- 0
  }
}

modeHVAC <- rep(modeHVAC, each=6)

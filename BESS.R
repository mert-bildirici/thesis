#BESS model

#time

time <- data_2$time

#power difference

P.diff <- data_2$`power difference (MW)`

#open loop window lenght as second

window_length <- 10*60

#power capacity as MW

P <- 100

#energy capacity as MWh

C <- 10000

##Initial energy capacity

C.init <- 0.4 * C

#SOC limits

SOC.ul <- 0.3
SOC.ol <- 1

###

P.bess <- numeric(length = nrow(data_2))

for(i in 1:nrow(data_2)){

  if(i == 1){
    
    SOC <- C.init / C
    
    if(P.diff[i] > P){
      
      P.bess[i] <- P
      
    }
    
    else if(P.diff[i] < -P){
      
      P.bess[i] <- -P
      
    }
    
    else{
      
      P.bess[i] <- P.diff[i]
      
    }
    
  }
  
  else{
    
    SOC <- (C.init + (P.bess[i-1] / (window_length / 3600))) / C    
    
    if(SOC < SOC.ul){
      
      if(P.diff[i] > 0){
        
        if(P.diff[i] > P){
          
          P.bess[i] <- P
          
        }
        
        else{
          
          P.bess[i] <- P.diff[i]
          
        }
        
      }
      
      else{
        
        P.bess[i] <- 0
        
      }
      
    }
    
    else if(SOC > SOC.ol){
      
      if(P.diff[i] < 0){
        
        if(P.diff[i] < -P){
          
          P.bess[i] <- -P
          
        }
        
        else{
          
          P.bess[i] <- P.diff[i]
          
        }
        
      }
      
      else{
        
        P.bess[i] <- 0
        
      }
      
    }
    
    else{
      
      if(P.diff[i] > P){
        
        P.bess[i] <- P
        
      }
      
      else if(P.diff[i] < -P){
        
        P.bess[i] <- -P
        
      }
      
      else{
        
        P.bess[i] <- P.diff[i]
      
      }
        
    }
    
  }  
  
}

qplot(time,
      P.bess, 
      geom = "point",
      col = I("black"),
      main = "battery power vs time", 
      xlab = "Date", 
      ylab = "Power (MW)")

qplot(time,
      P.diff-P.bess, 
      geom = "point",
      col = I("black"),
      main = "extra power vs time", 
      xlab = "Date", 
      ylab = "Power (MW)")


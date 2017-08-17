library(ggplot2)
library(reshape2)

SoC <- numeric()
SoC[1] <- 1

for(i in 1:length(gridPower)){
  
  if((gridPower[i] <= (lineCapacity*(1-lineSafetyMargin))) && (grid[i] >= -(lineCapacity*(1-lineSafetyMargin)))){
    
    setPowerBESS[i] <- 0 
  }
  
  else if(gridPower[i] > (lineCapacity*(1-lineSafetyMargin))){
    
    setPowerBESS[i] <- -(gridPower[i]-(lineCapacity*(1-lineSafetyMargin)))
    
    if(setPowerBESS[i] < -powerBESS){
      
      setPowerBESS[i] <- -powerBESS
    }
  }
  
  else if(gridPower[i] < -(lineCapacity*(1-lineSafetyMargin))){
    
    setPowerBESS[i] <- -(gridPower[i]-(lineCapacity*(1-lineSafetyMargin)))
    
    if(setPowerBESS[i] > powerBESS){
      
      setPowerBESS[i] <- powerBESS
    }
  }
}

plot(setPowerBESS, type="l")


for(i in 1:length(setPowerBESS)){
  
  if(setPowerBESS[i] >= 0){
    
    outputBESS[i] <- ((SoC[i]-minSoC)*energyBESS)/6
    
    if(setPowerBESS[i] < outputBESS[i]){
      
      outputBESS[i] <- setPowerBESS[i]
    }
  }
  
  else if(setPowerBESS[i] < 0){
    
    outputBESS[i] <- ((SoC[i]-maxSoC)*energyBESS)/6
    
    if(setPowerBESS[i] > outputBESS[i]){
      
      outputBESS[i] <- setPowerBESS[i]
    }
  }
  
  SoC[i+1] <- SoC[i]-((outputBESS[i]/6)/(energyBESS))
}

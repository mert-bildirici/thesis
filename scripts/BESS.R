library(ggplot2)
library(reshape2)

SoC <- numeric()
SoC[1] <- 0.9

setPowerBESS <- numeric(length(gridPower))

for(i in 1:length(gridPower)){
  
  if((gridPower[i] <= (lineCapacity*(1-lineSafetyMargin))) && (grid[i] >= -(lineCapacity*(1-lineSafetyMargin)))){
    
    setPowerBESS[i] <- 0 
  }
  
  else{
    
    setPowerBESS[i] <- -(gridPower[i]-(lineCapacity*(1-lineSafetyMargin)))
  }
}

plot(setPowerBESS, type="l")


outputBESS <- numeric(length=length(setPowerBESS))

for(i in 1:length(setPowerBESS)){
  
  if(setPowerBESS[i] >= 0){
    
    outputBESS[i] <- ((SoC[i]-minSoC)*energyBESS)*6
    
    if(outputBESS[i] > powerBESS){
      
      outputBESS[i] <- powerBESS
    }

    if(setPowerBESS[i] < outputBESS[i]){
      
      outputBESS[i] <- setPowerBESS[i]
    }
  }
  
  else{
    
    outputBESS[i] <- ((SoC[i]-maxSoC)*energyBESS)*6
    
    if(outputBESS[i] < -powerBESS){
      
      outputBESS[i] <- -powerBESS
    }
    
    if(setPowerBESS[i] > outputBESS[i]){
      
      outputBESS[i] <- setPowerBESS[i]
    }
  }
  
  SoC[i+1] <- SoC[i]-((outputBESS[i]/6)/(energyBESS))
}


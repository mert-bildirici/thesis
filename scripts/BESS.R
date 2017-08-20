library(ggplot2)
library(reshape2)

SoC <- numeric()
SoC[1] <- 0.9

setPowerBESS <- numeric(length(gridPower))

for(i in 1:length(gridPower)){
  
  if((generationData$activePower[i] >= (lineCapacity*(1-lineSafetyMargin)))){
    
    setPowerBESS[i] <- -(generationData$activePower[i]-(lineCapacity*(1-lineSafetyMargin)))
  }
  
  else{
    
    setPowerBESS[i] <- powerBESS
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
    
    outputBESS[i] <- outputBESS[i]*dschEf
    
    SoC[i+1] <- SoC[i]-((outputBESS[i]/6)/(energyBESS))
  }
  
  else{
    
    outputBESS[i] <- ((SoC[i]-maxSoC)*energyBESS)*6
    
    if(outputBESS[i] < -powerBESS){
      
      outputBESS[i] <- -powerBESS
    }
    
    if(setPowerBESS[i] > outputBESS[i]){
      
      outputBESS[i] <- setPowerBESS[i]
    }
    
    SoC[i+1] <- SoC[i]-(((outputBESS[i]*chEff)/6)/(energyBESS))
  }
}

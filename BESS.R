library(ggplot2)

#----
#data

data.1 <- data_2

time <- data.1[ ,3]
P_diff <- data.1[ ,5]
data_lenght <- nrow(data.1)

#----
#Initial values

##open loop window lenght (min)

window_length <- 20

a <- window_length / 10

##battery power (MW)

P_bess.l <- 200

##battery energy (MWh)

E_bess.l <- 30000

##battery SOC (%)

SOC_bess.ul <- 0.3
SOC_bess.ol <- 0.9
SOC_bess <- numeric()
SOC_bess[1] <- 0.5

#----
#BESS model

P_bess <- numeric()

for(i in 1:data_lenght){
  
  if((i %% a) == 1){

    if(((SOC_bess[i] < SOC_bess.ul) && (P_diff[i] < 0)) || ((SOC_bess[i] > SOC_bess.ol) && (P_diff[i] > 0))){
      
      P_bess[i] <- 0
      
    }
    
    else{
      
      if(P_diff[i] > P_bess.l){
        
        P_bess[i] <- P_bess.l
        
      }
      
      else if(P_diff[i] < -P_bess.l){
        
        P_bess[i] <- -P_bess.l
        
      }
      
      else{
        
        P_bess[i] <- P_diff[i]
        
      }
      
    }
    
    SOC_bess[i+a] <- SOC_bess[i] + ((P_bess[i] * (window_length / 60)) / E_bess.l)
    
  }

}

for(i in 1:data_lenght){
  
  if(is.na(P_bess[i])){
    
    P_bess[i] <- P_bess[i-1]
    
  }
  
  if(is.na(SOC_bess[i])){
    
    SOC_bess[i] <- SOC_bess[i-1] + ((P_bess[i-1] / 6) / E_bess.l)
    
  }

}



#----
#Graphs

##Battery Power Graph

qplot(time,
      P_bess, 
      geom = "point",
      col = I("black"),
      main = "battery power vs time", 
      xlab = "Date", 
      ylab = "Power (MW)")

##Excess Power Graph

qplot(time,
      P_diff - P_bess, 
      geom = "point",
      col = I("black"),
      main = "excess power vs time", 
      xlab = "Date", 
      ylab = "Power (MW)")

##Battery SOC Graph

qplot(time,
      SOC_bess[1:data_lenght]*100,
      geom = "point",
      col = I("black"),
      main = "SOC vs time", 
      xlab = "Date", 
      ylab = "SOC (%)")


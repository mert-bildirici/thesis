library(ggplot2)

#----
#data

time <- data_2$time
P_diff <- data_2$`power difference (MW)`
data_lenght <- nrow(data_2)

#----
#Initial values

##open loop window lenght (sec)

window_length <- 10*60

##battery power (MW)

P_bess.l <- 100

##battery energy (MWh)

E_bess.l <- 10000

##battery SOC (%)

SOC_bess.ul <- 0.3
SOC_bess.ol <- 0.9
SOC_bess <- numeric()
SOC_bess[1] <- 0.3

#----
#BESS model

P_bess <- numeric()

for(i in 1:data_lenght){
  
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
  
  SOC_bess[i+1] <- SOC_bess[i] + ((P_bess[i] * (window_length / 3600)) / E_bess.l)

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

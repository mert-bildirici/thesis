#----
#data import

P_gen <- data_2$`generation (MW)`

#----
#parameters

SoC <- numeric()

SoC[1] <- 0.3
SoC_min <- 0.3
SoC_max <- 0.95

eff_d <- 0.9
eff_c <- 0.9

self_d <- 0.002

C_max <- 100

E_to_P <- 5

P_set <- 155

P_diff <- P_set - P_gen

#----
#capacity need calculation

exit <- 0

while(exit != 1){

  P_d_max <- C_max / E_to_P
  P_c_max <- C_max / E_to_P
  
  for(i in 1:data_lenght){
    
    if(((SoC[i] > SoC_max) && (P_diff[i] < 0)) || ((SoC[i] < SoC_min) && (P_diff[i] > 0))){
      
      SoC[i+1] <- SoC[i] * (1 - (self_d /144))
      
    }
    
    else{
      
      if(P_diff[i] > 0){
        
        if(P_diff[i] > P_d_max){
          
          SoC[i+1] <- (SoC[i] * (1 - (self_d /144))) - ((P_d_max / 6) / C_max)
          
        }
        
        else{
          
          SoC[i+1] <- (SoC[i] * (1 - (self_d /144))) - ((P_diff[i] / 6) / C_max)
          
        }
        
      }
      
      else{
        
        if(P_diff[i] < -P_c_max){
          
          SoC[i+1] <- (SoC[i] * (1 - (self_d /144))) + (((P_c_max * eff_c) / 6) / C_max)
          
        }
        
        else{
          
          SoC[i+1] <- (SoC[i] * (1 - (self_d /144))) - (((P_diff[i] * eff_c) / 6) / C_max)
          
        }
        
      }
      
    }
    
  }
  
  if(max(SoC) > SoC_max){
    
    C_max <- C_max + 5
    
  }
  
  else{
    
    exit = 1
    
  }
  
}

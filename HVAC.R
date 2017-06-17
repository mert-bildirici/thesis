#temperature function

tempDynamics <- function(T_room, T_out, s, Q, R=50, C=9200){
  
  T_room_next <- T_room+((((T_out-T_room)/R)+(s*Q))*(600/C))
  
  return(T_room_next)
}

#temperature constraints check function

constCheck <- function(T_room_next, T_min=22, T_max=26){
  
  if(T_room_next < T_min){
    
    return(2)
  }
  
  else if(T_room_next > T_max){
    
    return(0)
  }
  
  else{
    
    return(1)
  }
}

#datas

n <- 10
P_set <- P_control/1000

#information of HVACs for one time step

HVAC_data <- data.frame(id=1:n, T_room=24, T_out=15, Q=3, state=logical(length=n), control=numeric(length=n))

result <- data.frame(step = integer(0), id = integer(0), room_temp = numeric(0), state = logical(0))

#simulation

for(j in 1:52704){
  
  HVAC_data[ ,3] <- T_out[j]
  
  P_set_remind <- P_set[j]
  
  for(i in 1:n){
    
    HVAC_data[i,2] <- tempDynamics(HVAC_data[i,2], HVAC_data[i,3], HVAC_data[i,5], HVAC_data[i,4])
    
    HVAC_data[i,6] <- constCheck(HVAC_data[i,2])
  }
  
  HVAC_data <- HVAC_data[order(-HVAC_data$control, -HVAC_data$state), ]
  
  for(i in 1:n){
    
    if(HVAC_data[i,6] == 2){
      
      HVAC_data[i,5] = T 
      
      P_set_remind = P_set_remind - HVAC_data[i,4]
    }
    
    else if(HVAC_data[i,6] == 1 && P_set_remind > 0){
      
      HVAC_data[i,5] = T
      
      P_set_remind = P_set_remind - HVAC_data[i,4]
    }
    
    else{
      
      HVAC_data[i,5] = F
    }
  }
  
  P_out[j] <-  P_set[j] - P_set_remind
  
  result <- rbind(result, data.frame(step=j, id=HVAC_data[ ,1], room_temp=HVAC_data[ ,2], state=HVAC_data[ ,5]))
  
  if(j%%144 == 0){
    
    print(j/144)
  }
}

library(ggplot2)
library(mice)

#missing data percentages
missing_data_g <- sum(is.na(data_1$`generation-1 (MW)`)) / nrow(data_1) * 100
missing_data_l <- sum(is.na(data_1$`load-1 (MW)`)) / nrow(data_1) * 100

#data analysis without imputing missing data
data_1 <- original_data

  ##finding averages
  generation_average_1 <- sum(data_1$`generation-1 (MW)`, na.rm = T) / (nrow(data_1)-sum(is.na(data_1$`generation-1 (MW)`)))
  load_average_1 <- sum(data_1$`load-1 (MW)`, na.rm = T) / (nrow(data_1)-sum(is.na(data_1$`load-1 (MW)`)))

  ##comparison of generation and load
  power_constant_1 <- generation_average_1 / load_average_1
  energy_constant_1 <- sum(data_1$`generation-1 (MW)`, na.rm = T) / sum(data_1$`load-1 (MW)`, na.rm = T)

  ##new load assumption with energy constant
  load_assumption_1 <- energy_constant_1 * data_1$`load-1 (MW)`
  data_1 <- cbind(data_1, "new load (MW)" = load_assumption_1)

  ##power difference after assumption
  difference_1 <- data_1$`generation-1 (MW)` - data_1$`new load (MW)`
  data_1 <- cbind(data_1, "power difference (MW)" = difference_1)

  ##plots
  qplot(c(1:nrow(data_1)), 
        data_1$`generation-1 (MW)`, 
        geom = "point",
        col = I("black"),
        main = "Generation-1", 
        xlab = "Data number", 
        ylab = "Generation (MW)")
  
  qplot(data_1$`generation-1 (MW)`, 
        geom = "histogram",
        binwidth = 10,
        col = I("black"),
        fill = I("black"),
        alpha = I(.5),
        main = "Distribution of Generation-1", 
        xlab = "Generation (MW)", 
        ylab = "Frequency")
  
  qplot(c(1:nrow(data_1)), 
        data_1$`new load (MW)`, 
        geom = "point",
        col = I("black"),
        main = "Assumed Load", 
        xlab = "Data number", 
        ylab = "Assumed Load (MW)")
  
  qplot(data_1$`new load (MW)`,
        geom = "histogram",
        binwidth = 10,
        col = I("black"),
        fill = I("black"),
        alpha = I(.5),
        main = "Distribution of Assumed Load", 
        xlab = "Assumed Load (MW)", 
        ylab = "Frequency")
  
  qplot(c(1:nrow(data_1)), 
        data_1$`power difference (MW)`, 
        geom = "point",
        col = I("black"),
        main = "Power Difference", 
        xlab = "Data number", 
        ylab = "Power Difference (MW)")
  
  qplot(data_1$`power difference (MW)`,
        geom = "histogram",
        binwidth = 10,
        col = I("black"),
        fill = I("black"),
        alpha = I(.5),
        main = "Distribution of Power Difference", 
        xlab = "Power Difference (MW)", 
        ylab = "Frequency")
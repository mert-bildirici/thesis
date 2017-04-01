#without imputing missing data

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

#with imputing missing data

  library(mice)
  
  data_2 <- original_data
  md.pattern(data_2)
  
  ##imputation method?
  temp_data <- mice(data_2, method = "mean")
  data_2 <- complete(temp_data)
  
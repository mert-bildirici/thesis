#----
#transmission-side

generation <- generationData$activePower
load <- (loadData$activePower/4)*0.8

increaseGeneration <- 0.1
increaseLoad <- 0.0

grid <- (generation*(1+increaseGeneration))-(load*(1+increaseLoad))

lineCapacity <- 40
lineSafetyMargin <- 0.01

#----
#HVAC

numberHVAC <- 4000
controlNumberHVAC <- 400

powerHVAC <- 2

heatingCoP <- 4
coolingCoP <- 3.5

thermCap <- 9200
thermRes <- 50

tempOut <- tempSubhourly$temp4

tempRoomWanted <- 22
tempWindow <- 2

tempRoomMin <- tempRoomWanted-(tempWindow/2)
tempRoomMax <- tempRoomWanted+(tempWindow/2)

#----
#BESS

minSoC <- 0.4
maxSoC <- 0.9

powerBESS <- 1

energy2powerRatio <- 2
  
energyBESS <- powerBESS*energy2powerRatio

dschEff <- 0.98

chEff <- 0.98

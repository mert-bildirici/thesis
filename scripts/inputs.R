
#----
#transmission-side

generation <- (generationData$activePower*4)
load <- (loadData$activePower)

increaseGeneration <- 1.25
increaseLoad <- 1


grid <- (generation*(increaseGeneration))-(load*(increaseLoad))

lineCapacity <- 182
lineSafetyMargin <- 0.02

min(grid)
max(grid)

#----
#HVAC

numberHVAC <- 8000
groupHVAC <- 20

heatingPower <- 2.45
coolingPower <- 2.65

heatingCoP <- 3.21
coolingCoP <- 2.64

thermCap <- 9200
thermRes <- 50

tempOut <- tempSubhourly$temp4

tempRoomWanted <- 23
tempWindow <- 4

tempRoomMin <- tempRoomWanted-(tempWindow/2)
tempRoomMax <- tempRoomWanted+(tempWindow/2)

#----
#BESS

minSOC <- 0.25
maxSOC <- 0.95


powerBESS <- 0
energyBESS <- 2.2

dschEff <- 0.93
chEff <- 0.93



powerBESS <- 2.2
energyBESS <- 2.2

dschEff <- 0.93
chEff <- 0.93

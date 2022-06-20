### TREE TALKERS CHECK LAST VERSION ####
###
### BY: Isabel C. Grisales Sanchez, E-MAIL:  igriss00@estudiantes.unileon.es
### DATE: June-17-2022
###
### HELP GUIDE FROM: "An alert system to monitoring of TreeTalkers devices" by Llaria Zorzi, et al., (2021)


#####################################################################################-
# PART 1: DOWNLOAD AND READ DATABASES -----------------------
#####################################################################################-

#' Inputs: Cloud servers link or databases in TXT files
#' Outputs: One database appended to C:/TreeTalkerDB/TT_CloudDB.csv

#' WAYS TO READ AND DOWNLOAD THE DATA:
#'
#' Three options:
#'   (A) indicating the server links and parameters
#'   (B) reading *.txt files from the computer with the data
#'   (C) using the R package 'TreeTalkersCheck' and modifying the parameters

# Only use ONE option and go to PART 2


####
## OPTION A: Indicating the servers links ----
####

rm(list=ls())

# (A.1) Write/modify the following inputs:
server <- c('http://www.naturetalkers.altervista.org/C0210043/ttcloud.txt',
            'http://www.naturetalkers.altervista.org/C0210044/ttcloud.txt') # Server data links for each Cloud
site   <- c('site1','site2') # Call each cloud (site#)
cloud  <- c('C0210043','C0210044') # Serial number of each cloud

# (A.2) Run "DownloadNaturetalkersC" to save the function:
DownloadNaturetalkersA <- function() {
  # Read input parameters
  inpuParameters <- as.data.frame(cbind(server, site, cloud))
  # Create folder on C:/ for the outputs
  outdir <- file.path("C:", "TreeTalkerDB") # crea esta carpeta
  outfile <- file.path("C:", "TreeTalkerDB", "TT_CloudDB.csv") # define la ruta del archivo de salida
  if (file.exists(outfile))
    file.remove(outfile)
  if (isFALSE(dir.exists(outdir))) {
    dir.create(outdir)
  }
  outFileNames <-
    paste0(outdir,
           "/",
           paste0(inpuParameters$cloud, "_", inpuParameters$site),
           ".txt")
  # Download files
  for (i in 1:nrow(inpuParameters))  {
    download.file(inpuParameters$server[i], outFileNames[i])
    file_i <-
      read.table(
        outFileNames[i],
        sep = ";",
        col.names = paste0("V", 1:70),
        fill = T
      )
    file_i <- cbind(file_i, inpuParameters$site[i])
    write.table(
      file_i,
      outfile,
      sep = ";",
      col.names = F,
      append = T
    )
    # Message
    cat("The",
        inpuParameters$server[i],
        "file has been appended to",
        outfile,
        "\n")
  }
}

## (A.3) Run the function: ----
DownloadNaturetalkersA()

#' End Option (A) ----------------------------------------------------------


####
## OPTION B: Having the data bases on *.txt files --------------
####

# (B.1) Input directory with TXT files
setwd("C:/TreeTalkerDB_Muestra") # Path of the TXT files

# (B.2) Run "DownloadNaturetalkers" to save the function:
DownloadNaturetalkersB <- function() {
  # Read input parameters
  filelist <- list.files(pattern = ".*.txt")
  # Path to new folder and files
  outdir <- file.path("C:", "TreeTalkerDB") # crea esta carpeta
  outfile <- file.path("C:", "TreeTalkerDB", "TT_CloudDB.csv") # define la ruta del archivo de salida
  if (file.exists(outfile))
    file.remove(outfile)
  if (isFALSE(dir.exists(outdir))) {
    dir.create(outdir)
  }
  FileNames <- paste0(getwd(),"/",filelist)
  for (i in 1:length(FileNames))  {
    file_i <-
      read.table(
        FileNames[i],
        sep = ";",
        col.names = paste0("V", 1:70),
        fill = T
      )
    file_i <- cbind(file_i, filelist[i])
    write.table(
      file_i,
      outfile,
      sep = ";",
      col.names = F,
      append = T
    )
    cat("The file",
        filelist[i],
        "has been appended to",
        outfile,
        "\n")
  }
}

## (B.3) Run the function: ----
DownloadNaturetalkersB()

#' End Option (B) ----------------------------------------------------------


####
## OPTION C: Using the package to obtain the DB ----
####

rm(list=ls())

# (C.1) Install the package "TreeTalkersCheck"
install.packages("devtools", repos = "http://cran.us.r-project.org")
devtools::install_github("saveriofrancini/TreeTalkersCheck")

# (C.2) Define the parameters in the "TreeTalkersCheck" package installation folder.
#' Follow the steps:
#'    1. Search for folder "parameters" in R installation folder:
#'       C:/.../R/win-library/4.1/TreeTalkersCheck/parameters
#'
#'    2. Manually modify both files:
#'       - (serverLinks.txt) With: Cloud server link and assign a site# for each Cloud
#'       - (TTId.txt) With: (TT#) and TT serial number

# (C.3) Run "DownloadNaturetalkersA" to save the function:
DownloadNaturetalkersC <- function() {
  # Read input parameters
  inpuParametersPath <-
    system.file("parameters", "serverLinks.txt", package = "TreeTalkersCheck") # Lee los links de los parámetros
  suppressWarnings(inpuParameters <-
                     read.table(inpuParametersPath, header = T))
  inpuParameters$server <- as.character(inpuParameters$server)
  inpuParameters$site   <- as.character(inpuParameters$site)
  inpuParameters$cloud  <- basename(dirname(inpuParameters$server))
  # Download
  outdir <- file.path("C:", "TreeTalkerDB") 
  outfile <- file.path("C:", "TreeTalkerDB", "TT_CloudDB.csv")
  if (file.exists(outfile))
    file.remove(outfile)
  if (isFALSE(dir.exists(outdir))) {
    dir.create(outdir)
  }
  outFileNames <-
    paste0(outdir,
           "/",
           paste0(inpuParameters$cloud, "_", inpuParameters$site),
           ".txt")
  for (i in 1:nrow(inpuParameters))  {
    download.file(inpuParameters$server[i], outFileNames[i])
    file_i <-
      read.table(
        outFileNames[i],
        sep = ";",
        col.names = paste0("V", 1:70),
        fill = T
      )
    file_i <- cbind(file_i, inpuParameters$site[i]) # junta los archivos .txt en un solo .csv
    write.table(
      file_i,
      outfile,
      sep = ";",
      col.names = F,
      append = T
    )
    cat("The",
        inpuParameters$server[i],
        "file has been appended to",
        outfile,
        "\n")
  }
}

## (C.4) Run the function: ----
DownloadNaturetalkersC()

#' End Option (C) ---------------------------------------------------------


#####################################################################################-
# PART 2: READ AND SPLIT DATABASES -----------------------
#####################################################################################-

ReadServerData <- function() {
  dbname <- "C:/TreeTalkerDB/TT_CloudDB.csv"
  cat(
    "Cleaning the",
    dbname,
    "file and organizing the data into the C:/TreeTalkerDB/ttDB.csv and the C:/TreeTalkerDB/CloudDB.csv files.",
    "\n"
  )
  
  # reading servers data
  serversData <- read.table(dbname, sep = ";")
  
  # manage V2 column
  cutV2 <- strsplit(as.character(serversData$V2), ",")
  # select just the second element
  ttId <- do.call(c, lapply(cutV2, function(x) {
    x[2]
  }))
  
  sitoId <- serversData[, ncol(serversData)]
  
  serversData <-
    cbind(sitoId, ttId, serversData[, 4:(ncol(serversData) - 1)])
  
  # We have 4 different types of data
  dbString <- list("4B", "4C", "4D", "49", "55")
  db4B <- serversData[serversData$V4 == "4B", ]
  db4C <- serversData[serversData$V4 == "4C", ]
  
  db4D <- serversData[serversData$V4 == "4D" | serversData$V4 == "55", ]
  db49 <- serversData[serversData$V4 == "49", ]
  
  ## DB cloud
  # Columns rename
  colnames(db4B) <-
    c(
      "Location",
      "cloudId",
      "stringType",
      "timeStamp",
      "accumulatedRecords",
      "pendingRecords",
      "MCC",
      "MNC",
      "GSMregistration",
      "GSMfield",
      "Battery",
      "firware"
    )
  db4B <- db4B[, 1:12]
  
  # remove the columns with empty timeslot otherwise R sees them as a record
  db4C$V7 <- NULL
  
  colnames(db4C) <-
    c(
      "Location",
      "cloudId",
      "stringType",
      "timeStamp",
      "TBLlocked",
      paste0("Sensor", 1:64)
    )
  
  # merge cloud different strings (4B and 4C)
  cloudDB <- merge(db4B, db4C, by = c('cloudId', 'timeStamp'))
  cloudDB <<- cloudDB[!duplicated(cloudDB), ]
  cloudDB_filename <- file.path("C:", "TreeTalkerDB", "CloudDB.csv")
  
  write.table(cloudDB,
              cloudDB_filename,
              sep = ";",
              row.names = F)
  
  # DB TT
  #rename
  colnames(db4D) <- c(
    "Location",
    "TTsn",
    "stringType",
    "timeStamp",
    "tref_0",
    "theat_0",
    "growth",
    "adc_bandgap",
    "nbits",
    "relativeHumidity",
    "temperature",
    "gzm",
    "gzsd",
    "gym",
    "gysd",
    "gxm",
    "gxsd",
    "tref_1",
    "theat_1",
    "stwc",
    "adc_vbat"
  )
  db4D <- db4D[, 1:21]
  colnames(db49) <- c("Location",
                      "TTsn",
                      "stringType",
                      "timeStamp",
                      paste0("B", 1:12),
                      "intTime",
                      "gain")
  db49 <- db49[, 1:18]
  # merge TT different strings (4D and 49)
  ttDB <- merge(db4D, db49, by = c('TTsn', 'timeStamp'))
  ttDB <- ttDB[!duplicated(ttDB), ]
  
  # add the TTId to the db
  TT_code <-
    read.table(
      system.file("parameters", "TTId.txt", package = "TreeTalkersCheck"),
      sep = ";",
      header = T
    )
  colnames(TT_code)[1] <- "ttid"
  TTsn <- as.character(ttDB$TTsn)
  TTid <- TT_code$ttid[match(TTsn, TT_code$TTSn)]
  ttDB <<- cbind(TTid, ttDB)
  
  ttDB_filename <- file.path("C:", "TreeTalkerDB", "ttDB.csv")
  write.table(ttDB, ttDB_filename, sep = ";", row.names = F)
  
  #return(list(cloudDB, ttDB))
  }

# Run the function: ----
ReadServerData()



#####################################################################################-
# PART 3: CHECK CLOUDS ALERTS -----------------------
#####################################################################################-

# Define variables for the function
dbCloud <- cloudDB          # Copy Cloud DB
whichDay <- Sys.Date() - 1  # Date of interest. Format: (YYYY-MM-DD)

checkCloud <- function(dbCloud, whichDay = Sys.Date() - 1) {
  cat("Checking the status of clouds on", paste0(whichDay),  "\n")
  
  # dbCloudToday
  dbCloud$timeStamp <-
    lubridate::as_datetime(dbCloud$timeStamp) + 7200 # Convert an object to a date-time +2:00
  dbCloudToday <- as.Date(dbCloud$timeStamp) == whichDay
  dbCloudToday <- dbCloud[dbCloudToday,]      # Filtra los datos con la fecha escogida
  
  # remove the columns with empty timeslots otherwise R recognize them as a record
  colonneFullZero <- apply(dbCloudToday, 2, function(x) {
    any(x != 0)
  })
  colonneFullZero[is.na(colonneFullZero)] <- FALSE
  dbCloudToday <- dbCloudToday[, colonneFullZero]
  
  # input parameters
  minimumGSM <- 9                   # condition_1 GSM field level ranges from 0 to 32
  maximumNumberOfPengindItems <- 10 # condition_2 Pending data to be sent
  minimumBattery <- 4000            # condition_3 Battery level ranges from 3300 to 4100 (mV)
  minimumTalkerSignal <- -100       # condition_4 RSSI signal ranges from -137 to -53, -53 excellent signal
  
  # check for factors
  for (i in 16:ncol(dbCloudToday)) {
    dbCloudToday[, i] <- as.double(as.character(dbCloudToday[, i]))
  }
  
  # verifying the conditions
  condition_1 <- dbCloudToday$GSMfield > minimumGSM
  condition_2 <-
    dbCloudToday$pendingRecords < maximumNumberOfPengindItems # Empty when all the records are in the server
  condition_3 <- dbCloudToday$Battery > minimumBattery
  lowSignalSensorsList <- list()
  
  for (i in 1:nrow(dbCloudToday)) {
    Sensors_row_i <-
      as.double(as.character(dbCloudToday[i, 16:ncol(dbCloudToday)]))
    lowSignalSensors <- Sensors_row_i < minimumTalkerSignal
    lowSignalSensors[is.na(lowSignalSensors)] <- FALSE
    whichSensorsLowSignal <-
      (1:(ncol(dbCloudToday) - 14))[lowSignalSensors]
    lowSignalSensorsList[[i]] <- whichSensorsLowSignal
  }
  
  # Extract the checking info
  
  # GSM field
  signal <- dbCloudToday[condition_1 == FALSE,]
  signal <- signal[, c(1, 2, 3, 9)]
  
  # pending records
  if (length(condition_2) > 0) {
    pendingRecords <- dbCloudToday[condition_2 == FALSE,]
    pendingRecords <- pendingRecords[, c(1, 2, 3, 6)]
  } else{
    pendingRecords <- dbCloudToday[FALSE, c(1, 2, 3, 6)]
  }
  # battery level
  battery <- dbCloudToday[condition_3 == FALSE,]
  battery <- cbind(battery[, c(1, 2, 3)], battery$Battery)
  
  # RSSI signal
  lowSignalSensorsList <-
    lapply(lowSignalSensorsList, function(x) {
      paste(x, collapse = ";")
    })
  lowSignalSensors <- unlist(lowSignalSensorsList)
  
  lowSignalSensorsDb <- dbCloudToday[lowSignalSensors != "",]
  lowSignalSensorsDb <- lowSignalSensorsDb[, c(1, 2, 3)]
  lowSignalSensorsDb <-
    cbind(lowSignalSensorsDb, lowSignalSensors[lowSignalSensors != ""])
  colnames(lowSignalSensorsDb)[4] <- "whichSensor"
  
  colnames(signal)[4] <- "issue"
  colnames(pendingRecords)[4] <- "issue"
  colnames(battery)[4] <- "issue"
  colnames(lowSignalSensorsDb)[4] <- "issue"
  tipology <- c(
    rep("cloudSignal", nrow(signal)),
    rep("pendingRecords", nrow(pendingRecords)),
    rep("battery", nrow(battery)),
    rep("sensorSignal", nrow(lowSignalSensorsDb))
  )
  
  out <- rbind(signal, pendingRecords, battery, lowSignalSensorsDb)
  out <- cbind(tipology, out$issue, out[, 1:3])
  colnames(out)[2] <- "issue"
  
  out_dir <- file.path("C:", "TreeTalkerDB", "dailyDB")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)
  
  write.table(out,
              file.path(out_dir, paste0(whichDay, "_cloudAlert.csv")),
              row.names = FALSE,
              sep = ";")
  write.table(dbCloudToday,
              file.path(out_dir, paste0(whichDay, "_cloud.csv")),
              row.names = FALSE,
              sep = ";")
  
  #return(list(dbCloudToday, out))
}

# Run function:
checkCloud(dbCloud, whichDay)


#####################################################################################-
# PART 4: CHECK TTs -----------------------
#####################################################################################-

# Define variables for the function
dbTT <- ttDB                # Copy Cloud DB

checkTT <- function(dbTT, whichDay = Sys.Date()) {
  whichDay <- as.Date(whichDay)
  
  cat("Checking the status of TTs on", paste0(whichDay), "\n")
  cat("Check out the final output in C:/TreeTalkerDB/dailyDB as", paste0(whichDay, "_Full_TT.csv"), "\n")
  
  # dbTT_whichDay
  dbTT$timeStamp <- lubridate::as_datetime(dbTT$timeStamp) +7200 #+7200 is the offset GMT+02:00 in Spain
  #dbTT <- dbTT[order(dbTT$ltimeStamp),]
  
  # Temperature
  dbTT$temperature <- dbTT$temperature / 10
  
  # battery calculation
  batteryLevel <- 2 * 1100 * dbTT$adc_vbat / dbTT$adc_bandgap / 1000
  batteryLevel <- round(batteryLevel, 2)
  dbTT <- cbind(dbTT, batteryLevel)
  
  # sap flux density calculation
  FromDigitalNumberToTemperature = function(x) {
    127.6 - (0.006045 * x) + (0.000000126 * x ^ 2) - (0.00000000000115 * x ^ 3)
  }
  dbTT$tref_0 <-
    FromDigitalNumberToTemperature(dbTT$tref_0)
  dbTT$tref_1 <-
    FromDigitalNumberToTemperature(dbTT$tref_1)
  dbTT$theat_0 <-
    FromDigitalNumberToTemperature(dbTT$theat_0)
  dbTT$theat_1 <-
    FromDigitalNumberToTemperature(dbTT$theat_1)
  
  # Method 1
  # DeltaTon <- dbTT$theat_1 - dbTT$tref_1
  # DeltaToff <- dbTT$theat_0 - dbTT$tref_0
  # dbTT <- cbind(dbTT, DeltaTon, DeltaToff)
  # DeltaT_max <-
  #   aggregate(DeltaTon, list(as.character(dbTT$TTid)), max)
  #
  # #DeltaTMax has been calculated daily for each TT and then appied to each TT associating TTid and Location
  # colnames(DeltaT_max) <- c("Location", "DeltaTMax")
  # ....
  # ....
  # sapFluxDensity <-
  #   (118.99 * (((
  #     as.double(as.character(dbTT$DeltaTMax)) /
  #       (as.double(as.character(
  #         dbTT$DeltaTon
  #       )) -
  #         as.double(as.character(
  #           dbTT$DeltaToff
  #         )))
  #   ) - 1) ^ 1.231))
  
  # Method 2
  DeltaT_heat <- dbTT$theat_1 - dbTT$theat_0
  DeltaT_max <-
    aggregate(DeltaT_heat, list(as.character(dbTT$TTid)), max)
  DeltaTMax <-
    as.character(DeltaT_max$x)[match(as.character(dbTT$TTid),
                                     as.character(DeltaT_max$Group.1))]
  # dbTT <- cbind(dbTT, DeltaT_heat, DeltaT_max)
  dbTT <- cbind(dbTT, DeltaT_heat)
  
  # sapFluxDensity <- 4.79 * 27.777 * ((as.double(DeltaTMax)-DeltaT_heat)/DeltaT_heat)
  sapFluxDensity <-
    4.79 * ((as.double(DeltaTMax) - DeltaT_heat) / DeltaT_heat)
  
  dbTT <- cbind(dbTT, sapFluxDensity)
  
  # growth rate calculation: distance (cm) (1.5 - 5 cm)
  #growthRate <- 18.2823+(-0.0006*dbTT$growth)+(6.9143e-009*dbTT$growth^2)+(-3.0237e-14*dbTT$growth^3)
  #dbTT <- cbind(dbTT, growthRate)
  
  # Stem Diameter Variation (SDV) (cm)
  # stemDiameterVariation <-
  #   (0.000000008 * (dbTT$growth ^ 2)) - (0.0016 * dbTT$growth) +
  #   89.032
  
  stemDiameterVariation <-
    (237908.4541 + (-1.1171 * dbTT$growth)) / (199.4330 + dbTT$growth)
  
  dbTT <- cbind(dbTT, stemDiameterVariation)
  
  # Tree Movement: angle of osciliation (°)
  dbTT$gzm <- as.numeric(dbTT$gzm)
  gz <- abs(dbTT$gzm)
  gx <- abs(dbTT$gxm)
  gy <- abs(dbTT$gym)
  gxy <- sqrt(gx^2 + gy^2)
  TreeMovement <- (atan2(gxy,gz)*180)/(pi)
  
  dbTT <- cbind(dbTT, TreeMovement)
  
  # Spectrometer
  # To convert the digital numbers (d.n.) into corrected spectral band
  
  # dbTT$B1 <- -312.45+(1.6699 *dbTT$B1)
  # dbTT$B2 <- -561.56+(1.5199 *dbTT$B2)
  # dbTT$B3 <- -1511.2+(1.6209 *dbTT$B3)
  # dbTT$B4 <- -1012.5+(1.4549 *dbTT$B4)
  # dbTT$B5 <- 91.58+(0.8414 *dbTT$B5)
  # dbTT$B6 <- 334.88+(0.531 *dbTT$B6)
  # dbTT$B7 <- -212.62+(0.4562*dbTT$B7)
  # dbTT$B8 <- as.numeric(dbTT$B8)
  # dbTT$B8 <- -232.13+(0.6257 *dbTT$B8)
  # dbTT$B9 <- -842.1+(1.0546 *dbTT$B9)
  # dbTT$B10 <- -666.72+(1.0462 *dbTT$B10)
  # dbTT$B11 <- -328.08+(0.8654 *dbTT$B11)
  # dbTT$B12 <- 202.77+(0.7829 *dbTT$B12)
  
  # return as a variable in global environment
  dbTT <<- dbTT
  
  # Write outputs
  out_dir <- file.path("C:", "TreeTalkerDB", "dailyDB")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)
  
  write.table(dbTT,
              file.path(out_dir, paste0(whichDay, "_Full_TT.csv")),
              row.names = FALSE,
              sep = ";")
  
  #return(list(dbTT))
}

# Run function:
checkTT(dbTT, whichDay)




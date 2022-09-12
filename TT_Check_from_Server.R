###' TREE TALKERS CHECK FROM SERVER####
###'
###' BY: Isabel C. Grisales Sanchez
###' E-MAIL: igriss00@estudiantes.unileon.es
###' DATE: August, 2022
###' Developed with R 4.2.1. and RStudio (2022.07.1+554)
###' COLLABORATOR: Laboratorio de teledetección próxima a la tierra del observatorio 
###' de cambio global de bosque mediterráneo
 

# Loading required packages
required_packages <- c("signal", "lubridate", "ggplot2", "dplyr",
                       "scales", "reshape2", "knitr", "rmarkdown", "prospectr")
missing_packages <-
  required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) != 0)
  install.packages(missing_packages, repos = "http://cran.us.r-project.org")
suppressWarnings(suppressPackageStartupMessages(
  noPrint <- lapply(required_packages, library, character.only = T)
))



#####################################################################################-
# PART 1: DOWNLOAD DATABASES -----------------------
#####################################################################################-

#' WAYS TO READ AND DOWNLOAD THE DATA:
#'
#' Two options:
#'   (A) indicating the server links and parameters: TT_Check_from_Server.R
#'   (B) reading *.txt files that were downloaded directly from TT+: TT_Check_from_TXT.R


####
## OPTION A: Indicating the servers links ----
####

#' Inputs: TTCloud server links from Naturetalkers
#' Outputs: One database appended to C:/TreeTalkerDB/Raw_fullDB.csv

rm(list=ls())

# (A.1) Write/modify the following 3 inputs (examples):

  # Server data links for each Cloud
server <- c('http://www.naturetalkers.altervista.org/C0210043/ttcloud.txt',
            'http://www.naturetalkers.altervista.org/C0210044/ttcloud.txt')
  # Give a name to each cloud (site or location name)
site   <- c('VM',
            'TV')
  # Serial number of each cloud
cloud  <- c('C0210043',
            'C0210044')

# (A.2) Run "DownloadTTDB_A" to save the function:

DownloadTTDB_A <- function() {
  # Read input parameters
  inpuParameters <- as.data.frame(cbind(server, site, cloud))
  # Create folder on C:/ for the outputs
  outdir <- file.path("C:", "TreeTalkerDB")
  outfile <- file.path("C:", "TreeTalkerDB", "Raw_fullDB.csv")
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
DownloadTTDB_A()


#####################################################################################-
# PART 2: READ AND SPLIT DATABASES -----------------------
#####################################################################################-

ReadServerData <- function() {
  dbname <- "C:/TreeTalkerDB/Raw_fullDB.csv"
  cat("Cleaning the",
    dbname,
    "file and organizing the data into the C:/TreeTalkerDB/Raw_TTDB.csv",
    "and the C:/TreeTalkerDB/Raw_CloudDB.csv files.",
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

  siteId <- serversData[, ncol(serversData)]

  serversData <-
    cbind(siteId, ttId, serversData[, 4:(ncol(serversData) - 1)])

  # We have 4 different types of data
  dbString <- list("4B", "4C", "4D", "49", "55")
  db4B <- serversData[serversData$V4 == "4B", ]
  db4C <- serversData[serversData$V4 == "4C", ]
  db55 <- serversData[serversData$V4 == "4D" | serversData$V4 == "55", ]
  db49 <- serversData[serversData$V4 == "49", ]

  ## DB cloud
  colnames(db4B) <- c(
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
  cloudDB_filename <- file.path("C:", "TreeTalkerDB", "Raw_CloudDB.csv")

  write.table(cloudDB,
              cloudDB_filename,
              sep = ";",
              row.names = F)

  # DB TT
  colnames(db55) <- c(
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
  db55 <- db55[, 1:21]
  colnames(db49) <- c("Location",
                      "TTsn",
                      "stringType",
                      "timeStamp",
                      paste0("B", 1:12),
                      "intTime",
                      "gain")
  db49 <- db49[, 1:18]

  # merge TT different strings (4D and 49)
  ttDB <- merge(db55, db49, by = c('TTsn', 'timeStamp'))
  ttDB <- ttDB[!duplicated(ttDB), ]

  TTsn <- as.character(ttDB$TTsn)
  TTid <- paste0(ttDB$Location.x, "_TT", substring(ttDB$TTsn,7))

  ttDB <<- cbind(TTid, ttDB)

  ttDB_filename <- file.path("C:", "TreeTalkerDB", "Raw_TTDB.csv")
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

# Date of interest for clouds, by default yesterday. Format: (YYYY-MM-DD)
whichDay = Sys.Date() - 1

checkCloud <- function(dbCloud, whichDay = Sys.Date() - 1) {
  cat("Checking the status of clouds on", paste0(whichDay),  "\n")
  cat("Check out the outputs on C:/TreeTalkerDB/TTResults as",
      paste0(whichDay, "cloud.csv"), "and", paste0(whichDay, "cloudAlert.csv"), "\n")

  # dbCloudToday
  dbCloud$timeStamp <-
    lubridate::as_datetime(dbCloud$timeStamp) + 7200 # Convert an object to a date-time +2:00

  # Filter data by day
  dbCloudToday <- as.Date(dbCloud$timeStamp) == whichDay
  dbCloudToday <- dbCloud[dbCloudToday,]

  # remove the columns with empty timeslots otherwise R recognize them as a record
  colNA <- apply(dbCloudToday, 2, function(x) {
    any(x != 0)
  })
  colNA[is.na(colNA)] <- FALSE
  dbCloudToday <- dbCloudToday[, colNA]

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

  # 3.1. GSM field ----
  signal <- dbCloudToday[condition_1 == FALSE,]
  signal <- signal[, c(1, 2, 3, 9)]

  # 3.2. Pending records ----
  if (length(condition_2) > 0) {
    pendingRecords <- dbCloudToday[condition_2 == FALSE,]
    pendingRecords <- pendingRecords[, c(1, 2, 3, 6)]
  } else{
    pendingRecords <- dbCloudToday[FALSE, c(1, 2, 3, 6)]
  }

  # 3.3. Battery level ----
  battery <- dbCloudToday[condition_3 == FALSE,]
  battery <- cbind(battery[, c(1, 2, 3)], battery$Battery)

  # 3.4. RSSI signal ----
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

  dbCloudToday <<- dbCloudToday

  # Save outputs

  out_dir <- file.path("C:", "TreeTalkerDB", "TTResults")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)

  write.table(out,
              file.path(out_dir, paste0(whichDay, "_CloudAlert.csv")),
              row.names = FALSE,
              sep = ";")
  write.table(dbCloudToday,
              file.path(out_dir, paste0(whichDay, "_Cloud.csv")),
              row.names = FALSE,
              sep = ";")

  #return(list(dbCloudToday, out))
}

# Run function:
checkCloud(dbCloud, whichDay)


#####################################################################################-
# PART 4: CHECK TTs -----------------------
#####################################################################################-

#' Output:"_TTDB.csv" in "C:/TreeTalkerDB/TTResults"

# Define variables for the function
dbTT <- ttDB  # Copy TT's DB from previous step.
              # Filter by TT here if it is necessary

checkTT <- function(dbTT) {
  whichDay <- as.Date(Sys.Date())

  cat("Checking the status of TTs on", paste0(whichDay), "\n")
  cat("Check out the output as the full database on C:/TreeTalkerDB/TTResults as",
      paste0(whichDay, "_TTBD.csv"), "\n")

  # 4.1. TimeStamp -------------------------------------------------------------
  dbTT$timeStamp <- lubridate::as_datetime(dbTT$timeStamp) + 7200
  #+7200 is the offset GMT+02:00 in Spain

  # 4.2. Battery level ---------------------------------------------------------
  batteryLevel <- 2 * 1100 * dbTT$adc_vbat / dbTT$adc_bandgap / 1000
  batteryLevel <- round(batteryLevel, 2)
  dbTT <- cbind(dbTT, batteryLevel)

  # 4.3. Air Temperature -------------------------------------------------------
  dbTT$temperature <- dbTT$temperature / 10

   # clean temperature data
  dbTT$temperature[dbTT$temperature < -20 | dbTT$temperature> 50] <- NA

  # Plot to visualize temperature data
  #plot(dbTT$temperature,
  #     type = "l",
  #     main = "Temperature",
  #     xlab = "Data in order",
  #     ylab = "Temperature (°C)")

  # 4.4. Sap flux density (SFD) ------------------------------------------------
  # Method adapted from Tomelleri, et al 2022

  # From D.N. to temperature
  FromDigitalNumberToTemperature = function(x) {
    127.6 - (0.006045 * x) + (1.26E-7 * x ^ 2) - (1.15E-12 * x ^ 3)
  }
  dbTT$tref_0 <-
    FromDigitalNumberToTemperature(dbTT$tref_0)
  dbTT$tref_1 <-
    FromDigitalNumberToTemperature(dbTT$tref_1)
  dbTT$theat_0 <-
    FromDigitalNumberToTemperature(dbTT$theat_0)
  dbTT$theat_1 <-
    FromDigitalNumberToTemperature(dbTT$theat_1)

  # clean temperature data
  dbTT$tref_0[dbTT$tref_0 < -20 | dbTT$tref_0> 50] <- NA
  dbTT$tref_1[dbTT$tref_1 < -20 | dbTT$tref_1> 50] <- NA
  dbTT$theat_0[dbTT$theat_0 < -20 | dbTT$theat_0> 50] <- NA
  dbTT$theat_1[dbTT$theat_1 < -20 | dbTT$theat_1> 50] <- NA

  # Apply a Savitzky-Golay smoothing
  library(signal)

  ## For tref_0
  tfilt <- dbTT$tref_0 # data to be filtered

  ID <- unique(dbTT$TTid)
  for (j in 1:length(ID)) {
    ts <- tfilt[dbTT$TTid == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    m = 0
    p = 2
    w = 7
    ts_filt <- savitzkyGolay(ts, m, p, w)
    tfilt[dbTT$TTid == ID[j]] <- ts_filt[1:length(ts)]
  }
  dbTT$tref_0filt <- tfilt

  ## For tref_1
  tfilt <- dbTT$tref_1

  ID <- unique(dbTT$TTid)
  for (j in 1:length(ID)) {
    ts <- tfilt[dbTT$TTid == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    m = 0
    p = 2
    w = 7
    ts_filt <- savitzkyGolay(ts, m, p, w)
    tfilt[dbTT$TTid == ID[j]] <- ts_filt[1:length(ts)]
  }
  dbTT$tref_1filt <- tfilt

  ## For theat_0
  tfilt <- dbTT$theat_0

  ID <- unique(dbTT$TTid)
  for (j in 1:length(ID)) {
    ts <- tfilt[dbTT$TTid == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    m = 0
    p = 2
    w = 7
    ts_filt <- savitzkyGolay(ts, m, p, w)
    tfilt[dbTT$TTid == ID[j]] <- ts_filt[1:length(ts)]
  }
  dbTT$theat_0filt <- tfilt

  ## For theat_1
  tfilt <- dbTT$theat_1

  ID <- unique(dbTT$TTid)
  for (j in 1:length(ID)) {
    ts <- tfilt[dbTT$TTid == ID[j]]
    if (length(ts) < 11) {
      next()
    }
    m = 0
    p = 2
    w = 7
    ts_filt <- savitzkyGolay(ts, m, p, w)
    tfilt[dbTT$TTid == ID[j]] <- ts_filt[1:length(ts)]
  }
  dbTT$theat_1filt <- tfilt

  # Plot to visualize one temperature data (tref_0_filt in this case)
  #plot(dbTT$tref_0filt,
  #    type = "l",
  #    main = "tref_0 filtered with Savitzky-Golay",
  #    xlab = "Data in order",
  #    ylab = "tref_0filt (°C)")

  # SFD, method from the manual, SFD (ldm-2h-1)

  # Two options to calculate deltaT:
    # delta T1
  DeltaT_heat <- dbTT$theat_1filt - dbTT$theat_0filt
    # delta T2
  DeltaT_heat2 <- (dbTT$theat_1filt -dbTT$tref_1filt) - (dbTT$theat_0filt - dbTT$tref_0filt)

  # Get the Max value from each TT PER DAY

  # create a column by day
  dataDay <- paste0(month(dbTT$timeStamp), day(dbTT$timeStamp))

  # Create a new df for SFD
  TTid <- dbTT$TTid
  dbSFD <- as.data.frame(cbind(TTid, dataDay, DeltaT_heat, DeltaT_heat2))

  # Aggregate data by TT and by day = obtain the max value per day and TT
  delta_temp <- aggregate(DeltaT_heat ~ TTid + dataDay, data = dbSFD, FUN = max, na.rm = TRUE)

  dbSFD <- left_join(dbSFD, delta_temp, by = c ("TTid", "dataDay"))

  DeltaTMax <- dbSFD$DeltaT_heat.y

  # SFD
  sapFluxDensity <-
    4.79 * ((as.double(DeltaTMax) - DeltaT_heat) / DeltaT_heat)

  #sapFluxDensity[sapFluxDensity < 0] <- NA

  dbTT <- cbind(dbTT, sapFluxDensity)

  # Plot for SFD
  #plot(dbTT$sapFluxDensity,
  #  type = "l",
  #  main = "Sap Flux Density",
  #  xlab = "Data in order",
  #  ylab = "SFD (ldm-2h-1)")


  # 4.5. Weekly Radial Growth --------------------------------------------------

  # Packages:
  #library(lubridate)
  #library(dplyr)

  # Conversion range according to the manual (September 2020)
  # First data filter for [D.N.]
  tgrow <- dbTT$growth
  tgrow[tgrow > 85000] <- NA
  tgrow[tgrow < 30000] <- NA

  # Plot to visualize growth sensor d.n.
  #plot(tgrow,
  #     type = "l",
  #     main = "Growth sensor d.n.",
  #     xlab = "Data in order",
  #     ylab = "Growth sensor d.n.")

  # Distance from sensor to the bark (cm from the TT+ manual, *10 to mm units)
  dbTT$DistGrowth <-
    ((237908.4541 + (-1.1171 * tgrow)) / (199.4330 + tgrow))*10

  # Create a time index for the temporal averaging (YYYYww)
  time_index <- paste0(year(dbTT$timeStamp), week(dbTT$timeStamp))
  dbTT$YYYYww <-  time_index

  # Method from paper: Tomelleri et al, 2022
  ID <- unique(dbTT$TTid)
  for (j in 1:length(ID)) {
    myDendro_data_L0 <-
      data.frame(dbTT$TTid, dbTT$timeStamp, dbTT$YYYYww, dbTT$DistGrowth)
    colnames(myDendro_data_L0) <- c("series", "ts", "YYYYww", "dendrometer")

    # Subset dataset for TTids
    myDendro_data_L0 <- myDendro_data_L0 %>% dplyr::filter(series == ID[j])
    if (length(na.omit(myDendro_data_L0$dendrometer))<100){next}

    #remove outliers
    t_05 <- quantile(myDendro_data_L0$dendrometer, p=0.05, na.rm=T)
    t_95 <- quantile(myDendro_data_L0$dendrometer, p=0.95, na.rm=T)
    myDendro_data_L0$dendrometer[myDendro_data_L0$dendrometer<t_05] <- NA
    myDendro_data_L0$dendrometer[myDendro_data_L0$dendrometer>t_95] <- NA

    #create a data.frame with Level 1 data
    myDendro_data_L1 <- myDendro_data_L0

    #temporal averaging
    myDendro_data_weekly <-
      aggregate(dendrometer ~ YYYYww, myDendro_data_L1, median)

    #normalize the radial growth with the initial sensor distance
    myDendro_data_weekly$dendrometer <-
      (max(myDendro_data_weekly$dendrometer) - myDendro_data_weekly$dendrometer)

    #create a data.frame with Level 2 data
    myDendro_data_L2 <- subset(myDendro_data_L1, select = -dendrometer)

    #merge averaged sharp data with the specific TreeTalker data.frame
    myDendro_data_L2 <- merge(myDendro_data_L2, myDendro_data_weekly, all.x=T)

    #insert processed data into the dbTT data.frame
    dbTT$dendrometer[dbTT$TTid == ID[j]] <- myDendro_data_L2$dendrometer
  }

  # plot weekly radial growth (mm)
  #plot(dbTT$dendrometer,
  #     type = "l",
  #     main = "Weekly Radial Growth (mm)",
  #     xlab = "Data in order",
  #     ylab = "Weekly Radial Growth (mm)")


  # 4.6. Tree Movement: angle of osciliation (°) -------------------------------

  dbTT$gzm <- as.numeric(dbTT$gzm)
  gz <- abs(dbTT$gzm)
  gx <- abs(dbTT$gxm)
  gy <- abs(dbTT$gym)

  ## Yaw angle
  #gxy <- sqrt(gx^2 + gy^2)
  #TreeMovement <- (atan2(gxy,gz)*180)/(pi)

  ## Pitch angle
  gyz <- sqrt(gy^2 + gz^2)
  TreeMovement <- (atan2(gx,gyz)*180)/(pi)

  dbTT <- cbind(dbTT, TreeMovement)

  # 4.7. Spectral light through the canopy -------------------------------------

  # To convert the digital numbers (d.n.) into corrected spectral bands

  #Copy bands
  #Near infrared
  dbTT$B610 <- as.numeric(dbTT$B1)
  dbTT$B680 <- as.numeric(dbTT$B2)
  dbTT$B730 <- as.numeric(dbTT$B3)
  dbTT$B760 <- as.numeric(dbTT$B4)
  dbTT$B810 <- as.numeric(dbTT$B5)
  dbTT$B860 <- as.numeric(dbTT$B6)
  #Visible light spectrum
  dbTT$B450 <- as.numeric(dbTT$B7)
  dbTT$B500 <- as.numeric(dbTT$B8)
  dbTT$B550 <- as.numeric(dbTT$B9)
  dbTT$B570 <- as.numeric(dbTT$B10)
  dbTT$B600 <- as.numeric(dbTT$B11)
  dbTT$B650 <- as.numeric(dbTT$B12)

  #Delete high values
  dbTT$B610[dbTT$B610 == 0 | dbTT$B610 > 65000 ] <- NA
  dbTT$B680[dbTT$B680 == 0 | dbTT$B680 > 65000 ] <- NA
  dbTT$B730[dbTT$B730 == 0 | dbTT$B730 > 65000 ] <- NA
  dbTT$B760[dbTT$B760 == 0 | dbTT$B760 > 65000 ] <- NA
  dbTT$B810[dbTT$B810 == 0 | dbTT$B810 > 65000 ] <- NA
  dbTT$B450[dbTT$B450 == 0 | dbTT$B450 > 65000 ] <- NA
  dbTT$B500[dbTT$B500 == 0 | dbTT$B500 > 65000 ] <- NA
  dbTT$B550[dbTT$B550 == 0 | dbTT$B550 > 65000 ] <- NA
  dbTT$B570[dbTT$B570 == 0 | dbTT$B570 > 65000 ] <- NA
  dbTT$B600[dbTT$B600 == 0 | dbTT$B600 > 65000 ] <- NA
  dbTT$B650[dbTT$B650 == 0 | dbTT$B650 > 65000 ] <- NA

  #Convert the digital numbers (d.n.) into corrected spectral bands
  dbTT$B610 <- -312.45+(1.6699 *dbTT$B610)
  dbTT$B680 <- -561.56+(1.5199 *dbTT$B680)
  dbTT$B730 <- -1511.2+(1.6209 *dbTT$B730)
  dbTT$B760 <- -1012.5+(1.4549 *dbTT$B760)
  dbTT$B810 <- 91.58+(0.8414 *dbTT$B810)
  dbTT$B860 <- 334.88+(0.531 *dbTT$B860)
  dbTT$B450 <- -212.62+(0.4562*dbTT$B450)
  dbTT$B500 <- -232.13+(0.6257 *dbTT$B500)
  dbTT$B550 <- -842.1+(1.0546 *dbTT$B550)
  dbTT$B570 <- -666.72+(1.0462 *dbTT$B570)
  dbTT$B600 <- -328.08+(0.8654 *dbTT$B600)
  dbTT$B650 <- 202.77+(0.7829 *dbTT$B650)


  # 4.8. Stem Water Content ---------------------------------------------------

  #' Relative Saturation Index, adapted from Asgharinia et al. (2022)

  # Filter stwc if necessary (capacity 3MHz in this case)
  # See plot
  #plot(dbTT$stwc,
  #     type = "l",
  #     main = "Stem water content (stwc)",
  #     xlab = "Data in order",
  #     ylab = "ECf_stem sensor, stwc [d.n.]")

  # Get the min value of the stwc sensor from each TT+
  stwc_min <-
    aggregate(dbTT$stwc, list(as.character(dbTT$TTid)), min, na.rm=TRUE)

  # Ceate a vector with the Min value of stwc per TT+
  stwcMin <-
    as.numeric(stwc_min$x)[match(as.character(dbTT$TTid),
                                 as.character(stwc_min$Group.1))]

  # Obtain temperature (filtered previously) with associated to the Min stwc
  Tmin <- dbTT[dbTT$stwc == stwcMin,] # stwc_min$x replaced for stwcMin

  stwcT_Min <-
    as.numeric(Tmin$tref_0filt)[match(as.character(dbTT$TTid),
                                      as.character(Tmin$TTid))]

  ECfsat <- stwcMin
  Tsat <- stwcT_Min

  # m, the slope of temperature sensitivity for “water”*
  # *in this case for now, it should be the tree species
  m = -81

  # Expected frequency signal
  ECf_adj <- m * (dbTT$tref_0filt - Tsat) + ECfsat

  # Relative index of the stem saturation (%)
  RelStemSat <- (1-((dbTT$stwc - ECf_adj)/ECf_adj))*100

  # Plot for the Relative Stem Saturation (%)
  #plot(RelStemSat,
  #     type = "l",
  #     main = "Relative Stem Saturation",
  #     xlab = "Data in order",
  #     ylab = "Relative Stem Saturation (%)")

  #Join results to the main DF
  dbTT <- cbind(dbTT, RelStemSat)


  ## End of variables calculation  ---------------------------------------------

  # Select columns for results
  dbTT_Results <- dbTT[,c(
    "TTid", "TTsn", "timeStamp", "Location.x", "relativeHumidity",
    "temperature", "batteryLevel", "sapFluxDensity",
    "dendrometer", "TreeMovement", "RelStemSat",
    "B610", "B680", "B730", "B760", "B810", "B860",
    "B450", "B500", "B550", "B570", "B600", "B650")]

  #' Apply a date filter here if prefer a df filtered by date

  # Return as a variable in the global environment
  dbTT <<- dbTT                   # db with some step by step calculations
  dbTT_Results <<- dbTT_Results   # db with the main results

  # Write outputs
  out_dir <- file.path("C:", "TreeTalkerDB", "TTResults")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)

  write.table(dbTT_Results,
              file.path(out_dir, paste0(whichDay, "_TTBD.csv")),
              row.names = FALSE,
              sep = ";")

  #return(list(dbTT))
}

## Run the function: ----
checkTT(dbTT)


###############################################################################-
# PART 5: TTAlert                                        -----------------------
###############################################################################-

#' Creates reports to alert anomalies from TT data

# Copy complete data base history
dbTT_alert <- dbTT

#' Activate one of the next 2 parameters options if data results is desired for one day:
#Day_alert <- as.Date("2022-08-10") # Day to check the alerts
#Day_alert <- as.Date(Sys.Date()-1) # yesterday
#' And activate next 2 lines to subset data frame by a date (Day_alert)
#data_alert <- as.Date(dbTT_alert$timeStamp) == Day_alert
#dbTT_alert <- dbTT_alert[data_alert,]

#' Entry dates for next lines if data results is desired for a range of dates, format: "YYYY-MM-DD"
#Date0 <- as.Date("2022-04-07") # Start
#Date1 <- as.Date("2022-08-17") # End
#dbTT_alert <- dbTT_alert[dbTT_alert$timeStamp >= Date0 & dbTT_alert$timeStamp <= Date1 + 1,]

TTAlert <- function(dbTT_alert) {

  whichDay <- as.Date(Sys.Date())

  cat("Checking the status of TTs on", paste0(whichDay), "\n")

  # Input parameters, when these conditions are TRUE values are reported as alerts

  minimumBattery <- 3.5     # Battery level ranges should be > 3.5 (mV) to work property
  maximumAngle <- 45        # Risk of a falling tree (angle)
  minimumSFD <- 1.0         # Low weekly sap flux density (ldm-2h-1)
  minimumGrowth <- 0.1      # Low weekly radial growth (mm)

  # 5.1. Low battery level alert ----

  ##' To get the last date and value when batteryLevel was <= to minimumBattery by TT
  batteryAlert <- dbTT_alert[dbTT_alert$batteryLevel <= minimumBattery, ]
  batteryAlert <- batteryAlert[, c("TTid", "timeStamp", "batteryLevel")]

  if (nrow(batteryAlert) > 0) {
    batteryAlert2 <- aggregate(timeStamp ~ TTid, data= batteryAlert, FUN=max)
    batteryAlert2 <- left_join(batteryAlert2, batteryAlert,
                               by = c("TTid", "timeStamp"))
    batteryAlert <- batteryAlert2
  }


  # 5.2.Tree movement alert ----

  angleAlert <- dbTT_alert[dbTT_alert$TreeMovement >= maximumAngle, ]
  angleAlert <- angleAlert[, c("TTid", "timeStamp", "TreeMovement")]

  if (nrow(angleAlert) > 0) {
    angleAlert <-
      aggregate(angleAlert[, 2:3], list(as.character(angleAlert$TTid)), min)
    colnames(angleAlert)[1] <- "TTid"
    angleAlert$TreeMovement <- round(angleAlert$TreeMovement)
    colnames(angleAlert)[3] <- "YawAngle"
  }


  # 5.3. Sap Flux Density (SFD) alert ----

  sap_alert <- dbTT_alert[, c("TTid", "timeStamp", "YYYYww", "sapFluxDensity")]
  sap_alert$sapFluxDensity <- round(sap_alert$sapFluxDensity, 4)

  # Maximum weekly  SFD
  WeekMaxSFD <- aggregate(sapFluxDensity ~ TTid + YYYYww, data = sap_alert, FUN = max, na.rm = TRUE)

  # Alert df
  SFDAlert <- WeekMaxSFD[WeekMaxSFD$sapFluxDensity <= minimumSFD, ]

  if (nrow(SFDAlert) > 0) {
    SFDAlert <- left_join(SFDAlert, sap_alert, by = c ("TTid","YYYYww", "sapFluxDensity"))
  }


  # 5.4. Radial growth alert ----

  growth_alert <- dbTT_alert[, c("TTid", "timeStamp", "YYYYww", "dendrometer")]
  growth_alert$dendrometer <- round(growth_alert$dendrometer, 2)

  # Maximum weekly  growth
  WeekMaxGrowth <- aggregate(dendrometer ~ TTid + YYYYww, data = growth_alert, FUN = max, na.rm = TRUE)

  # Package
  #library(dplyr)
  WeekMaxGrowth <- WeekMaxGrowth %>%
    group_by(TTid) %>%
    mutate(deltaGrowth = dendrometer - lag(dendrometer, default = 0))

  # Alert df
  growthAlert <- WeekMaxGrowth[WeekMaxGrowth$deltaGrowth <= minimumGrowth, ]

  # Include week after installation of TTs on trees, otherwise full report
  growthAlert <- growthAlert[growthAlert$YYYYww >= 202216, ]

  # Sort df by TT, not by time
  if (nrow(SFDAlert) > 0) {
    growthAlert <- growthAlert[order(growthAlert$TTid),]
  }

  # Save outputs ----

  out_dir <- file.path("C:", "TreeTalkerDB", "TTResults")
  if (dir.exists(out_dir) == F)
    dir.create(out_dir)

  write.table(batteryAlert,
              file.path(out_dir, paste0(whichDay, "_BatteryTTAlert.csv")),
              row.names = FALSE,
              sep = ";")

  write.table(angleAlert,
              file.path(out_dir, paste0(whichDay, "_AngleTTAlert.csv")),
              row.names = FALSE,
              sep = ";")

  write.table(SFDAlert,
              file.path(out_dir, paste0(whichDay, "_SapFDTTAlert.csv")),
              row.names = FALSE,
              sep = ";")

  write.table(growthAlert,
              file.path(out_dir, paste0(whichDay, "_GrowthTTAlert.csv")),
              row.names = FALSE,
              sep = ";")

  batteryAlert <<- batteryAlert
  angleAlert <<- angleAlert
  SFDAlert <<- SFDAlert
  growthAlert <<- growthAlert
}

# Run function:
TTAlert(dbTT_alert)


###############################################################################-
# PART 6: PLOTS                                         -----------------------
###############################################################################-

# Define db for the function

#' Graphics for complete data (leave by default)
TTdb_today <- dbTT  # Copy TT's DB from previous step

#' Filter by one TT here if it is necessary. Examples:
#unique(dbTT$TTsn)
#TTdb_today <- dbTT[dbTT$TTsn == "621B0393",]

#' Two or more TTs at the same time
#TTdb_today <- dbTT[dbTT$TTsn == "621B0396" | dbTT$TTsn == "621B0397",]

# Entry dates for graphics, format: "YYYY-MM-DD"
Date0 <- as.Date("2022-05-01") # Start
Date1 <- as.Date("2022-05-08") # End

#' plotting is recommended for 1 week or 2 for all variables,
#' disable line that starts with 'scale_x' (**) on each variable when plotting > 2 weeks.
#' If the plotting fails, be sure that the timestamp from TTs are on time.

Plot <- function(TTdb_today, Date0, Date1) {

  # Path for outputs
  outPathReport <-
    file.path(file.path("C:", "TreeTalkerDB"), "TTPlots")
  if (dir.exists(outPathReport) == F)
    dir.create(outPathReport)

  # Message
  cat("Calculating and saving some graphics in", outPathReport,  "\n")

  # Path for outputs 2 (plots)
  TTdirectory <- file.path("C:/", "TreeTalkerDB")
  outPath <-
    file.path(TTdirectory, "TTPlots", paste0(Date0, " _to_", Date1, "_plots"))
  if (dir.exists(outPath) == F)
    dir.create(outPath)

  # data reading
  whichDay = Sys.Date()

  # Subset data frame with dates of interest
  data_new <-
    TTdb_today[TTdb_today$timeStamp >= Date0 & TTdb_today$timeStamp <= Date1 + 1,]

  # Packages
  #library(ggplot2)
  #library(lubridate)
  #library(scales)
  #library(dplyr)
  #library(reshape2)

  theme_set(theme_bw())

  # 6.1. Air Temperature and air humidity -----

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=temperature, col=TTid), size=0.8) +
    labs(title="Air Temperature",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Temperature [°C]",
         x="Date") +
    scale_x_datetime(date_breaks = "12 hours", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "AirTemperature.PNG"),
         width = 8, height = 5)

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=relativeHumidity, col=TTid), size=0.8) +
    labs(title="Air Humidity",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Relative humidity (%)",
         x="Date") +
    scale_x_datetime(date_breaks = "12 hours", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "AirHumidity.PNG"),
         width = 8, height = 5)

  # 6.2. Battery level -----

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=batteryLevel, col=TTid), size=0.8) +
    geom_hline(yintercept = 3.5, color="red", size=1.0) +
    labs(title="TT's Battery Voltage",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Battery Voltage [mV]",
         x="Date") +
    scale_x_datetime(date_breaks = "12 hours", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "BatteryTT.PNG"),
         width = 8, height = 5)

  # 6.3. Sap flux density (SFD) -----

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=sapFluxDensity, col=TTid), size=0.8) +
    labs(title="Sap Flux Density",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="SFD [ldm-2h-1]",
         x="Date") +
    scale_x_datetime(date_breaks = "12 hours", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "sapFluxDensity.PNG"),
         width = 8, height = 5)

  # 6.4. Radial growth -----

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=dendrometer, col=TTid), size=0.8) +
    labs(title="Weekly Radial Growth",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Radial Growth [mm]",
         x="Date") +
    scale_x_datetime(date_breaks = "1 week", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "RadialGrowth.PNG"),
         width = 8, height = 5)

  # 6.5. Stem water content -----

  ggplot(data_new, aes(x=timeStamp)) +
    geom_line(aes(y=RelStemSat, col=TTid), size=0.8) +
    labs(title="Relative Stem Saturation",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Relative stem saturation (%)",
         x="Date") +
    scale_x_datetime(date_breaks = "12 hours", labels= date_format("%b %d-%H:%M")) + #**
    theme(axis.text.x = element_text(angle = 25, vjust=1.0, hjust=1.0, size = 10))

  ggsave(file.path(outPath, "RelStemSaturation.PNG"),
         width = 8, height = 5)

  # 6.6. Light trough the canopy, spectral bands -----

  # Select data only from 10AM to 2PM
  data_new$hour <- paste0(hour(data_new$timeStamp))
  dbspectral <- filter(data_new, hour > 10, hour <= 14 )

  #library(dplyr)
  datalist = list() #initialize the list
  ID <- unique(dbspectral$TTid)
  for (j in 1:length(ID)) {

    # Subset dataset for TTids
    spectrom_L0 <- dbspectral %>% dplyr::filter(TTid == ID[j])

    if (length(spectrom_L0$timeStamp)<10){next}

    #aggregate to daily
    spectrom_L0$Day <- floor_date(spectrom_L0$timeStamp, "day")

    spectrom_L1 <- spectrom_L0 %>%
      group_by(Day)  %>%
      summarize(L450 = median(B450, na.rm = TRUE),
                L500 = median(B500, na.rm = TRUE),
                L550 = median(B550, na.rm = TRUE),
                L570 = median(B570, na.rm = TRUE),
                L600 = median(B600, na.rm = TRUE),
                L650 = median(B650, na.rm = TRUE),
                L610 = median(B610, na.rm = TRUE),
                L680 = median(B680, na.rm = TRUE),
                L730 = median(B730, na.rm = TRUE),
                L760 = median(B760, na.rm = TRUE),
                L810 = median(B810, na.rm = TRUE),
                L860 = median(B860, na.rm = TRUE)
      )
    spectrom_L1$TTid <- rep(ID[j], length(spectrom_L1$Day))

    datalist[[j]] <- spectrom_L1 # add it to your list
  }

  # Average of data between hours, per day
  spectrom_L1_all <- dplyr::bind_rows(datalist)

  # Average between days, bands and TTid
  spectrom_L2_all <- spectrom_L1_all %>%
    group_by(TTid) %>%
    summarise_at(vars("L450", "L500", "L550", "L570", "L600",
                      "L650", "L610", "L680", "L730", "L760",
                      "L810", "L860"), mean)

  # Change column names for wavelength
  Wavelength <- c(450, 500, 550, 570, 600, 650, 610, 680, 730, 760, 810, 860)
  colnames(spectrom_L2_all) <- c("TTid", Wavelength)

  # Reshape df for plot
  #library(reshape2)
  spectral <- melt(spectrom_L2_all, id.vars = "TTid")

  ggplot(spectral, aes(x=variable)) +
    geom_line(aes(y=value, col=TTid, group=TTid), size=0.8) +
    labs(title="Daily average of spectral bands data",
         subtitle = paste0("From: ", Date0, " to ", Date1),
         y="Calibrated spectral bands",
         x="Wavelength (nm)")

  ggsave(file.path(outPath, "SpectralBands.PNG"),
         width = 8, height = 5)

}

## Run the function: ----
suppressWarnings(Plot(TTdb_today, Date0, Date1))


################################################################################
# PART 7: REPORT WITH RMARKDOWN -----------------------
################################################################################

#' Input the folder path with the "TTReport.Rmd" file downloaded
#' to create the report with RMarkdown (TTReport.pdf)

render_path <- file.path("C:/Users/Isabel/Downloads", "TTReport.Rmd")

# Run next lines
suppressWarnings(
  rmarkdown::render(
      render_path,
      output_format = "pdf_document",
      output_file = "TTReport.pdf",
      output_dir = file.path("C:", "TreeTalkerDB")
  )
)



###############################################################################-
#' END OF SCRIPT

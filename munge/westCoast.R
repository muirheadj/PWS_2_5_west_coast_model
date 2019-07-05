# West Coast Epidemiological Model Data ####
# Mark Minton #
# Created 2/12/2019 #

library(RMySQL)
library(reshape2)
library(plyr)
library(xlsx)
library(lubridate)

nvmc.sql <- "SELECT 
    NVMCws_Arrivals.Analysis_year_arrival,
    NVMCws_Arrivals.NVMC_ID,
    NVMCws_Arrivals.Arrival_Port,
    NVMCws_Arrivals.Arrival_Coast,
    NVMCws_Arrivals.Arrival_Date,
    NVMCws_Arrivals.Departure_Date,
    NVMCws_Arrivals.Transit_Type,
    NVMCws_Arrivals.NBIC_Vessel,
    NVMCws_Arrivals.Type,
    NVMCws_Arrivals.Sub_Type
FROM
    NVMCws_Arrivals
WHERE
    (((NVMCws_Arrivals.Analysis_year_arrival) > 2009
        AND (NVMCws_Arrivals.Analysis_year_arrival) < 2018)
        AND ((NVMCws_Arrivals.Status) = 'reviewed')
        AND ((NVMCws_Arrivals.Sub_Type) NOT Like 'Recreational'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Unknown'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Military'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Barge%')
        AND ((NVMCws_Arrivals.Arrival_Coast) = 'west'
          OR (NVMCws_Arrivals.Arrival_Coast) = 'alaska')
        AND ((NVMCws_Arrivals.Transit_Type) LIKE 'c%'
          OR (NVMCws_Arrivals.Transit_Type) LIKE 'o%')
        AND ((NVMCws_Arrivals.NBIC_Vessel) IS NOT NULL));"

nvmc_ca.sql <- paste("
SELECT 
    NVMCws_Arrivals.Analysis_year_arrival,
    NVMCws_Arrivals.NVMC_ID,
    NVMCws_Arrivals.Arrival_Port,
    NVMCws_Arrivals.Arrival_Coast,
    NVMCws_Arrivals.Arrival_Date,
    NVMCws_Arrivals.Departure_Date,
    NVMCws_Arrivals.Transit_Type,
    NVMCws_Arrivals.NBIC_Vessel,
    NVMCws_Arrivals.Type,
    NVMCws_Arrivals.Sub_Type,
    NVMCws_Arrivals.Last_Port,
    NVMCws_Arrivals.Last_Coast,
    NVMCws_Arrivals.Last_Arrive_Date,
    NVMCws_Arrivals.Last_Depart_Date
FROM
    NVMCws_Arrivals
WHERE
    (((NVMCws_Arrivals.Analysis_year_arrival) > 2009
        AND (NVMCws_Arrivals.Analysis_year_arrival) < 2018)
        AND ((NVMCws_Arrivals.Arrival_Coast) = 'ca-west')
        AND ((NVMCws_Arrivals.Sub_Type) NOT Like 'Recreational'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Unknown'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Military'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Barge%')
        AND ((NVMCws_Arrivals.Transit_Type) LIKE 'c%'
          OR (NVMCws_Arrivals.Transit_Type) LIKE 'o%')
        AND ((NVMCws_Arrivals.NBIC_Vessel) IS NOT NULL)
        AND ((NVMCws_Arrivals.Status) = 'reviewed'))
      OR (((NVMCws_Arrivals.Analysis_year_arrival) > 2009
        AND (NVMCws_Arrivals.Analysis_year_arrival) < 2018)        
        AND ((NVMCws_Arrivals.Sub_Type) NOT Like 'Recreational'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Unknown'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Military'
          AND (NVMCws_Arrivals.Sub_Type) NOT Like 'Barge%')
        AND ((NVMCws_Arrivals.Arrival_Coast) = 'alaska'
          OR (NVMCws_Arrivals.Arrival_Coast) = 'west')
        AND ((NVMCws_Arrivals.Transit_Type) LIKE 'c%'
          OR (NVMCws_Arrivals.Transit_Type) LIKE 'o%')
        AND ((NVMCws_Arrivals.NBIC_Vessel) IS NOT NULL)
        AND ((NVMCws_Arrivals.Last_Coast) = 'ca-west')
        AND ((NVMCws_Arrivals.Status) = 'reviewed'));")


con <- dbConnect(RMySQL::MySQL(), group = "nbic analysis", default.file = "C:/my.cnf")
nvmc.orig <- dbGetQuery(con, nvmc.sql)
nvmc_ca.orig <- dbGetQuery(con, nvmc_ca.sql)
dbDisconnect(con)

nvmc <- nvmc.orig
nvmc_ca <- nvmc_ca.orig
str(nvmc)
str(nvmc_ca)

nvmc$Sub_Type[nvmc$Sub_Type == "TugBarge" | nvmc$Sub_Type == "TugOilBarge" | nvmc$Sub_Type == "TugTankBarge"] <- "Tug"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2002410] <- "2011-3-25 10:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2188821] <- "2012-10-04 18:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2772271] <- "2015-11-03 23:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 1895322] <- "2010-05-15 05:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2329012] <- "2017-07-12 22:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2189973] <- "2012-10-08 12:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2194995] <- "2012-10-22 10:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2178169] <- "2012-08-29 17:30:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 1932567] <- "2010-08-24 18:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 1886429] <- "2010-04-21 04:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2911837] <- "2016-08-15 17:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2211644] <- "2012-12-05 17:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2494704] <- "2014-05-30 12:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2299242] <- "2012-05-22 05:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2198192] <- "2012-11-05 15:00:00"
# 
# nvmc$Departure_Date[nvmc$NVMC_ID == 2558710] <- "2014-09-20 13:00:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2759540] <- "2016-10-26 08:59:00"
# nvmc$Departure_Date[nvmc$NVMC_ID == 2185792] <- "2012-10-25 12:00:00"


nvmc$Arrival_Date <- as.POSIXct(nvmc$Arrival_Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
nvmc$Departure_Date <- as.POSIXct(nvmc$Departure_Date, format = "%Y-%m-%d %H:%M:%S", tz = "GMT")
nvmc[is.na(nvmc$Arrival_Date),]
nvmc[is.na(nvmc$Departure_Date),]
# Add 12 hours to Arrival Date to estimate departure date when NULL ####
nvmc$Departure_Date[is.na(nvmc$Departure_Date)] <- nvmc$Arrival_Date[is.na(nvmc$Departure_Date)] + hours(12)
nvmc[is.na(nvmc$Departure_Date),]

nvmc$porttime <- as.numeric(difftime(nvmc$Departure_Date, nvmc$Arrival_Date, units = "hours"))
write.csv(nvmc[(nvmc$porttime < 0 | nvmc$porttime > 1000) & !is.na(nvmc$porttime),],"//Serc4.si.edu/nbic/Admin_REQUESTS/MarineInvasionsLab_Requests/2019/Simkanin/dateproblem.csv", row.names = F)

nvmc <- nvmc[order(nvmc$porttime, decreasing =T),]
dcast(nvmc, Sub_Type ~ Transit_Type, mean)
dcast(nvmc, Type ~ Transit_Type, median)

dcast(nvmc, Type ~ Transit_Type, length)


nvmc[nvmc$porttime < 0 & !is.na(nvmc$porttime),]
# Add 12 hours to departure date when arrival and departue are the same ####
nvmc[nvmc$Arrival_Date == nvmc$Departure_Date,]
nvmc$Departure_Date[nvmc$Arrival_Date == nvmc$Departure_Date] <- nvmc$Departure_Date[nvmc$Arrival_Date == nvmc$Departure_Date] + hours(12)
nvmc[nvmc$Arrival_Date == nvmc$Departure_Date,]

nvmc_ca$Sub_Type[nvmc_ca$Sub_Type == "TugBarge" | nvmc_ca$Sub_Type == "TugOilBarge" | nvmc_ca$Sub_Type == "TugTankBarge"] <- "Tug"
nvmc_ca$Arrival_Date <- as.POSIXct(nvmc_ca$Arrival_Date, format = "%Y-%m-%d %H:%M:%S")
nvmc_ca$Departure_Date <- as.POSIXct(nvmc_ca$Departure_Date, format = "%Y-%m-%d %H:%M:%S")
nvmc_ca$Last_Arrive_Date <- as.POSIXct(nvmc_ca$Last_Arrive_Date, format = "%Y-%m-%d %H:%M:%S")
nvmc_ca$Last_Depart_Date <- as.POSIXct(nvmc_ca$Last_Depart_Date, format = "%Y-%m-%d %H:%M:%S")
nvmc_ca$Last_Depart_Date[nvmc_ca$Last_Arrive_Date == nvmc_ca$Last_Depart_Date & !is.na(nvmc_ca$Last_Arrive_Date) & !is.na(nvmc_ca$Last_Depart_Date)] <- nvmc_ca$Last_Depart_Date[nvmc_ca$Last_Arrive_Date == nvmc_ca$Last_Depart_Date & !is.na(nvmc_ca$Last_Arrive_Date) & !is.na(nvmc_ca$Last_Depart_Date)] + hours(12)
nvmc_ca$porttime <- as.numeric(difftime(nvmc_ca$Last_Depart_Date, nvmc_ca$Last_Arrive_Date, units = "hours"))

nvmc_ca[nvmc_ca$Last_Arrive_Date == nvmc_ca$Last_Depart_Date,]
nvmc_ca <- nvmc_ca[order(nvmc_ca$porttime, decreasing = T),]
nvmc_ca <- nvmc_ca[order(nvmc_ca$porttime, decreasing = F),]
write.csv(nvmc_ca[(nvmc_ca$porttime < 0 | nvmc_ca$porttime > 1000) & !is.na(nvmc_ca$porttime),],"//Serc4.si.edu/nbic/Admin_REQUESTS/MarineInvasionsLab_Requests/2019/Simkanin/CA_dateproblem.csv", row.names = F)
unique(nvmc$Sub_Type)

nvmc.vessel <- dcast(data = nvmc, NBIC_Vessel + Sub_Type ~ Transit_Type, length)
nvmc.vessel$Transit <- ifelse(nvmc.vessel$Coastwise == 0, "Overseas",ifelse(nvmc.vessel$Overseas == 0, "Coastwise","Both"))
nvmc.vessel$total <- nvmc.vessel$Coastwise + nvmc.vessel$Overseas
nvmc.vessel <- nvmc.vessel[order(nvmc.vessel$total, decreasing = TRUE),]
#nvmc.vessel.tab <- ddply(data = nvmc.vessel, .(Sub_Type, Transit), summarize, vessels = length(nvmc.vessel$NBIC_Vessel), arrivals = sum(nvmc.vessel$total))

nvmc.vessel.tab <- dcast(data = nvmc.vessel, Sub_Type ~ Transit, length)
nvmc.vessel.arr <- dcast(data = nvmc.vessel, Sub_Type ~ Transit, sum)

write.xlsx(nvmc.vessel.tab, file = "//serc4.si.edu/nbic/Admin_REQUESTS/MarineInvasionsLab_Requests/2019/Simkanin/vessels.xlsx", sheet = "Vessels", append = F)

write.xlsx(nvmc.vessel.arr, file = "//serc4.si.edu/nbic/Admin_REQUESTS/MarineInvasionsLab_Requests/2019/Simkanin/vessels.xlsx", sheet = "Arrivals", append = T)

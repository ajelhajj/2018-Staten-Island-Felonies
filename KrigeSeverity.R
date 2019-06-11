# Andrea Elhajj
# NYC Crime Data Exploration

setwd("~/CE 395 Env Site Char/ProjRound3")

# Load appropriate libraries
library(readr)
library(ggplot2)
library(dplyr)  
library(plyr) 
library(stringr)  
library(sp)
library(rgdal)
library(raster)
library(DT)
library(rgeos)
library(geosphere)
library(gstat)
library(geoR)
library(MASS)
library(ggmosaic)
library(dismo)
library(automap)
library(leaflet)
library(mapview)

# Bring in crime data.
TheData <- read_csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv")

# Select columns of interest and restrict data to crimes in 2018 on Staten Island
TheData <- TheData %>%
        filter(str_detect(CMPLNT_FR_DT, "2018"), BORO_NM == "STATEN ISLAND", LAW_CAT_CD == "FELONY",
               OFNS_DESC == "NYS LAWS-UNCLASSIFIED FELONY" | OFNS_DESC == "HOMICIDE-NEGLIGENT,UNCLASSIFIE" |
                       OFNS_DESC == "KIDNAPPING & RELATED OFFENSES" | OFNS_DESC == "KIDNAPPING" | OFNS_DESC == "DANGEROUS WEAPONS" | 
                        OFNS_DESC == "GRAND LARCENY" | OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" | 
                       OFNS_DESC == "CRIMINAL MISCHIEF & RELATED OF") %>%
        dplyr::select(KY_CD, OFNS_DESC, Latitude, Longitude)

# Remove any rows with missing coordinates:
TheData <- na.omit(TheData)

#Create "Severity Column," containing a numeric value corresponding to the severity of the crime:
TheData$Severity <- revalue(TheData$OFNS_DESC, c("NYS LAWS-UNCLASSIFIED FELONY" = 1, "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = 1,
                                                 "KIDNAPPING & RELATED OFFENSES" = 2, "KIDNAPPING" = 2,
                                                 "DANGEROUS WEAPONS" = 3, "GRAND LARCENY" = 4, 
                                                 "GRAND LARCENY OF MOTOR VEHICLE" = 4, "CRIMINAL MISCHIEF & RELATED OF" = 5))

# Convert this column from the character class to numeric: 
TheData$Severity <- as.numeric(TheData$Severity)

# Sort the data by ascending order of Longitude
TheData <- TheData[order(TheData$Longitude),]

# Clean data: Remove last row as these longitudinal values correspond 
# to points in the Hudson River and Upper New York Bay
n <- dim(TheData)[1]
TheData <- TheData[1:(n-1),]


# Get the n values for each Crime Severity:
occurences<-table(unlist(TheData$Severity))

# Let R know that the coordinates are spatial points
coords <- TheData[c("Longitude", "Latitude")]
sp <- SpatialPoints(coords)

# Bring in Staten Island Shapefile Boundary
# dsn is the folder the shape files are in. layer is the name of the file.
outline <- readOGR(dsn = "BoroughBoundaries", layer = "StatenIsland")

# ggplot2 works with dataframes, so we need to convert the outline shapefile into a 
# spatial polygons dataframe with the fortify function.
outline_fortify <- fortify(outline)
# Create plot to visualize crime severity

crime_severity_plot <- ggplot() + 
        geom_polygon(data = outline_fortify, aes(x = long, y = lat, group = group, fill = NA), color = "black", fill=NA, size=0.5) + 
        geom_point(data = TheData, size =3, aes(x = Longitude, y = Latitude, color = as.factor(Severity))) +
        coord_map() + ggtitle("Felony Severity") + scale_color_manual(name = "Crime", values=c("firebrick1", "blue", "green", "darkred", "purple"), 
                                                                      labels = c("1 (Murder, n = 44)", "2 (Kidnapping, n = 5)", "3 (Dangerous Weapons, n = 189)", 
                                                                      "4 (Grand Larceny, n = 1259)", "5 (Criminal Mischief, n = 495)")) +
        theme(text = element_text(size=20))
crime_severity_plot

# Gender Exploration: Is suspect gender correlated to type of felony crime committed? 
GenderExploration <- read_csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv") 

# Select columns of interest and restrict data to crimes in 2018 on Staten Island
GenderExploration <- GenderExploration %>%
        filter(str_detect(CMPLNT_FR_DT, "2018"), BORO_NM == "STATEN ISLAND", LAW_CAT_CD == "FELONY",
               OFNS_DESC == "NYS LAWS-UNCLASSIFIED FELONY" | OFNS_DESC == "HOMICIDE-NEGLIGENT,UNCLASSIFIE" |
                       OFNS_DESC == "KIDNAPPING & RELATED OFFENSES" | 
                       OFNS_DESC == "KIDNAPPING" | OFNS_DESC == "DANGEROUS WEAPONS" | 
                       OFNS_DESC == "GRAND LARCENY" | 
                       OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" | 
                       OFNS_DESC == "CRIMINAL MISCHIEF & RELATED OF") %>%
        dplyr::select(OFNS_DESC, SUSP_SEX)

GenderExploration <- GenderExploration %>%
        filter(SUSP_SEX != "U") # Remove rows with unknown suspect gender

#Create "Severity Column," containing a numeric value corresponding to the severity of the crime:
GenderExploration$Severity <- revalue(GenderExploration$OFNS_DESC, c("NYS LAWS-UNCLASSIFIED FELONY" = "1: Murder", "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = "Murder",
                                                 "KIDNAPPING & RELATED OFFENSES" = "2: Kidnapping", "KIDNAPPING" = "2: Kidnapping",
                                                 "DANGEROUS WEAPONS" = "3: Dangerous Weapons", "GRAND LARCENY" = "4: Grand Larceny", 
                                                 "GRAND LARCENY OF MOTOR VEHICLE" = "4: Grand Larceny", "CRIMINAL MISCHIEF & RELATED OF" = "5: Criminal Mischief"))

# Now create a contingency table from these 514 observations:
gendertbl = table(GenderExploration$SUSP_SEX, GenderExploration$Severity)

# Test the hypothesis whether the gender of the suspect is independent of the type of crime committed
# at 0.05 significance level:

chisq.test(gendertbl) 
# Pearson's Chi-squared test
# X-squared = 31.7, df = 5, p-value = 6.813e-06
# Reject the null that suspect sex is independent of type of crime committed.
ggplot(data = GenderExploration) +
        geom_mosaic(aes(x = product(Severity), fill = SUSP_SEX), na.rm=TRUE) + 
        labs(x = "Crime", y = "Gender", title="Suspect Gender and Crime Committed", fill = "Gender") +
        theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y=element_blank())

legend.title = element_blank()
# Age Exploration: Is suspect age correlated to type of felony crime committed? 
AgeExploration <- read_csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv") 

# Select columns of interest and restrict data to crimes in 2018 on Staten Island
AgeExploration <- AgeExploration %>%
        filter(str_detect(CMPLNT_FR_DT, "2018"), BORO_NM == "STATEN ISLAND", LAW_CAT_CD == "FELONY",
               OFNS_DESC == "NYS LAWS-UNCLASSIFIED FELONY" | OFNS_DESC == "HOMICIDE-NEGLIGENT,UNCLASSIFIE" |
                       OFNS_DESC == "KIDNAPPING & RELATED OFFENSES" | 
                       OFNS_DESC == "KIDNAPPING" | OFNS_DESC == "DANGEROUS WEAPONS" | 
                       OFNS_DESC == "GRAND LARCENY" | 
                       OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" | 
                       OFNS_DESC == "CRIMINAL MISCHIEF & RELATED OF") %>%
        dplyr::select(OFNS_DESC, SUSP_AGE_GROUP)

AgeExploration <- AgeExploration %>%
        filter(SUSP_AGE_GROUP != "UNKNOWN") # Remove rows with unknown suspect age group

#Create "Severity Column," containing a numeric value corresponding to the severity of the crime:
AgeExploration$Severity <- revalue(AgeExploration$OFNS_DESC, c("NYS LAWS-UNCLASSIFIED FELONY" = "1: Murder", "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = "Murder",
                                                                     "KIDNAPPING & RELATED OFFENSES" = "2: Kidnapping", "KIDNAPPING" = "2: Kidnapping",
                                                                     "DANGEROUS WEAPONS" = "3: Dangerous Weapons", "GRAND LARCENY" = "4: Grand Larceny", 
                                                                     "GRAND LARCENY OF MOTOR VEHICLE" = "4: Grand Larceny", "CRIMINAL MISCHIEF & RELATED OF" = "5: Criminal Mischief"))

# Now create a contingency table from these 514 observations:
agetbl = table(AgeExploration$SUSP_AGE_GROUP, AgeExploration$Severity)

# Test the hypothesis whether the age range of the suspect is independent of the type of crime committed
# at 0.05 significance level:

chisq.test(agetbl) 
# Pearson's Chi-squared test
# X-squared = 40.79, df = 20, p-value = 0.003964 < 0.05
# Reject the null that suspect age range is independent of type of crime committed.
ggplot(data = AgeExploration) +
        geom_mosaic(aes(x = product(Severity), fill = SUSP_AGE_GROUP), na.rm=TRUE) + 
        labs(x = "Crime", y = "Age Group", title="Suspect Age and Crime Committed", fill = "Age Group") +
        theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y=element_blank())

# Race Exploration: Is suspect race correlated to type of felony crime committed?
RaceExploration <- read_csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv") 

# Select columns of interest and restrict data to crimes in 2018 on Staten Island
RaceExploration <- RaceExploration %>%
        filter(str_detect(CMPLNT_FR_DT, "2018"), BORO_NM == "STATEN ISLAND", LAW_CAT_CD == "FELONY",
               OFNS_DESC == "NYS LAWS-UNCLASSIFIED FELONY" | OFNS_DESC == "HOMICIDE-NEGLIGENT,UNCLASSIFIE" |
                       OFNS_DESC == "KIDNAPPING & RELATED OFFENSES" | 
                       OFNS_DESC == "KIDNAPPING" | OFNS_DESC == "DANGEROUS WEAPONS" | 
                       OFNS_DESC == "GRAND LARCENY" | 
                       OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" | 
                       OFNS_DESC == "CRIMINAL MISCHIEF & RELATED OF") %>%
        dplyr::select(OFNS_DESC, SUSP_RACE)

RaceExploration <- RaceExploration %>%
        filter(SUSP_RACE != "UNKNOWN") # Remove rows with unknown suspect race

#Create "Severity Column," containing a numeric value corresponding to the severity of the crime:
RaceExploration$Severity <- revalue(RaceExploration$OFNS_DESC, c("NYS LAWS-UNCLASSIFIED FELONY" = "1: Murder", "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = "Murder",
                                                                     "KIDNAPPING & RELATED OFFENSES" = "2: Kidnapping", "KIDNAPPING" = "2: Kidnapping",
                                                                     "DANGEROUS WEAPONS" = "3: Dangerous Weapons", "GRAND LARCENY" = "4: Grand Larceny", 
                                                                     "GRAND LARCENY OF MOTOR VEHICLE" = "4: Grand Larceny", "CRIMINAL MISCHIEF & RELATED OF" = "5: Criminal Mischief"))

# Now create a contingency table from these 514 observations:
racetbl = table(RaceExploration$SUSP_RACE, RaceExploration$Severity)

# Test the hypothesis whether the race of the suspect is independent of the type of crime committed
# at 0.05 significance level:

chisq.test(racetbl) 
# Pearson's Chi-squared test
# X-squared = 36.482, df = 20, p-value = 0.01349 < 0.05
# Reject the null that suspect race is independent of type of crime committed.
ggplot(data = RaceExploration) +
        geom_mosaic(aes(x = product(Severity), fill = SUSP_RACE), na.rm=TRUE) + 
        labs(x = "Crime", y = "Race", title="Suspect Race and Crime Committed", fill = "Race") +
        theme(text = element_text(size=20), axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y=element_blank())



rm(list=ls(all=TRUE))

# Bring in crime data again (this time, we want x and y coordinate columns for Kriging, not long/lat columns)
XYData <- read_csv("NYPD_Complaint_Data_Current__Year_To_Date_.csv")

# Select columns of interest and restrict data to crimes in 2018 on Staten Island
XYData <- XYData %>%
        filter(str_detect(CMPLNT_FR_DT, "2018"), BORO_NM == "STATEN ISLAND", LAW_CAT_CD == "FELONY",
               OFNS_DESC == "NYS LAWS-UNCLASSIFIED FELONY" | OFNS_DESC == "HOMICIDE-NEGLIGENT,UNCLASSIFIE" |
                       OFNS_DESC == "KIDNAPPING & RELATED OFFENSES" | OFNS_DESC == "KIDNAPPING" | OFNS_DESC == "DANGEROUS WEAPONS" | 
                       OFNS_DESC == "GRAND LARCENY" | OFNS_DESC == "GRAND LARCENY OF MOTOR VEHICLE" | 
                       OFNS_DESC == "CRIMINAL MISCHIEF & RELATED OF") %>%
        dplyr::select(X_COORD_CD, Y_COORD_CD, OFNS_DESC)

# Remove any rows with missing coordinates:
XYData <- na.omit(XYData)

#Create "Severity Column," containing a numeric value corresponding to the severity of the crime:
XYData$Severity <- revalue(XYData$OFNS_DESC, c("NYS LAWS-UNCLASSIFIED FELONY" = 1, "HOMICIDE-NEGLIGENT,UNCLASSIFIE" = 1,
                                               "KIDNAPPING & RELATED OFFENSES" = 2, "KIDNAPPING" = 2,
                                               "DANGEROUS WEAPONS" = 3, "GRAND LARCENY" = 4, 
                                               "GRAND LARCENY OF MOTOR VEHICLE" = 4, "CRIMINAL MISCHIEF & RELATED OF" = 5))

# Convert this column from the character class to numeric: 
XYData$Severity <- as.numeric(XYData$Severity)

# Ensure pure dataframe not tbl or tbl_df:
XYData <- as.data.frame(XYData)

# Rename headings of x and y for ease of use:
XYData <- XYData %>% 
        dplyr::select(X_COORD_CD, Y_COORD_CD, Severity) %>%
        dplyr::rename(x = X_COORD_CD, y = Y_COORD_CD)

# Sort the data by ascending order of Longitude
XYData <- XYData[order(XYData$x),]

# Clean data: Remove last row as these longitudinal values correspond 
# to points in the Hudson River and Upper New York Bay
n <- dim(XYData)[1]
XYData <- XYData[1:(n-1),]

setwd("~/CE 395 Env Site Char/ProjRound3")

library(geoR)
library(sp)
library(readr)

XYData$x <- XYData$x - min(XYData$x) # want origin of grid to be (0,0)
XYData$y <- XYData$y - min(XYData$y)
XYData$x <- XYData$x*0.0003048 # convert from ft to km
XYData$y <- XYData$y*0.0003048

coordinates(XYData) = ~x+y
XYData <- remove.duplicates(XYData)
mapview(XYData)

geo_data <- as.geodata(XYData)
summary(geo_data) # return a summary of the coordinates and data values

points.geodata(geo_data) # produces a plot showing the data locations
cloud1 <- variog(geo_data, option = "cloud", max.dist=1) # Empirical variograms are calculated 
bin1 <- variog(geo_data, uvec=seq(0,0.5,l=40)) # Return binned results
#plot(cloud1)
plot(bin1)

range = 0.15
nugget = 0.23
sill = 0.59
partial_sill = sill - nugget
method = "cub"

lines.variomodel(cov.model = method, cov.pars = c(partial_sill, range), nugget =nugget, max.dist = 0.5,  lwd = 3)
#smooth <- variog(geo_data, option = "smooth", max.dist = 1, n.points = 100, kernel = "normal", band = 0.2)
#lines(smooth, type ="l", lty = 2)
#legend(0.5, 0.4, c("empirical", "exponential model", "smoothed"), lty = c(1,2), cex=0.7)

# defining the grid
pred.grid <-  expand.grid(seq(0,17, l=200), seq(0,17, l=200))
# kriging calculations
kc <- krige.conv(geo_data, loc = pred.grid, krige = krige.control(cov.model = method, cov.pars = c(partial_sill, range), nugget = nugget))
# displaying predicted values
image(kc, loc = pred.grid, xlab="Coord X (km)", ylab="Coord Y (km)")
contour(kc, add = TRUE)
legend.krige(x.leg=c(1,1.5), y.leg=c(10,15), kc$pred, vert=TRUE)

image(kc, val=kc$krige.var, xlab="Coord X (km)", ylab="Coord Y (km)")
legend.krige(x.leg=c(1,1.5), y.leg=c(10,15), kc$krige.var, vert=TRUE)

# Try to krige over a smaller area for better output:
# defining the grid
pred.grid <-  expand.grid(seq(14,15, l=200), seq(14.5,15.5, l=200))
# kriging calculations
kc <- krige.conv(geo_data, loc = pred.grid, krige = krige.control(cov.model = method, cov.pars = c(partial_sill, range), nugget = nugget))
# displaying predicted values
image(kc, loc = pred.grid, xlab="Coord X (km)", ylab="Coord Y (km)")
#contour(kc, add = TRUE)
legend.krige(x.leg=c(14.02,14.05), y.leg=c(14.5,14.8), kc$pred, vert=TRUE)

image(kc, val=kc$krige.var, xlab="Coord X (km)", ylab="Coord Y (km)")
legend.krige(x.leg=c(14.02,14.05), y.leg=c(14.5,14.8), kc$krige.var, vert=TRUE)


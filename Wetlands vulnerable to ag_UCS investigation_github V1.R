# Author Stacy E. Woods
# For UCS investigation: Wetlands & Agriculture in the Upper Midwest, Dec. 2024 

# Data Sources: 
### Wetlands: Downloaded geodatabases 7/29/24 from National Wetlands Inventory https://www.fws.gov/program/national-wetlands-inventory
### Agriculture: Downloaded 7/31/24 from 2022 Census of Agriculture https://www.nass.usda.gov/Publications/AgCensus/2022/
### State area: Downloaded shapefile 7/31/24 from US Census TIGER/Line Shapefiles database https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

#################################################################
# load packages
library(tidyverse)
library(dplyr)
library(sf)


###########################################################################################
############################################################################################
##################  Study area states with acreage ##################
###########################################################################################
############################################################################################

# load packages
library(tidyverse)
library(dplyr)
library(sf)

# US states
setwd("C:...")

# Bring in shapefile

USstates <- st_read("tl_2023_us_state.shp")

# Is in NAD83, reproject into Projected CRS: NAD_1983_Albers to match wetlands
USstates <- st_transform(USstates, crs = 5070)

# check the reprojection
st_crs(USstates)

colnames(USstates)

# create total State area 
USstates$TotArSqM <- USstates$ALAND + USstates$AWATER

# check it
head(USstates, 10)
# first row and last row check
62266499712  + 489003081
23872589127 +  1030648383

# create total state area in acres from square meters
USstates$TotArAcres <- USstates$TotArSqM/4046.8564224

# check it
head(USstates, 10)
#first row and last row check
62755502793/4046.8564224
24903237510/4046.8564224

colnames(USstates)
str(USstates)

############# Study states ###########

unique(USstates$NAME)

### subset individual states by name

# Iowa
IA <- USstates[USstates$NAME == "Iowa", ]
st_crs(IA)
print(IA)

# Minnesota
MN <- USstates[USstates$NAME == "Minnesota", ]

#Illinois
IL  <- USstates[USstates$NAME == "Illinois", ]

#North Dakota
ND  <- USstates[USstates$NAME == "North Dakota", ]

#South Dakota
SD  <- USstates[USstates$NAME == "South Dakota", ]

#Nebraska
NE <- USstates[USstates$NAME == "Nebraska", ]
  
#Wisconsin
WI <- USstates[USstates$NAME == "Wisconsin", ]

#Michigan
MI <- USstates[USstates$NAME == "Michigan", ]

############# How many acres in states ###########
print(IA$TotArAcres)
print(MN$TotArAcres)
print(IL$TotArAcres)
print(ND$TotArAcres)
print(SD$TotArAcres)
print(WI$TotArAcres)
print(NE$TotArAcres)
print(MI$TotArAcres)

############## export as shapefiles
getwd()
setwd("C:...Upper Midwest states shapefiles")
st_write(IA, "IA.shp")
st_write(MN, "MN.shp")
st_write(IL, "IL.shp")
st_write(ND, "ND.shp")
st_write(SD, "SD.shp")
st_write(WI, "WI.shp")
st_write(NE, "NE.shp")
st_write(MI, "MI.shp")
# warnings to be expected bc values w too many digits for shapefile (eg, area = 146619947556 sq m)



###########################################################################################
############################################################################################
##################  Selecting wetlands by COE definition ##################
###########################################################################################
############################################################################################

# Following Gage et al 2020, remove wetland types riverine, lacustrine, freshwater pond, 
# marine deepwater from NWI to approximate the "3 factor" definition of wetland of 
# army corps of engineers (COE) which administer CWA wetlands regs

# load packages
library(tidyverse)
library(dplyr)
library(sf)

# Load geodatabase layers state by state

###########
### IA ##### 
###########

setwd("C:.../IA_geodatabase_wetlands")

# View layers
IAlayers <- st_layers(dsn = "IA_geodatabase_wetlands.gdb")
# list layers to check layer names
print(IAlayers)

# load layer from geodatabase - takes a while

# use st_read with an SQL query to filter the data so
# you are only loading the features you want (cuts down on time to load data)
IAwetlands <- st_read(dsn = "IA_geodatabase_wetlands.gdb", 
                      layer = "IA_Wetlands", 
                      query = "SELECT * FROM IA_Wetlands WHERE WETLAND_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected:
# "argument layer is ignored" - expected behavior because the SQL query 
# already specifies which layer to work with (can remove 'layer' argument to avoid)
# "polygon with more than 100 parts... processing may be really slow" expected bc a huge
# dataset. There are work arounds but might change geometry handling so will 
# skip it.
print(IAwetlands)

# Clip wetlands to state boundary bc NWI has buffer that overlaps state lines

# first make sure both in same projected CRS
# make target CRS the wetlands data
target_crs <- st_crs(IAwetlands)
#reproject state to same CRS
IA <- st_transform(IA, crs = target_crs)
# verify
print(IA)
print(IAwetlands)
# Projected CRS: NAD_1983_Albers

# Use st_intersection function to clip IAwetlands to the IA layer - Takes a while
# using intersect which retains only portion of wetland w/in boundary, not clip, which would retain
# full wetlands even if portion over boundary
IAwetlands_clipped <- st_intersection(IAwetlands, IA)

# inspect the clipped layer
print(IAwetlands_clipped)

# resulting layer appended data from IA layer
# retain only the columns from the orig wetlands layer
IAwetlands_clipped <- IAwetlands_clipped[, names(IAwetlands)]

# export as shapefile and investigate in Esri
getwd()
setwd("C:...aaa clipped wetlands to states")
st_write(IAwetlands_clipped, "IAwetlandsClipped.shp")


###########
### MN ##### 
###########

setwd("C:.../MN_geodatabase_wetlands")

# View layers
MNlayers <- st_layers(dsn = "MN_geodatabase_wetlands.gdb")
# list layers to check layer names
print(MNlayers)

# load layer from geodatabase - 
MNwetlands <- st_read(dsn = "MN_geodatabase_wetlands.gdb", 
                      layer = "MN_Wetlands", 
                      query = "SELECT * FROM MN_Wetlands WHERE WETLAND_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(MNwetlands)

# Note Projected CRS: NAD_1983_Albers

### Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(MNwetlands)
#reproject state to same CRS
MN <- st_transform(MN, crs = target_crs)
# verify
print(MN)
print(MNwetlands)
# Projected CRS: NAD_1983_Albers

# close other large programs (eg, ArcGIS Pro)

# Divide into ten parts bc is huge dataset and need to intersect

# Step 1: Split the df into parts
# Calculate the breakpoints for dividing the dataframe into 10 parts
tenth_point1 <- floor(nrow(MNwetlands) / 10)
tenth_point2 <- floor(2 * nrow(MNwetlands) / 10)
tenth_point3 <- floor(3 * nrow(MNwetlands) / 10)
tenth_point4 <- floor(4 * nrow(MNwetlands) / 10)
tenth_point5 <- floor(5 * nrow(MNwetlands) / 10)
tenth_point6 <- floor(6 * nrow(MNwetlands) / 10)
tenth_point7 <- floor(7 * nrow(MNwetlands) / 10)
tenth_point8 <- floor(8 * nrow(MNwetlands) / 10)
tenth_point9 <- floor(9 * nrow(MNwetlands) / 10)

# Create the 10 parts
MNwetlands_tenth1 <- MNwetlands[1:tenth_point1, ]
MNwetlands_tenth2 <- MNwetlands[(tenth_point1 + 1):tenth_point2, ]
MNwetlands_tenth3 <- MNwetlands[(tenth_point2 + 1):tenth_point3, ]
MNwetlands_tenth4 <- MNwetlands[(tenth_point3 + 1):tenth_point4, ]
MNwetlands_tenth5 <- MNwetlands[(tenth_point4 + 1):tenth_point5, ]
MNwetlands_tenth6 <- MNwetlands[(tenth_point5 + 1):tenth_point6, ]
MNwetlands_tenth7 <- MNwetlands[(tenth_point6 + 1):tenth_point7, ]
MNwetlands_tenth8 <- MNwetlands[(tenth_point7 + 1):tenth_point8, ]
MNwetlands_tenth9 <- MNwetlands[(tenth_point8 + 1):tenth_point9, ]
MNwetlands_tenth10 <- MNwetlands[(tenth_point9 + 1):nrow(MNwetlands), ]
                                
# check the number of features add up to MNwetlands
nrow(MNwetlands_tenth1)+ nrow(MNwetlands_tenth2)+ nrow(MNwetlands_tenth3)+
  nrow(MNwetlands_tenth4) + nrow(MNwetlands_tenth5) + nrow(MNwetlands_tenth6) +
  nrow(MNwetlands_tenth7) + nrow(MNwetlands_tenth8) + nrow(MNwetlands_tenth9) +
  nrow(MNwetlands_tenth10)

# Step 2: Crop each piece using the state boundary using intersection to keep only portion w/in state
MNwetlands_tenth1_cropped <- st_intersection(MNwetlands_tenth1, MN)

MNwetlands_tenth2_cropped <- st_intersection(MNwetlands_tenth2, MN)

MNwetlands_tenth3_cropped <- st_intersection(MNwetlands_tenth3, MN)

MNwetlands_tenth4_cropped <- st_intersection(MNwetlands_tenth4, MN)

MNwetlands_tenth5_cropped <- st_intersection(MNwetlands_tenth5, MN)

MNwetlands_tenth6_cropped <- st_intersection(MNwetlands_tenth6, MN)

MNwetlands_tenth7_cropped <- st_intersection(MNwetlands_tenth7, MN)

MNwetlands_tenth8_cropped <- st_intersection(MNwetlands_tenth8, MN)

MNwetlands_tenth9_cropped <- st_intersection(MNwetlands_tenth9, MN)

MNwetlands_tenth10_cropped <- st_intersection(MNwetlands_tenth10, MN)

#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(MNwetlands)
original_vars

# Step 2: Subset the cropped df to retain only the original variables
MNwetlands_tenth1_cropped_subset <- MNwetlands_tenth1_cropped[, original_vars]
colnames(MNwetlands_tenth1_cropped_subset)

MNwetlands_tenth2_cropped_subset <- MNwetlands_tenth2_cropped[, original_vars]
MNwetlands_tenth3_cropped_subset <- MNwetlands_tenth3_cropped[, original_vars]
MNwetlands_tenth4_cropped_subset <- MNwetlands_tenth4_cropped[, original_vars]
MNwetlands_tenth5_cropped_subset <- MNwetlands_tenth5_cropped[, original_vars]
MNwetlands_tenth6_cropped_subset <- MNwetlands_tenth6_cropped[, original_vars]
MNwetlands_tenth7_cropped_subset <- MNwetlands_tenth7_cropped[, original_vars]
MNwetlands_tenth8_cropped_subset <- MNwetlands_tenth8_cropped[, original_vars]
MNwetlands_tenth9_cropped_subset <- MNwetlands_tenth9_cropped[, original_vars]
MNwetlands_tenth10_cropped_subset <- MNwetlands_tenth10_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")

st_write(MNwetlands_tenth1_cropped_subset, "MNwetlandsClipped_part 1.shp")
st_write(MNwetlands_tenth2_cropped_subset, "MNwetlandsClipped_part 2.shp")
st_write(MNwetlands_tenth3_cropped_subset, "MNwetlandsClipped_part 3.shp")
st_write(MNwetlands_tenth4_cropped_subset, "MNwetlandsClipped_part 4.shp")
st_write(MNwetlands_tenth5_cropped_subset, "MNwetlandsClipped_part 5.shp")
st_write(MNwetlands_tenth6_cropped_subset, "MNwetlandsClipped_part 6.shp")
st_write(MNwetlands_tenth7_cropped_subset, "MNwetlandsClipped_part 7.shp")
st_write(MNwetlands_tenth8_cropped_subset, "MNwetlandsClipped_part 8.shp")
st_write(MNwetlands_tenth9_cropped_subset, "MNwetlandsClipped_part 9.shp")
st_write(MNwetlands_tenth10_cropped_subset, "MNwetlandsClipped_part 10.shp")
# expected errors: abbreviating field names and some values cropped
# for decimal places (usually in field area which I'm not using)


###########
### IL ##### 
###########

setwd("C:.../IL_geodatabase_wetlands")

# View layers
ILlayers <- st_layers(dsn = "IL_geodatabase_wetlands.gdb")
# list layers to check layer names
print(ILlayers)

# load layer from geodatabase - 
ILwetlands <- st_read(dsn = "IL_geodatabase_wetlands.gdb", 
                      layer = "IL_Wetlands", 
                      query = "SELECT * FROM IL_Wetlands WHERE WETLAND_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(ILwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(ILwetlands)
#reproject state to same CRS
IL <- st_transform(IL, crs = target_crs)
# verify
print(IL)
print(ILwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into thirds
third_point1 <- floor(nrow(ILwetlands) / 3)
third_point2 <- floor(2 * nrow(ILwetlands) / 3)

ILwetlands_third1 <- ILwetlands[1:third_point1, ]
ILwetlands_third2 <- ILwetlands[(third_point1 + 1):third_point2, ]
ILwetlands_third3 <- ILwetlands[(third_point2 + 1):nrow(ILwetlands), ]

# check the number of features add up
nrow(ILwetlands_third1)+ nrow(ILwetlands_third2)+ nrow(ILwetlands_third3)

# Step 2: Crop each piece using the state boundary 
ILwetlands_third1_cropped <- st_intersection(ILwetlands_third1, IL)
ILwetlands_third2_cropped <- st_intersection(ILwetlands_third2, IL)
ILwetlands_third3_cropped <- st_intersection(ILwetlands_third3, IL)

#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(ILwetlands)
original_vars

# Step 2: Subset the cropped dataframe to retain only the original variables
ILwetlands_third1_cropped_subset <- ILwetlands_third1_cropped[, original_vars]
colnames(ILwetlands_third1_cropped_subset)

ILwetlands_third2_cropped_subset <- ILwetlands_third2_cropped[, original_vars]
ILwetlands_third3_cropped_subset <- ILwetlands_third3_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")
st_write(ILwetlands_third1_cropped_subset, "ILwetlandsClipped_part 1.shp")
st_write(ILwetlands_third2_cropped_subset, "ILwetlandsClipped_part 2.shp")
st_write(ILwetlands_third3_cropped_subset, "ILwetlandsClipped_part 3.shp")


###########
### ND ##### 
###########

setwd("C:.../ND_geodatabase_wetlands")

# View layers
NDlayers <- st_layers(dsn = "ND_geodatabase_wetlands.gdb")
# list layers to check layer names
print(NDlayers)

# load layer from geodatabase - 
NDwetlands <- st_read(dsn = "ND_geodatabase_wetlands.gdb", 
                      layer = "ND_Wetlands", 
                      query = "SELECT * FROM ND_Wetlands WHERE WETLAND_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(NDwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(NDwetlands)
#reproject state to same CRS
ND <- st_transform(ND, crs = target_crs)
# verify
print(ND)
print(NDwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into parts
# Calculate the breakpoints for dividing the dataframe into parts
tenth_point1 <- floor(nrow(NDwetlands) / 10)
tenth_point2 <- floor(2 * nrow(NDwetlands) /10)
tenth_point3 <- floor(3 * nrow(NDwetlands) /10)
tenth_point4 <- floor(4 * nrow(NDwetlands) /10)
tenth_point5 <- floor(5 * nrow(NDwetlands) /10)
tenth_point6 <- floor(6 * nrow(NDwetlands) /10)
tenth_point7 <- floor(7 * nrow(NDwetlands) /10)
tenth_point8 <- floor(8 * nrow(NDwetlands) /10)
tenth_point9 <- floor(9 * nrow(NDwetlands) /10)

# Create the parts
NDwetlands_tenth1 <- NDwetlands[1:tenth_point1, ]
NDwetlands_tenth2 <- NDwetlands[(tenth_point1 + 1):tenth_point2, ]
NDwetlands_tenth3 <- NDwetlands[(tenth_point2 + 1):tenth_point3, ]
NDwetlands_tenth4 <- NDwetlands[(tenth_point3 + 1):tenth_point4, ]
NDwetlands_tenth5 <- NDwetlands[(tenth_point4 + 1):tenth_point5, ]
NDwetlands_tenth6 <- NDwetlands[(tenth_point5 + 1):tenth_point6, ]
NDwetlands_tenth7 <- NDwetlands[(tenth_point6 + 1):tenth_point7, ]
NDwetlands_tenth8 <- NDwetlands[(tenth_point7 + 1):tenth_point8, ]
NDwetlands_tenth9 <- NDwetlands[(tenth_point8 + 1):tenth_point9, ]
NDwetlands_tenth10 <- NDwetlands[(tenth_point9 + 1):nrow(NDwetlands), ]

# check the number of features add up to NDwetlands
nrow(NDwetlands_tenth1)+ nrow(NDwetlands_tenth2)+ nrow(NDwetlands_tenth3)+
  nrow(NDwetlands_tenth4) + nrow(NDwetlands_tenth5) + nrow(NDwetlands_tenth6) +
  nrow(NDwetlands_tenth7) + nrow(NDwetlands_tenth8) + nrow(NDwetlands_tenth9) +
  nrow(NDwetlands_tenth10)

# Step 2: Crop each piece using the state boundary using intersection to keep only portion w/in state
NDwetlands_tenth1_cropped <- st_intersection(NDwetlands_tenth1, ND)

NDwetlands_tenth2_cropped <- st_intersection(NDwetlands_tenth2, ND)

NDwetlands_tenth3_cropped <- st_intersection(NDwetlands_tenth3, ND)

NDwetlands_tenth4_cropped <- st_intersection(NDwetlands_tenth4, ND)

NDwetlands_tenth5_cropped <- st_intersection(NDwetlands_tenth5, ND)

NDwetlands_tenth6_cropped <- st_intersection(NDwetlands_tenth6, ND)

NDwetlands_tenth7_cropped <- st_intersection(NDwetlands_tenth7, ND)

NDwetlands_tenth8_cropped <- st_intersection(NDwetlands_tenth8, ND)

NDwetlands_tenth9_cropped <- st_intersection(NDwetlands_tenth9, ND)
NDwetlands_tenth10_cropped <- st_intersection(NDwetlands_tenth10, ND)

#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(NDwetlands)
original_vars

# Step 2: Subset the cropped df to retain only the original variables
NDwetlands_tenth1_cropped_subset <- NDwetlands_tenth1_cropped[, original_vars]
colnames(NDwetlands_tenth1_cropped_subset)

NDwetlands_tenth2_cropped_subset <- NDwetlands_tenth2_cropped[, original_vars]
NDwetlands_tenth3_cropped_subset <- NDwetlands_tenth3_cropped[, original_vars]
NDwetlands_tenth4_cropped_subset <- NDwetlands_tenth4_cropped[, original_vars]
NDwetlands_tenth5_cropped_subset <- NDwetlands_tenth5_cropped[, original_vars]
NDwetlands_tenth6_cropped_subset <- NDwetlands_tenth6_cropped[, original_vars]
NDwetlands_tenth7_cropped_subset <- NDwetlands_tenth7_cropped[, original_vars]
NDwetlands_tenth8_cropped_subset <- NDwetlands_tenth8_cropped[, original_vars]
NDwetlands_tenth9_cropped_subset <- NDwetlands_tenth9_cropped[, original_vars]
NDwetlands_tenth10_cropped_subset <- NDwetlands_tenth10_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")

st_write(NDwetlands_tenth1_cropped_subset, "NDwetlandsClipped_part 1.shp")
st_write(NDwetlands_tenth2_cropped_subset, "NDwetlandsClipped_part 2.shp")
st_write(NDwetlands_tenth3_cropped_subset, "NDwetlandsClipped_part 3.shp")
st_write(NDwetlands_tenth4_cropped_subset, "NDwetlandsClipped_part 4.shp")
st_write(NDwetlands_tenth5_cropped_subset, "NDwetlandsClipped_part 5.shp")
st_write(NDwetlands_tenth6_cropped_subset, "NDwetlandsClipped_part 6.shp")
st_write(NDwetlands_tenth7_cropped_subset, "NDwetlandsClipped_part 7.shp")
st_write(NDwetlands_tenth8_cropped_subset, "NDwetlandsClipped_part 8.shp")
st_write(NDwetlands_tenth9_cropped_subset, "NDwetlandsClipped_part 9.shp")
st_write(NDwetlands_tenth10_cropped_subset, "NDwetlandsClipped_part 10.shp")

###########
### SD ##### 
###########

setwd("C:.../SD_geodatabase_wetlands")

# View layers
SDlayers <- st_layers(dsn = "SD_geodatabase_wetlands.gdb")
# list layers to check layer names
print(SDlayers)

# load layer from geodatabase
SDwetlands <- st_read(dsn = "SD_geodatabase_wetlands.gdb", 
                      layer = "South_Dakota_Wetlands", 
                      query = "SELECT * FROM South_Dakota_Wetlands WHERE WETLand_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(SDwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(SDwetlands)
#reproject state to same CRS
SD <- st_transform(SD, crs = target_crs)
# verify
print(SD)
print(SDwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into parts
# Calculate the breakpoints for dividing the dataframe into parts
tenth_point1 <- floor(nrow(SDwetlands) / 10)
tenth_point2 <- floor(2 * nrow(SDwetlands) /10)
tenth_point3 <- floor(3 * nrow(SDwetlands) /10)
tenth_point4 <- floor(4 * nrow(SDwetlands) /10)
tenth_point5 <- floor(5 * nrow(SDwetlands) /10)
tenth_point6 <- floor(6 * nrow(SDwetlands) /10)
tenth_point7 <- floor(7 * nrow(SDwetlands) /10)
tenth_point8 <- floor(8 * nrow(SDwetlands) /10)
tenth_point9 <- floor(9 * nrow(SDwetlands) /10)

# Create the parts
SDwetlands_tenth1 <- SDwetlands[1:tenth_point1, ]
SDwetlands_tenth2 <- SDwetlands[(tenth_point1 + 1):tenth_point2, ]
SDwetlands_tenth3 <- SDwetlands[(tenth_point2 + 1):tenth_point3, ]
SDwetlands_tenth4 <- SDwetlands[(tenth_point3 + 1):tenth_point4, ]
SDwetlands_tenth5 <- SDwetlands[(tenth_point4 + 1):tenth_point5, ]
SDwetlands_tenth6 <- SDwetlands[(tenth_point5 + 1):tenth_point6, ]
SDwetlands_tenth7 <- SDwetlands[(tenth_point6 + 1):tenth_point7, ]
SDwetlands_tenth8 <- SDwetlands[(tenth_point7 + 1):tenth_point8, ]
SDwetlands_tenth9 <- SDwetlands[(tenth_point8 + 1):tenth_point9, ]
SDwetlands_tenth10 <- SDwetlands[(tenth_point9 + 1):nrow(SDwetlands), ]

# check the number of features add up to SDwetlands
nrow(SDwetlands_tenth1)+ nrow(SDwetlands_tenth2)+ nrow(SDwetlands_tenth3)+
  nrow(SDwetlands_tenth4) + nrow(SDwetlands_tenth5) + nrow(SDwetlands_tenth6) +
  nrow(SDwetlands_tenth7) + nrow(SDwetlands_tenth8) + nrow(SDwetlands_tenth9) +
  nrow(SDwetlands_tenth10)

# Step 2: Crop each piece using the state bouSDary using intersection to keep only portion w/in state
SDwetlands_tenth1_cropped <- st_intersection(SDwetlands_tenth1, SD)

SDwetlands_tenth2_cropped <- st_intersection(SDwetlands_tenth2, SD)

SDwetlands_tenth3_cropped <- st_intersection(SDwetlands_tenth3, SD)

SDwetlands_tenth4_cropped <- st_intersection(SDwetlands_tenth4, SD)

SDwetlands_tenth5_cropped <- st_intersection(SDwetlands_tenth5, SD)

SDwetlands_tenth6_cropped <- st_intersection(SDwetlands_tenth6, SD)

SDwetlands_tenth7_cropped <- st_intersection(SDwetlands_tenth7, SD)

SDwetlands_tenth8_cropped <- st_intersection(SDwetlands_tenth8, SD)

SDwetlands_tenth9_cropped <- st_intersection(SDwetlands_tenth9, SD)
SDwetlands_tenth10_cropped <- st_intersection(SDwetlands_tenth10, SD)

# Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(SDwetlands)
original_vars

# Step 2: Subset the cropped df to retain only the original variables
SDwetlands_tenth1_cropped_subset <- SDwetlands_tenth1_cropped[, original_vars]
colnames(SDwetlands_tenth1_cropped_subset)

SDwetlands_tenth2_cropped_subset <- SDwetlands_tenth2_cropped[, original_vars]
SDwetlands_tenth3_cropped_subset <- SDwetlands_tenth3_cropped[, original_vars]
SDwetlands_tenth4_cropped_subset <- SDwetlands_tenth4_cropped[, original_vars]
SDwetlands_tenth5_cropped_subset <- SDwetlands_tenth5_cropped[, original_vars]
SDwetlands_tenth6_cropped_subset <- SDwetlands_tenth6_cropped[, original_vars]
SDwetlands_tenth7_cropped_subset <- SDwetlands_tenth7_cropped[, original_vars]
SDwetlands_tenth8_cropped_subset <- SDwetlands_tenth8_cropped[, original_vars]
SDwetlands_tenth9_cropped_subset <- SDwetlands_tenth9_cropped[, original_vars]
SDwetlands_tenth10_cropped_subset <- SDwetlands_tenth10_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")

st_write(SDwetlands_tenth1_cropped_subset, "SDwetlandsClipped_part 1.shp")
st_write(SDwetlands_tenth2_cropped_subset, "SDwetlandsClipped_part 2.shp")
st_write(SDwetlands_tenth3_cropped_subset, "SDwetlandsClipped_part 3.shp")
st_write(SDwetlands_tenth4_cropped_subset, "SDwetlandsClipped_part 4.shp")
st_write(SDwetlands_tenth5_cropped_subset, "SDwetlandsClipped_part 5.shp")
st_write(SDwetlands_tenth6_cropped_subset, "SDwetlandsClipped_part 6.shp")
st_write(SDwetlands_tenth7_cropped_subset, "SDwetlandsClipped_part 7.shp")
st_write(SDwetlands_tenth8_cropped_subset, "SDwetlandsClipped_part 8.shp")
st_write(SDwetlands_tenth9_cropped_subset, "SDwetlandsClipped_part 9.shp")
st_write(SDwetlands_tenth10_cropped_subset, "SDwetlandsClipped_part 10.shp")



###########
### WI ##### 
###########

setwd("C:.../WI_geodatabase_wetlands")

# View layers
WIlayers <- st_layers(dsn = "WI_geodatabase_wetlands.gdb")
# list layers to check layer names
print(WIlayers)

# load layer from geodatabase 
WIwetlands <- st_read(dsn = "WI_geodatabase_wetlands.gdb", 
                      layer = " WI_Wetlands", 
                      query = "SELECT * FROM  WI_Wetlands WHERE WETLand_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(WIwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(WIwetlands)
#reproject state to same CRS
WI <- st_transform(WI, crs = target_crs)
# verify
print(WI)
print(WIwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into parts
# Calculate the breakpoints for dividing the dataframe into parts
tenth_point1 <- floor(nrow(WIwetlands) / 10)
tenth_point2 <- floor(2 * nrow(WIwetlands) /10)
tenth_point3 <- floor(3 * nrow(WIwetlands) /10)
tenth_point4 <- floor(4 * nrow(WIwetlands) /10)
tenth_point5 <- floor(5 * nrow(WIwetlands) /10)
tenth_point6 <- floor(6 * nrow(WIwetlands) /10)
tenth_point7 <- floor(7 * nrow(WIwetlands) /10)
tenth_point8 <- floor(8 * nrow(WIwetlands) /10)
tenth_point9 <- floor(9 * nrow(WIwetlands) /10)

# Create the parts
WIwetlands_tenth1 <- WIwetlands[1:tenth_point1, ]
WIwetlands_tenth2 <- WIwetlands[(tenth_point1 + 1):tenth_point2, ]
WIwetlands_tenth3 <- WIwetlands[(tenth_point2 + 1):tenth_point3, ]
WIwetlands_tenth4 <- WIwetlands[(tenth_point3 + 1):tenth_point4, ]
WIwetlands_tenth5 <- WIwetlands[(tenth_point4 + 1):tenth_point5, ]
WIwetlands_tenth6 <- WIwetlands[(tenth_point5 + 1):tenth_point6, ]
WIwetlands_tenth7 <- WIwetlands[(tenth_point6 + 1):tenth_point7, ]
WIwetlands_tenth8 <- WIwetlands[(tenth_point7 + 1):tenth_point8, ]
WIwetlands_tenth9 <- WIwetlands[(tenth_point8 + 1):tenth_point9, ]
WIwetlands_tenth10 <- WIwetlands[(tenth_point9 + 1):nrow(WIwetlands), ]

# check the number of features add up to WIwetlands
nrow(WIwetlands_tenth1)+ nrow(WIwetlands_tenth2)+ nrow(WIwetlands_tenth3)+
  nrow(WIwetlands_tenth4) + nrow(WIwetlands_tenth5) + nrow(WIwetlands_tenth6) +
  nrow(WIwetlands_tenth7) + nrow(WIwetlands_tenth8) + nrow(WIwetlands_tenth9) +
  nrow(WIwetlands_tenth10)

# Step 2: Crop each piece using the state boundary using intersection to keep only portion w/in state
WIwetlands_tenth1_cropped <- st_intersection(WIwetlands_tenth1, WI)

WIwetlands_tenth2_cropped <- st_intersection(WIwetlands_tenth2, WI)

WIwetlands_tenth3_cropped <- st_intersection(WIwetlands_tenth3, WI)

WIwetlands_tenth4_cropped <- st_intersection(WIwetlands_tenth4, WI)

WIwetlands_tenth5_cropped <- st_intersection(WIwetlands_tenth5, WI)

WIwetlands_tenth6_cropped <- st_intersection(WIwetlands_tenth6, WI)

WIwetlands_tenth7_cropped <- st_intersection(WIwetlands_tenth7, WI)

WIwetlands_tenth8_cropped <- st_intersection(WIwetlands_tenth8, WI)

WIwetlands_tenth9_cropped <- st_intersection(WIwetlands_tenth9, WI)

WIwetlands_tenth10_cropped <- st_intersection(WIwetlands_tenth10, WI)


#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(WIwetlands)
original_vars

# Step 2: Subset the cropped df to retain only the original variables
WIwetlands_tenth1_cropped_subset <- WIwetlands_tenth1_cropped[, original_vars]
colnames(WIwetlands_tenth1_cropped_subset)

WIwetlands_tenth2_cropped_subset <- WIwetlands_tenth2_cropped[, original_vars]
WIwetlands_tenth3_cropped_subset <- WIwetlands_tenth3_cropped[, original_vars]
WIwetlands_tenth4_cropped_subset <- WIwetlands_tenth4_cropped[, original_vars]
WIwetlands_tenth5_cropped_subset <- WIwetlands_tenth5_cropped[, original_vars]
WIwetlands_tenth6_cropped_subset <- WIwetlands_tenth6_cropped[, original_vars]
WIwetlands_tenth7_cropped_subset <- WIwetlands_tenth7_cropped[, original_vars]
WIwetlands_tenth8_cropped_subset <- WIwetlands_tenth8_cropped[, original_vars]
WIwetlands_tenth9_cropped_subset <- WIwetlands_tenth9_cropped[, original_vars]
WIwetlands_tenth10_cropped_subset <- WIwetlands_tenth10_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")

st_write(WIwetlands_tenth1_cropped_subset, "WIwetlandsClipped_part 1.shp")
st_write(WIwetlands_tenth2_cropped_subset, "WIwetlandsClipped_part 2.shp")
st_write(WIwetlands_tenth3_cropped_subset, "WIwetlandsClipped_part 3.shp")
st_write(WIwetlands_tenth4_cropped_subset, "WIwetlandsClipped_part 4.shp")
st_write(WIwetlands_tenth5_cropped_subset, "WIwetlandsClipped_part 5.shp")
st_write(WIwetlands_tenth6_cropped_subset, "WIwetlandsClipped_part 6.shp")
st_write(WIwetlands_tenth7_cropped_subset, "WIwetlandsClipped_part 7.shp")
st_write(WIwetlands_tenth8_cropped_subset, "WIwetlandsClipped_part 8.shp")
st_write(WIwetlands_tenth9_cropped_subset, "WIwetlandsClipped_part 9.shp")
st_write(WIwetlands_tenth10_cropped_subset, "WIwetlandsClipped_part 10.shp")

###########
### NE ##### 
###########

setwd("C:.../NE_geodatabase_wetlands")

# View layers
NElayers <- st_layers(dsn = "NE_geodatabase_wetlands.gdb")
# list layers to check layer names
print(NElayers)

# load layer from geodatabase 
NEwetlands <- st_read(dsn = "NE_geodatabase_wetlands.gdb", 
                      layer = "Nebraska_Wetlands", 
                      query = "SELECT * FROM Nebraska_Wetlands WHERE WETLAND_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(NEwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(NEwetlands)
#reproject state to same CRS
NE <- st_transform(NE, crs = target_crs)
# verify
print(NE)
print(NEwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into thirds
third_point1 <- floor(nrow(NEwetlands) / 3)
third_point2 <- floor(2 * nrow(NEwetlands) / 3)

NEwetlands_third1 <- NEwetlands[1:third_point1, ]
NEwetlands_third2 <- NEwetlands[(third_point1 + 1):third_point2, ]
NEwetlands_third3 <- NEwetlands[(third_point2 + 1):nrow(NEwetlands), ]

# check the number of features add up
nrow(NEwetlands_third1)+ nrow(NEwetlands_third2)+ nrow(NEwetlands_third3)

# Step 2: Crop each piece using the state boundary 
NEwetlands_third1_cropped <- st_intersection(NEwetlands_third1, NE)
NEwetlands_third2_cropped <- st_intersection(NEwetlands_third2, NE)
NEwetlands_third3_cropped <- st_intersection(NEwetlands_third3, NE)

#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(NEwetlands)
original_vars

# Step 2: Subset the cropped dataframe to retain only the original variables
NEwetlands_third1_cropped_subset <- NEwetlands_third1_cropped[, original_vars]
colnames(NEwetlands_third1_cropped_subset)
NEwetlands_third2_cropped_subset <- NEwetlands_third2_cropped[, original_vars]
NEwetlands_third3_cropped_subset <- NEwetlands_third3_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefNEe
setwd("C:...aaa clipped wetlands to states")
st_write(NEwetlands_third1_cropped_subset, "NEwetlandsClipped_part 1.shp")
st_write(NEwetlands_third2_cropped_subset, "NEwetlandsClipped_part 2.shp")
st_write(NEwetlands_third3_cropped_subset, "NEwetlandsClipped_part 3.shp")

########
## MI ##
########

setwd("C:.../MI_geodatabase_wetlands")

# View layers
MIlayers <- st_layers(dsn = "MI_geodatabase_wetlands.gdb")
# list layers to check layer names
print(MIlayers)

# load layer from geodatabase 
MIwetlands <- st_read(dsn = "MI_geodatabase_wetlands.gdb", 
                      layer = " MI_Wetlands", 
                      query = "SELECT * FROM  MI_Wetlands WHERE WETLand_TYPE IN ('Freshwater Emergent Wetland', 'Freshwater Forested/Shrub Wetland', 'Other')")
# Resulting warnings expected
print(MIwetlands)

# Note Projected CRS: NAD_1983_Albers

# Clip wetlands to state boundary

# make target CRS the wetlands data
target_crs <- st_crs(MIwetlands)
#reproject state to same CRS
MI <- st_transform(MI, crs = target_crs)
# verify
print(MI)
print(MIwetlands)
# Projected CRS: NAD_1983_Albers

# Clip to state boundaries

# Step 1: Split the df into parts
# Calculate the breakpoints for dividing the dataframe into parts
tenth_point1 <- floor(nrow(MIwetlands) / 10)
tenth_point2 <- floor(2 * nrow(MIwetlands) /10)
tenth_point3 <- floor(3 * nrow(MIwetlands) /10)
tenth_point4 <- floor(4 * nrow(MIwetlands) /10)
tenth_point5 <- floor(5 * nrow(MIwetlands) /10)
tenth_point6 <- floor(6 * nrow(MIwetlands) /10)
tenth_point7 <- floor(7 * nrow(MIwetlands) /10)
tenth_point8 <- floor(8 * nrow(MIwetlands) /10)
tenth_point9 <- floor(9 * nrow(MIwetlands) /10)

# Create the parts
MIwetlands_tenth1 <- MIwetlands[1:tenth_point1, ]
MIwetlands_tenth2 <- MIwetlands[(tenth_point1 + 1):tenth_point2, ]
MIwetlands_tenth3 <- MIwetlands[(tenth_point2 + 1):tenth_point3, ]
MIwetlands_tenth4 <- MIwetlands[(tenth_point3 + 1):tenth_point4, ]
MIwetlands_tenth5 <- MIwetlands[(tenth_point4 + 1):tenth_point5, ]
MIwetlands_tenth6 <- MIwetlands[(tenth_point5 + 1):tenth_point6, ]
MIwetlands_tenth7 <- MIwetlands[(tenth_point6 + 1):tenth_point7, ]
MIwetlands_tenth8 <- MIwetlands[(tenth_point7 + 1):tenth_point8, ]
MIwetlands_tenth9 <- MIwetlands[(tenth_point8 + 1):tenth_point9, ]
MIwetlands_tenth10 <- MIwetlands[(tenth_point9 + 1):nrow(MIwetlands), ]

# check the number of features add up to MIwetlands
nrow(MIwetlands_tenth1)+ nrow(MIwetlands_tenth2)+ nrow(MIwetlands_tenth3)+
  nrow(MIwetlands_tenth4) + nrow(MIwetlands_tenth5) + nrow(MIwetlands_tenth6) +
  nrow(MIwetlands_tenth7) + nrow(MIwetlands_tenth8) + nrow(MIwetlands_tenth9) +
  nrow(MIwetlands_tenth10)

# Step 2: Crop each piece using the state boundary using intersection to keep only portion w/in state
MIwetlands_tenth1_cropped <- st_intersection(MIwetlands_tenth1, MI)

MIwetlands_tenth2_cropped <- st_intersection(MIwetlands_tenth2, MI)

MIwetlands_tenth3_cropped <- st_intersection(MIwetlands_tenth3, MI)

MIwetlands_tenth4_cropped <- st_intersection(MIwetlands_tenth4, MI)

MIwetlands_tenth5_cropped <- st_intersection(MIwetlands_tenth5, MI)

MIwetlands_tenth6_cropped <- st_intersection(MIwetlands_tenth6, MI)

MIwetlands_tenth7_cropped <- st_intersection(MIwetlands_tenth7, MI)

MIwetlands_tenth8_cropped <- st_intersection(MIwetlands_tenth8, MI)

MIwetlands_tenth9_cropped <- st_intersection(MIwetlands_tenth9, MI)

MIwetlands_tenth10_cropped <- st_intersection(MIwetlands_tenth10, MI)

#Export to shapefile, removing extraneous columns from intersection

# Step 1: Identify the variables from the original dataframe
original_vars <- names(MIwetlands)
original_vars

# Step 2: Subset the cropped df to retain only the original variables
MIwetlands_tenth1_cropped_subset <- MIwetlands_tenth1_cropped[, original_vars]
colnames(MIwetlands_tenth1_cropped_subset)

MIwetlands_tenth2_cropped_subset <- MIwetlands_tenth2_cropped[, original_vars]
MIwetlands_tenth3_cropped_subset <- MIwetlands_tenth3_cropped[, original_vars]
MIwetlands_tenth4_cropped_subset <- MIwetlands_tenth4_cropped[, original_vars]
MIwetlands_tenth5_cropped_subset <- MIwetlands_tenth5_cropped[, original_vars]
MIwetlands_tenth6_cropped_subset <- MIwetlands_tenth6_cropped[, original_vars]
MIwetlands_tenth7_cropped_subset <- MIwetlands_tenth7_cropped[, original_vars]
MIwetlands_tenth8_cropped_subset <- MIwetlands_tenth8_cropped[, original_vars]
MIwetlands_tenth9_cropped_subset <- MIwetlands_tenth9_cropped[, original_vars]
MIwetlands_tenth10_cropped_subset <- MIwetlands_tenth10_cropped[, original_vars]

# Step 3: Export the subsetted dataframe as a shapefile
setwd("C:...aaa clipped wetlands to states")

st_write(MIwetlands_tenth1_cropped_subset, "MIwetlandsClipped_part 1.shp")
st_write(MIwetlands_tenth2_cropped_subset, "MIwetlandsClipped_part 2.shp")
st_write(MIwetlands_tenth3_cropped_subset, "MIwetlandsClipped_part 3.shp")
st_write(MIwetlands_tenth4_cropped_subset, "MIwetlandsClipped_part 4.shp")
st_write(MIwetlands_tenth5_cropped_subset, "MIwetlandsClipped_part 5.shp")
st_write(MIwetlands_tenth6_cropped_subset, "MIwetlandsClipped_part 6.shp")
st_write(MIwetlands_tenth7_cropped_subset, "MIwetlandsClipped_part 7.shp")
st_write(MIwetlands_tenth8_cropped_subset, "MIwetlandsClipped_part 8.shp")
st_write(MIwetlands_tenth9_cropped_subset, "MIwetlandsClipped_part 9.shp")
st_write(MIwetlands_tenth10_cropped_subset, "MIwetlandsClipped_part 10.shp")



###########################################################################################
############################################################################################
##################  Create summary tables ##################
###########################################################################################
############################################################################################

# load libraries

library(sf)
library(dplyr)
library(stringr)
library(pdftools)

##############
## Wetlands ##
##############

# In ArcGIS Pro, calc acres of COE wetlands
# If continuing, remove objects from environment

setwd("C:...aaa clipped wetlands to states")

# list files
shapefiles <- list.files(pattern = "\\.shp$")
shapefiles

# bring in dfs w Esri-calculated wetland acres:
# load all spatial df's in folder via lapply (can be slow process)
spatial_dfs <- lapply(shapefiles, st_read)
# reads each shapefile into a spatial df and stores them in a list named spatial_dfs

# assign names to list elements
names(spatial_dfs) <- shapefiles

# view the content of a few of the df's
print(names(spatial_dfs))
# since you are pulling from a list, need to tweak syntax:
summary(spatial_dfs[["IAwetlandsClipped.shp"]])
summary(spatial_dfs[["SDwetlandsClipped_part 7.shp"]])
summary(spatial_dfs[["WIwetlandsClipped_part 9.shp"]])

# Create a function to extract state abbreviation from file names
extract_state <- function(filename) {
  str_extract(filename, "^[A-Z]{2}")  # Assumes state abbreviation is the first two uppercase letters
}

# Create empty dataframe to store the summary
summary_df <- data.frame(State = character(), Wetland_Acres = numeric(), stringsAsFactors = FALSE)

# Loop through each dataframe in the list
for (name in names(spatial_dfs)) {
  # Extract state abbreviation
  state <- extract_state(name)
  
  # Read the dataframe from the list
  df <- spatial_dfs[[name]]
  
  # Calculate the sum of wetland acres for this dataframe
  wetland_acres <- sum(df$WtlndAcr, na.rm = TRUE)
  
  # Append to summary_df
  summary_df <- summary_df %>%
    add_row(State = state, Wetland_Acres = wetland_acres)
}

summary_df <- summary_df %>%
  group_by(State) %>%
  summarise(Wetland_Acres = sum(Wetland_Acres, na.rm = TRUE))

# Rename the column
summary_df <- summary_df %>%
  rename(`Wetland area (acres)` = Wetland_Acres)

print(summary_df)

# Create annual flood mitigation value
new_column <- summary_df$`Wetland area (acres)` * 744.62
# per acre flood mitigation value from Taylor and Druckenmiller 2022, converted from hectare

# Add the new column to the dataframe
summary_df <- cbind(summary_df, `Annual residential flood mitigation value ($)` = new_column)

# Print the updated dataframe
print(summary_df)

# Add column for low estimated long term flood mitigation value
# using 7% discount rate
new_column2 <- summary_df$`Annual residential flood mitigation value ($)` * (1/0.07)
summary_df <- cbind(summary_df, `Estimated long-term flood mitigation value (low) ($)` = new_column2)
print(summary_df)

# Add column for high estimated long term flood mitigation value
# using 3% discount rate
new_column3 <- summary_df$`Annual residential flood mitigation value ($)` * (1/0.03)
summary_df <- cbind(summary_df, `Estimated long-term flood mitigation value (high) ($)` = new_column3)
print(summary_df)

# Export to csv
file_path <- "C:...SUMMARY TABLES/Wetlands with valuations_Upper Midwest.csv"
write.csv(summary_df, file = file_path, row.names = FALSE)

#################
## Agriculture ##
#################

# Read pdf
pdf_path <- "C:...Ag land by state/st99_2_001_001_Table1. State Summary Highlights_ 2022.pdf"

# Since pages are inconsistently formatted, extract data page by page

######## Extract text from page 3 - Illinois ########
pdf_text_page3 <- pdf_text(pdf_path)[3]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page3, "\n"))

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements from column headings and data
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create dataframe
# Use only as many column names as there are data values to avoid mismatch
num_columns <- min(length(column_headings), length(data_line))
df <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df) <- column_headings[1:num_columns]

# View
print(df)

######## Extract text from page 4 - Iowa ########
pdf_text_page4 <- pdf_text(pdf_path)[4]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page4, "\n"))

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
# Use only as many column names as there are data values to avoid mismatch
num_columns <- min(length(column_headings), length(data_line))
df2 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df2) <- column_headings[1:num_columns]
print(df2)
colnames(df2)


######## Extract text from page 5 - Michigan & Minnesota ########
pdf_text_page5 <- pdf_text(pdf_path)[5]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page5, "\n"))

# Inspect lines around the column headings and data
cat("Lines from page 5:\n")
cat(lines[1:10], sep = "\n")  # Print the first 10 lines for context

# note lines of interest are in 4 and 7

# Extract the column headings (line 4) and data (line 7) 
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
num_columns <- min(length(column_headings), length(data_line))
df3 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df3) <- column_headings[1:num_columns]
print(df3)
colnames(df3)


######## Extract text from page 6 - Nebraska ########
pdf_text_page6 <- pdf_text(pdf_path)[6]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page6, "\n"))

# Inspect lines around the column headings and data
cat("Lines from page 6:\n")
cat(lines[1:10], sep = "\n")  # Print the first 10 lines for context

# note lines of interest

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
num_columns <- min(length(column_headings), length(data_line))
df4 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df4) <- column_headings[1:num_columns]
print(df4)
colnames(df4)


######## Extract text from page 7 - North Dakota ########
pdf_text_page7 <- pdf_text(pdf_path)[7]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page7, "\n"))

# Inspect lines around the column headings and data
cat("Lines from page 7:\n")
cat(lines[1:10], sep = "\n")  # Print the first 10 lines for context

# note lines of interest

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
num_columns <- min(length(column_headings), length(data_line))
df5 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df5) <- column_headings[1:num_columns]
print(df5)
colnames(df5)



######## Extract text from page 8 - South Dakota ########
pdf_text_page8 <- pdf_text(pdf_path)[8]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page8, "\n"))

# Inspect lines around the column headings and data
cat("Lines from page 8:\n")
cat(lines[1:10], sep = "\n")  # Print the first 10 lines for context

# note lines of interest

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
num_columns <- min(length(column_headings), length(data_line))
df6 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df6) <- column_headings[1:num_columns]
print(df6)
colnames(df6)


######## Extract text from page 9 - Wisconsin ########
pdf_text_page9 <- pdf_text(pdf_path)[9]

# Split the page text into lines
lines <- unlist(strsplit(pdf_text_page9, "\n"))

# Inspect lines around the column headings and data
cat("Lines from page 9:\n")
cat(lines[1:10], sep = "\n")  # Print the first 10 lines for context

# note lines of interest

# Extract the column headings (line 4) and data (line 7)
column_headings <- unlist(strsplit(lines[4], "\\s{2,}"))
data_line <- unlist(strsplit(lines[7], "\\s{2,}"))

# Remove any potential empty elements
column_headings <- column_headings[nzchar(column_headings)]
data_line <- data_line[nzchar(data_line)]

# Ensure lengths match
if (length(column_headings) != length(data_line)) {
  cat("Warning: The number of column headings does not match the number of data values.\n")
  cat("Number of column headings:", length(column_headings), "\n")
  cat("Number of data values:", length(data_line), "\n")
}

# Create & view dataframe
num_columns <- min(length(column_headings), length(data_line))
df7 <- data.frame(matrix(data_line[1:num_columns], ncol = num_columns, byrow = TRUE))
colnames(df7) <- column_headings[1:num_columns]
print(df7)
colnames(df7)


######## Create summary ########
print(df)
print(df2)
print(df3)
print(df4)
print(df5)
print(df6)
print(df7)

# inspect content
head(df)
head(df2)
head(df3)
head(df4)
head(df5)
head(df6)
head(df7)


# Extract values from each df - create function
# to extract the value based on a partial match
extract_value <- function(df, state_col, pattern) {
  # Find the row with a partial match to the pattern
  value_row <- which(grepl(pattern, df$Item))
  
  if (length(value_row) > 0) {
    return(df[value_row, state_col])
  } else {
    warning(paste("Row with pattern", pattern, "not found in dataframe"))
    return(NA)  # Return NA if the row is not found
  }
}

# Define pattern for the row identifier
pattern <- "Land in farms"

# Extract values from each dataframe
land_in_farms_acres <- c(
  extract_value(df, "Illinois", pattern),  # From df
  extract_value(df2, "Iowa", pattern),     # From df2
  extract_value(df3, "Michigan", pattern), # From df3
  extract_value(df3, "Minnesota", pattern),# From df3
  extract_value(df4, "Nebraska", pattern), # From df4
  extract_value(df5, "North Dakota", pattern), # From df5
  extract_value(df6, "South Dakota", pattern), # From df6
  extract_value(df7, "Wisconsin", pattern) # From df7
)

# Create and print the summary dataframe
summary_df_ag <- data.frame(
  State = c("Illinois", "Iowa", "Michigan", "Minnesota", "Nebraska", "North Dakota", "South Dakota", "Wisconsin"),
  Land_in_Farms_Acres = land_in_farms_acres
)

print(summary_df_ag)

# import state land
setwd("C:...Upper Midwest states shapefiles")

IA <- st_read("IA.shp")
MN <- st_read("MN.shp")
IL <- st_read("IL.shp")
ND <- st_read("ND.shp")
SD <- st_read("SD.shp")
WI <- st_read("WI.shp")
NE <- st_read("NE.shp")
MI <- st_read("MI.shp")

colnames(IA)
head(IA)
head(MN)

colnames(summary_df_ag)

print(unique(summary_df_ag$State))

# Create a list of state spatial dataframes
state_dfs <- list(IA = IA, MN = MN, IL = IL, ND = ND, SD = SD, WI = WI, NE = NE, MI = MI)

# manually match abbreviations to state names
state_map <- c(
  "Illinois" = "IL", 
  "Iowa" = "IA", 
  "Michigan" = "MI", 
  "Minnesota" = "MN",
  "Nebraska" = "NE",
  "North Dakota" = "ND",
  "South Dakota" = "SD",
  "Wisconsin" = "WI"
)

# Apply to State column
summary_df_ag$State <- state_map[summary_df_ag$State]

#check out total area in acres
lapply(state_dfs, function(df) head(df$TotArAcres))

# Loop through each state dataframe and add TotArAcres to summary_df_ag
for(state in names(state_dfs)) {
  # Extract the total area in acres for the current state
  total_acres <- sum(state_dfs[[state]]$TotArAcres, na.rm = TRUE)
  
  # Debugging output to check values being added
  print(paste("State:", state, "Total Acres:", total_acres))
  
  # Add the total area to the corresponding row in summary_df_ag
  summary_df_ag[summary_df_ag$State == state, "TotArAcres"] <- total_acres
}

# Check the updated summary_df_ag
print(summary_df_ag)

# rename columns
colnames(summary_df_ag) <- c("State", "Agriculture area (acres)", "Total area (acres)")

str(summary_df_ag)
# remove commas from ag area and convert to numeric
summary_df_ag$`Agriculture area (acres)` <- as.numeric(gsub(",", "", summary_df_ag$`Agriculture area (acres)`))

# calc % area that is used for ag
summary_df_ag$`Percent of state in agriculture (%)` <- (summary_df_ag$`Agriculture area (acres)` / summary_df_ag$`Total area (acres)`) * 100

print(summary_df_ag)

# Export to csv
file_path <- "C:...SUMMARY TABLES/Agricultural land_Upper Midwest.csv"
write.csv(summary_df_ag, file = file_path, row.names = FALSE)

# END #



# ProjectModels_Spatial

In this workshop we will create a spatial projection of our random forest model for monthly CH4. 

To date, we have completed 
model calibration, validation, and sensitivity analysis. Now we are ready to apply out model to a landscape to estimate natural methane emissions. For this workshop we will estimate natural emissions for Conneticut.


### Load the datasets and the random forest model: 
```{r}
rm(list=ls())
load(file="data/final_model.RDATA" )
```
There are three four items in this .RDATA file. (1) a randomForest model, (2) the fluxnet dataset, (3) the training data, and (4) the testing data.

### Look at the model to determine which variables are in it:
```{r}
library(randomForest)

FCH4_F_gC.rf
```
The model includes precipitation in mm (P_F), mean air temperature temperature in degrees Celsius (TA_F) and an indicator for Upland ecosystems (Upland).

### Check the class of the each variable:
```{r}
class(train$P_F)
class(train$TA_F)
class(train$Upland)
```
To project this model in space we will need the following variables:

(1) Monthly total precipitation in mm and the name of the layer needs to be "P_F"
(2) Monthly mean air temperature temperature in degrees Celsius andt the layer name needs to be "TA_F"
(3) We need an indicator for Upland ecosystems (Upland). All inundated ecosystems (+ snow) are called "inundated" and nonindundated ecosystems are called "upland". Croplands and urban areas are filtered out of this layer. 

### Determine where you will get your spatial data from.

(1) Monthly total precipitation (mm): Terra Climate
(2) Monthly mean air temperature temperature in degrees Celsius: Terra climate (getTerraClim())
(3) We need an indicator for Upland ecosystems (Upland): MODIS Land Cover Data (Majority_Land_Cover_Type_1) downloaded from: (2001 - 2022) https://lpdaac.usgs.gov/products/mcd12c1v061/ the user guige is available here: https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

# Workshop Raster processing: 

# Start with downloading the climate layers you will need for P_F and T_F. I will spatialize the model for 2021. 

```{r}
library(AOI)
library(climateR)
library(terra)
library(tidyverse)
```

# Create an AOI for Connecticut: 
```{r}
ct <- AOI::aoi_get(state="CT")
plot(ct)
```

# Downlooad terra climate data (Precipitation and air temperature) for 2021: 
```{r}
global.clim.N <- ct %>% getTerraClim(varname = c("ppt", "tmin", "tmax"), 
                                     startDate = "2021-01-01",
                                     endDate = "2021-12-31")
```

# Subset the data for each variable
```{r}
global.clim.ppt <- global.clim.N$ppt
global.clim.tmin <-global.clim.N$tmin
global.clim.tmax <- global.clim.N$tmax 
```

# We need mean air temperature. Calculate the mean of the maximum and minimum air temperature.
```{r}
global.clim.tmean <-   mean(global.clim.tmin, global.clim.tmax)
global.clim.tmean
```
```{r}
rm(global.clim.tmin, global.clim.tmax) # Remove layers you no longer need
```

# Save the layers:
```{r}
writeRaster(global.clim.tmean, "/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/TERRA_TMEAN_2021_CT.tif", overwrite=TRUE )
writeRaster(global.clim.ppt, "/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/TERRA_PPT_2021_CT.tif", overwrite=TRUE )
```
# Now we need to get the MODIS IGBP layers:

```{r}
igbp.ct <- terra::rast("/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/MODIS_IGBP_2001-2022_CT.tif")
igbp.ct 
```

# This layer needs to be reformatted: Using the User guide we can determine what each numerical value represents: https://lpdaac.usgs.gov/documents/1409/MCD12_User_Guide_V61.pdf

# 1: ENF
# 2: EBF
# 3: DNF
# 4: DBF
# 5: MF
# 6: CS
# 7: OS
# 8: WS
# 9 : SAV
#10 : GRA
# 11: WET
# 12 : CRO
# 13 : URB
#14 : CRO
# 15 : SNO
#16: Barren
# 17 : WAT
# 0: Unclassified

# look at the layer:
```{r}
plot(igbp.ct[[1]] )
```
# Reclassify each value one at a time to think about how we should reclassify each one. We want to give all uplands the value 1 and all inundated systems the value 0.
```{r}
igbp.ct.r <- igbp.ct
```

```{r}
igbp.ct.r[ igbp.ct.r == 0] <- NA # remove the fill value
plot(igbp.ct.r[[1]] )
```

```{r}
igbp.ct.r[ igbp.ct.r == 1] <- 1 
igbp.ct.r[ igbp.ct.r == 2] <- 1
igbp.ct.r[ igbp.ct.r == 3] <- 1
igbp.ct.r[ igbp.ct.r == 4] <- 1
igbp.ct.r[ igbp.ct.r == 5] <- 1
igbp.ct.r[ igbp.ct.r == 6] <- 1
igbp.ct.r[ igbp.ct.r == 7] <- 1
igbp.ct.r[ igbp.ct.r == 8] <- 1
igbp.ct.r[ igbp.ct.r == 9] <- 1
igbp.ct.r[ igbp.ct.r == 10] <- 1
igbp.ct.r[ igbp.ct.r == 11] <- 0
igbp.ct.r[ igbp.ct.r == 12] <- NA
igbp.ct.r[ igbp.ct.r == 13] <- NA
igbp.ct.r[ igbp.ct.r == 14] <- NA
igbp.ct.r[ igbp.ct.r == 15] <- 0
igbp.ct.r[ igbp.ct.r == 16] <- 1
igbp.ct.r[ igbp.ct.r == 17] <- 0
```

```{r}
plot(igbp.ct[[1]] ) # Everything is reclassified to an upland:
```

# Format the upland layer as a factor: 
```{r}
factors.df <- data.frame(id=c(1, 0), cover=c("upland", "inundated"))
```

# create a for loop to assign the factor layers to each layer one at a time:
```{r}
for ( i in 1:22){
  print(i)
  levels(igbp.ct.r[[i]]) <- factors.df
  is.factor(igbp.ct.r)
}

plot(igbp.ct.r )
```
# we only need the layer for 2021. subset the 2021 layer:
```{r}
igbp.ct.r.2021 <- igbp.ct.r[[21]]
```

# we will use the projection on the terra climate layers:
```{r}
igbp.ct.r.2021 <- terra::project( igbp.ct.r.2021, global.clim.ppt.resample)
```

# All the resolutions must match to combine the rasters into one item. We will set the terra climate layers to match the igbp.ct.r layer:
```{r}
global.clim.tmean.resample <- resample( global.clim.tmean, igbp.ct.r.2021)
global.clim.ppt.resample <- resample( global.clim.ppt, igbp.ct.r.2021)
```

```{r}
writeRaster(global.clim.tmean.resample, "/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/TERRA_TMEAN_2021_CT_rs.tif", overwrite=TRUE )
writeRaster(global.clim.ppt.resample, "/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/TERRA_PPT_2021_CT_rs.tif", overwrite=TRUE )
```

# Now we are ready to project the model:
```{r}
model.rasters.m1 <- c(igbp.ct.r.2021, global.clim.tmean.resample[[1]], global.clim.ppt.resample[[2]] )
```

# make the names of the layers match the dataframe:
```{r}
names(model.rasters.m1 ) <- c("Upland", "TA_F", "P_F" )
model.rasters.m1
```

```{r}
class(train$Upland )
summary(train$Upland )
levels(train$Upland )
```

```{r}
model.rasters.m1.pred <- terra::predict(  object= model.rasters.m1, model=FCH4_F_gC.rf)
writeRaster(model.rasters.m1.pred ,"/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/MODEL_PRED_m1.tif", overwrite=TRUE )
```

# We can do this in a for loop to get all 12 months:
```{r}
setwd('/Users/sm3466/Dropbox (YSE)/Teaching/Workshops/')
directory <- getwd()
subDir <- 'predictions'

dir.create(file.path(directory , subDir))

setwd(subDir)


for ( i in 1:12){
  print(i)
  
  model.rasters <- c(igbp.ct.r.2021, global.clim.tmean.resample[[i]], global.clim.ppt.resample[[i]] )
  names(model.rasters) <- c("Upland", "TA_F", "P_F" )
  pred <- terra::predict(  object= model.rasters, model=FCH4_F_gC.rf)
  writeRaster(pred ,paste("MODEL_PRED_m",i,".tif", sep =""), overwrite=TRUE )
  
}
```

# Make of list of all the files in a directory with a specific name element:
```{r}
pred <- list.files( pattern="MODEL_PRED_m")
```

# Import the files:
```{r}
predictions <- rast(pred)
```

# create the 2021 methane budget:
```{r}
predictions.2021.total <- sum(predictions )

plot(predictions.2021.total)
```

Now you are ready to follow the same workflow for your model:
(1) Make a list of the variable, their units, the exact name and class of each variable in your data. 
(2) Determine where you can get a spatial version of each variable in your dataset.
(3) Format each spatial layer
(4) Make predictions
(5) Use predictions.

Ensure your raster layers all have the same CRS and resolution!

The post-workshop assessment ...



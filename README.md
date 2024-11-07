# Spatial Projects of a randomForest model

In this workshop we will create a spatial projection of our random forest model for monthly methane echange from natural ecosystems. 

To date, we have completed model calibration, validation, and sensitivity analysis. Next we are ready to apply the model to a landscape to estimate natural methane emissions. For this workshop we will estimate natural emissions for Connecticut.

In this workshop we will:
(1) Make a list of the variables, their units, the exact name and class of each variable in your model. 
(2) Determine where you can get a spatial version of each variable in your model.
(3) Format each spatial layer
(4) Make predictions in space
(5) Use predictions to calculate a budget


# (1) Make a list of the variables, their units, the exact name and class of each variable in your model. 

Load the datasets and the model. 
```{r}
rm(list=ls())
load(file="data/final_model.RDATA" )
```
There are four items in this .RDATA file. (1) a randomForest model, (2) the fluxnet dataset, (3) the training data, and (4) the testing data.

### Look at the model to determine which variables are in it:
```{r}
library(randomForest)

FCH4_F_gC.rf
```
The model includes precipitation in mm (P_F), mean air temperature temperature in degrees Celsius (TA_F) and an indicator for upland ecosystems (Upland).

Check the class of the each variable.
```{r}
class(train$P_F)
class(train$TA_F)
class(train$Upland)
```

To project this model in space we need the following variables:

(1) Monthly total precipitation in mm and the name of the layer needs to be "P_F"
(2) Monthly mean air temperature temperature in degrees Celsius and the layer name needs to be "TA_F"
(3) We need an indicator for Upland ecosystems called Upland. All inundated ecosystems (+ snow) are called "inundated" and non-inundated ecosystems are called "upland". Croplands and urban areas should be filtered out of this layer. 

# (2) Determine where you can get a spatial version of each variable in your model.

(1) Monthly total precipitation (mm): Terra climate (getTerraClim())
(2) Monthly mean air temperature temperature in degrees Celsius: Terra climate (getTerraClim())
(3) Indicator for Upland ecosystems (Upland): MODIS Land Cover Data (Majority_Land_Cover_Type_1) downloaded from: (2001 - 2022) https://lpdaac.usgs.gov/products/mcd12c1v061/ the user guige is available here: https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf

To use raster layers with the predict function they have to have the same crs, resolution, and extent!

#(3) Format each spatial layer 

Download the climate layers needed for P_F and T_F. We will spatialize the model for Connecticut in the year 2021. 
```{r}
library(AOI)
library(climateR)
library(terra)
library(tidyverse)
```
Create an AOI for Connecticut. 
```{r}
ct <- AOI::aoi_get(state="CT")
plot(ct$geometry)
```

Download terra climate data (Precipitation and air temperature) for 2021.
```{r}
global.clim.N <- ct %>% getTerraClim(varname = c("ppt", "tmin", "tmax"), 
                                     startDate = "2021-01-01",
                                     endDate = "2021-12-31")
```
Subset the data for each variable.
```{r}
global.clim.ppt <- global.clim.N$ppt
global.clim.tmin <-global.clim.N$tmin
global.clim.tmax <- global.clim.N$tmax 
```
We need mean air temperature. Calculate the mean using the maximum and minimum air temperature.
```{r}
global.clim.tmean <-   mean(global.clim.tmin, global.clim.tmax)
global.clim.tmean
```
Remove the layers you no longer need. 
```{r}
rm(global.clim.tmin, global.clim.tmax)
```
Save the layers.
```{r}
writeRaster(global.clim.tmean, "data/TERRA_TMEAN_2021_CT.tif", overwrite=TRUE )
writeRaster(global.clim.ppt, "data/TERRA_PPT_2021_CT.tif", overwrite=TRUE )
```
Now, we need to get the MODIS IGBP layers. The dataset provided was developed from MODIS Land Cover Data (Majority_Land_Cover_Type_1) downloaded from: (2001 - 2022) https://lpdaac.usgs.gov/products/mcd12c1v061/. This dataset was downloaded for the entire globe and cropped to include only Connecticut.

Load the data.
```{r}
igbp.ct <- terra::rast("data/MODIS_IGBP_2001-2022_CT.tif")
igbp.ct 
```
This layer needs to be reformatted. Using the User Guide we can determine what each numerical value represents: https://lpdaac.usgs.gov/documents/1409/MCD12_User_Guide_V61.pdf

1: ENF
2: EBF
3: DNF
4: DBF
5: MF
6: CS
7: OS
8: WS
9 : SAV
10 : GRA
11: WET
12 : CRO
13 : URB
14 : CRO
15 : SNO
16: Barren
17 : WAT
0: Unclassified

look at the layer. Here I use"[[1]]" to see only the first layer, which is for the year 2001.
```{r}
terra::plot(igbp.ct[[1]])
```
Reclassify each value, one at a time, to think about how you should reclassify each one. We want to give all uplands the value 1 and all inundated systems the value 0.

First, make a copy of the raters (igbp.ct) and call it igbp.ct.r
```{r}
igbp.ct.r <- igbp.ct
```

Reclassify 0 value to NA.
```{r}
igbp.ct.r[ igbp.ct.r == 0] <- NA 
terra::plot(igbp.ct.r[[1]] )
```
Reclassify the other values:
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
Look at the final rater to ensure everything is reclassified to upland, since Conneticuit doesn't have anything else at the resolution of MODIS.
```{r}
terra::plot(igbp.ct[[1]] ) 
```
Format the upland layer as a factor by first making a dataframe that has the raster values 0 and 1 and the corresponding factor level. 
```{r}
factors.df <- data.frame(id=c(1, 0), cover=c("upland", "inundated"))
```
Create a for loop to assign the factor levels to each raster layer one at a time:
```{r}
for ( i in 1:22){
  print(i)
  levels(igbp.ct.r[[i]]) <- factors.df
  is.factor(igbp.ct.r)
}

terra::plot(igbp.ct.r, col="red" )
```
We only need the layer for 2021. Subset the 2021 layer.
```{r}
igbp.ct.r.2021 <- igbp.ct.r[[21]]
```
We will use the CRS of the terra climate layers and make everything match this.
```{r}
igbp.ct.r.2021 <- terra::project( igbp.ct.r.2021, global.clim.ppt)
```
All the resolutions must be the same to combine the rasters into one item. We will set the terra climate layers to match the igbp.ct.r layer:

```{r}
global.clim.tmean.resample <- resample( global.clim.tmean, igbp.ct.r.2021)
global.clim.ppt.resample <- resample( global.clim.ppt, igbp.ct.r.2021)
```
Now export the files to save a version that is processed as needed.
```{r}
writeRaster(global.clim.tmean.resample, "data/TERRA_TMEAN_2021_CT_rs.tif", overwrite=TRUE )
writeRaster(global.clim.ppt.resample, "data/TERRA_PPT_2021_CT_rs.tif", overwrite=TRUE )
writeRaster(igbp.ct.r.2021, "data/MODIS_Upland_2021_CT.tif", overwrite=TRUE )
```
# (4) Make predictions

Combine all the variables into a raster stack, only including one month since igbp.ct.r.2021 has one layer and the climate has 12, one for each month.
```{r}
model.rasters.m1 <- c(igbp.ct.r.2021, global.clim.tmean.resample[[1]], global.clim.ppt.resample[[1]] )
```
If you have any issues combining the raster layers into one object, you may not have made everything the same resolution or extent.

Make the names of the raster layers match the dataframe.
```{r}
names(model.rasters.m1 ) <- c("Upland", "TA_F", "P_F" )
model.rasters.m1
```
Check the dataframe one more time to ensure you don't need to make additional changes to the raster.
```{r}
class(train$Upland )
summary(train$Upland )
levels(train$Upland )

model.rasters.m1$Upland

```
You are ready to use the predict function to predict you model in space.
```{r}
model.rasters.m1.pred <- terra::predict(  object= model.rasters.m1, model=FCH4_F_gC.rf)

plot(model.rasters.m1.pred)
```
We can do this in a for loop to get all 12 months.

First, determine where you want to export the files to, and make a new folder there called predictions.
```{r}
setwd('data') # sets the working directory to your data folder
directory <- getwd() # saves the path to directory
subDir <- 'predictions' # You will use this to make the folder called predictions

dir.create(file.path(directory , subDir)) # this makes the new folder in data called predictions

setwd(subDir) # sets the working directory to your new folder
```
Make the forloop to make predictions for all 12 months.
```{r}
for ( i in 1:12){
 
  print(i)
  
  model.rasters <- c(igbp.ct.r.2021, global.clim.tmean.resample[[i]], global.clim.ppt.resample[[i]] )
  names(model.rasters) <- c("Upland", "TA_F", "P_F" )
  pred <- terra::predict(  object= model.rasters, model=FCH4_F_gC.rf)
  writeRaster(pred ,paste("MODEL_PRED_m",i,".tif", sep =""), overwrite=TRUE )
}
```

Make of list of all the files in a directory with a specific name element.
```{r}
pred <- list.files( pattern="MODEL_PRED_m")
```
Import the files.
```{r}
predictions <- rast(pred)
predictions
```

# (5) Use predictions
Create the 2021 methane budget.
```{r}
predictions.2021.total <- sum(predictions )
plot(predictions.2021.total)
terra::global(predictions.2021.total, sum, na.rm=T)

```
Now you are ready to follow the same workflow for your model.
(1) Make a list of the variables, their units, the exact name and class of each variable in your model. 
(2) Determine where you can get a spatial version of each variable in your model.
(3) Format each spatial layer
(4) Make predictions
(5) Use predictions

Ensure your raster layers all have the same CRS and resolution!

Go to canvas to view the details of the post-workshop assessment.

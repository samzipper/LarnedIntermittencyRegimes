## BucketModel.R
# This code builds a daily ecohydrological 'bucket model' soil water balance accounting model.
#
# Daily input data (vectors of same length):
# - precip = precipitation [m/day]
# - PET = potential ET [m/day]
# Static input data:
# - Ksat = saturated hydraulic conductivity of soils [m/day] (Loheide et al., 2005 use 50 m/day)
# - porosity = porosity of soil [-]
# - S.field = relative soil moisture at field capacity, where relative soil moisture is defined as Vwater/(Vair + Vwater) [-]
#     If you have an estimate of Sy, you can estimate S.field as (Sy/porosity)
# - S.stress = relative soil moisture below which water stress occurs [-]
# - S.init = relative soil moisture at initial conditions [-]
# - int_depth = maximum quantity of interception [m]
# - aq_thickness = total depth of bucket [m]
# - root_depth = depth of rooting zone [m]

ts_data <- readr::read_csv(file.path("data", "Timeseries_InputData.csv"),
                           col_types = "Dddddddddd") %>% 
  subset(year(date_ghcn) > 1998)

# plot
ts_data %>% 
  dplyr::select(date_ghcn, stage_masl, LWPH4a, LWPH4c) %>% 
  tidyr::pivot_longer(-date_ghcn, values_to = "elevation_m") %>% 
  ggplot(aes(x = date_ghcn, y = elevation_m, color = name)) + 
  geom_line()

precip <- ts_data$prcp_mm/1000
PET <- ts_data$ETo_mm/1000
stream_stage <- ts_data$stage_masl

# define parameters
aq_bottom_elev <- 560
Sy <- 0.2
root_depth <- 2.25
land_elevation <- 595
streambed_elevation <- 593
streambed_k <- 50 # m/d
streambed_thickness <- 0.1 # m
head_init <- aq_bottom_elev + 15


## Input data checks
if (length(precip) != length(PET)) stop("precip and PET vector lengths differ")
if (sum(is.na(c(precip, PET))) > 0) stop("NA values in precip and/or PET input data")

## Set up model
# calculate some derived values
n_days <- length(precip) # [days] length of model simulation
root_depth_head <- land_elevation - root_depth      # head corresponding to bottom of root zone
stream_depth_head <- land_elevation - stream_depth  # head corresponding to bottom of streambed

# empty arrays
head         <- rep(NA, length = n_days)  # head in alluvial aquifer
infiltration <- rep(NA, length = n_days)  # infiltration into alluvial aquifer
ET           <- rep(NA, length = n_days)  # calculated groundwater component of ET
stream_flux  <- rep(NA, length = n_days)  # calculated stream-aquifer exchange. positive value indicates flow from stream into aquifer

# initial conditions
head[1] <- head_init

## Run daily water balance model
for (i in 2:n_days){
  # ET depends on head with respect to root depth
  if (head[i-1] > root_depth_head){
    ET[i] <- PET[i] * min(c(((head[i-1] - root_depth_head)/(land_elevation - root_depth_head)), 1))
  } else {
    ET[i] <- 0
  }
  
  # stream-aquifer flux depends on head with respect to stream stage
  if (is.finite(stream_stage[i]) & stream_stage[i] >= streambed_elevation){
    # if there is water in the stream...
    stream_flux_gradient <- (streambed_k/streambed_thickness)*(stream_stage[i] - head[i-1])
    stream_flux[i] <- max(c(min(c(stream_flux_gradient, streambed_k)), -streambed_k))
  } else {
    stream_flux[i] <- 0
  }
  
  # update head
  head[i] <- head[i-1] + (precip[i] + stream_flux[i] - ET[i])/Sy
  
}

# compile output
bucket_out <- 
  tibble::tibble(day = seq(1, n_days),
                 precip_m = precip,
                 PET_m = PET,
                 stream_stage_m = stream_stage,
                 stream_flux = stream_flux,
                 head = head,
                 ET_m = ET)

# plot
ggplot(bucket_out, aes(x = day, y = head)) +
  geom_hline(yintercept = root_depth_head, color = "green") +
  geom_hline(yintercept = stream_depth_head, color = "blue") +
  geom_line()

bucket_out %>% 
  dplyr::select(day, precip_m, ET_m, stream_flux) %>% 
  tidyr::pivot_longer(-day) %>% 
  ggplot(aes(x = day, y = value)) +
  geom_line() +
  facet_wrap(~name)

# Assign the variables that will define your model run here

int_depth <- 0.005      # [m] maximum quantity of interception
porosity <- 0.4         # [-] porosity of soil
Ksat <- 1               # [m/day] saturated hydraulic conductivity of bucket soils
S.field <- 0.3/porosity # refine
S.stress <- 0.3       # refine
S.init <- 0.5         # [-] relative soil moisture at initial conditions
aq_thickness <- 30    # refine
root_depth <- 1.5     # refine

# Create empty vectors to hold your output --------------------------------
# There should be one empty vector for each desired model output. For
# now, they will only contain NaNs, and we will fill them in as we go.

n_days <- length(precip) # [days] desired length of model simulation
precip_eff   <- rep(NA, length = n_days)  # effective precipitation
interception <- rep(NA, length = n_days)
runoff       <- rep(NA, length = n_days)
infiltration <- rep(NA, length = n_days)
ET           <- rep(NA, length = n_days)
leakage      <- rep(NA, length = n_days)
soil.moisture<- rep(NA, length = n_days)  # remember, this is relative soil moisture = Vwater/(Vair + Vwater) = theta/porosity 

# Run the model -----------------------------------------------------------

# First, set up your initial conditions
soil.moisture[1] <- S.init  # S.init is a user parameter defined above

# set all other fluxes to 0 at the first timestep
precip.eff[1] <- 0
interception[1] <- 0
runoff[1] <- 0
infiltration[1] <- 0
ET[1] <- 0
leakage[1] <- 0

# Partition rainfall between effective precipitation and interception 
# for each day based on int_depth
for (i in 2:n_days){  # start on day 2 because of your initial conditions
  if (precip[i] < int_depth){
    # if there is less precip than your interception depth (int_depth),
    # it should all go to interception. this is true even if precip=0
    interception[i] <- precip[i]
    
    # because it all went to interception, there is no effective precipitation
    precip.eff[i] <- 0
    
    # therefore, there is also no infiltration or runoff
    infiltration[i] <- 0
    runoff[i] <- 0
    
  } else {
    # if there is more precipitation than interception depth, the 
    # quantity int_depth will be intercepted, and the rest will reach the ground
    interception[i] <- int_depth
    
    # the rest of rainfall should be effective precipitation
    precip.eff[i] <- precip[i] - int_depth
    
    # now, we need to divide effective precipitation into infiltration
    # and runoff
    
    # calculate the maximum possible infiltration based on previous soil moisture storage
    infiltration.max <- (porosity*root_depth - soil.moisture[i-1]*porosity*root_depth)
    
    # decide whether effective precipitation infiltrates or runs off
    if (precip.eff[i] <= infiltration.max){
      # if there is less effective precipitation than is required
      # to fill your bucket, it will all infiltrate
      infiltration[i] <- precip.eff[i]
      
      # because it all infiltrates, there is no runoff
      runoff[i] <- 0
    } else {
      # if there is more effective precipitation than is required
      # to fill your bucket, first fill the bucket
      infiltration[i] <- infiltration.max
      
      # the effective precipitation that does not infiltrate will
      # become runoff
      runoff[i] <- precip.eff[i] - infiltration[i]
    }
  }
  
  # calculate leakage, which occurs regardless of precipitation (so it is
  # outside the for loop)
  if (soil.moisture[i-1] < S.field) {
    # if relative soil moisture is less than field capacity, there is no leakage
    leakage[i] <- 0
  } else {
    # if we are above field capacity, linearly scale from 0-Ksat
    leakage[i] <- Ksat*(soil.moisture[i-1] - S.field)/(1-S.field)
    
    # make sure that you don't remove enough water to reduce soil
    # moisture in bucket below field capacity
    if (leakage[i] >= (soil.moisture[i-1] - S.field)*porosity*root_depth){
      leakage[i] <- (soil.moisture[i-1] - S.field)*porosity*root_depth
    }
  }
  
  # calculate ET as a function of soil moisture at previous timestep
  if (soil.moisture[i-1] > S.stress){
    # if relative soil moisture is larger than S.stress ET=PET
    ET[i] <- PET[i]
  } else{
    # if relative soil moisture is less than S.stress, reduce linearly to 0
    ET[i] <- PET[i]*(soil.moisture[i-1])/(S.stress)
    
    # make sure ET is not so much that it drops us below 0
    if (ET[i] >= soil.moisture[i-1]*porosity*root_depth){
      ET[i] <- soil.moisture[i-1]*porosity*root_depth
    }
  }
  
  # update your soil moisture calculation
  soil.moisture[i] <- soil.moisture[i-1] + 
    (infiltration[i] - leakage[i] - ET[i])/(porosity*root_depth)   # inflows - outflows
  
}

### some plots
hist(soil.moisture,
     breaks = seq(0,1,0.02),
     main = "Histogram", 
     xlab = "Relative Soil Moisture",
     ylab = "# of Days")
hist(leakage)

# line plot of soil moisture for last 100 days
plot(seq(1,365,1), soil.moisture,
     type = "l",     # use "o" for lines with dots
     col = "blue",
     main = "Relative Soil Moisture, last 100 days", 
     xlab = "Day",
     ylab = "Relative Soil Moisture",
     ylim = c(0,1))
## RegimeShifts_rstars.R
# This script contains a modified version of the rstars package.

#setwd("C:/Users/samzipper/WorkGits/LarnedIntermittencyRegimes")

source(file.path("code", "paths+packages.R"))

source(file.path("code", "rstars", "Alpha.R"))
source(file.path("code", "rstars", "EqN.R"))
source(file.path("code", "rstars", "IMPK.R"))
source(file.path("code", "rstars", "IPN4.R"))
source(file.path("code", "rstars", "OLS.R"))
source(file.path("code", "rstars", "WeightedAverage.R"))
source(file.path("code", "rstars", "Stars_Citation.R"))

## set function input parameters 
#  anything commented out is defined below, changing rstars package default
#data.timeseries = PDO  # define below
#l.cutoff
pValue <- 0.05
Huber <- 1
Endfunction <- F
#preWhitening <- F
OLS <- F
MPK <- F
#IP4 <- F
save.data <- T
show.plot <- T
#FilteredData <- T
#save.path <- (choose.dir())
timeseries <- T

## rstars example from documentation
# Time <- as.character(seq(as.Date("1998-09-27"), as.Date("2020-12-31"), by = "week"))
# SST <- c(rnorm(length(Time)/2, mean = 1, 5),rnorm(length(Time)/2, mean = 6, 4) )
# require(data.table)
# myts <- data.frame(cbind(Time, SST))
# myts$SST <- as.numeric(as.character(myts$SST))
# rstars (myts, l.cutoff = , preWhitening = T, OLS = T)

# load/prep monthly discharge data
df_Q_day <- 
  readr::read_csv(file.path("data", "Streamflow+Stage_Daily_Clean.csv")) %>% 
  subset(Date >= first_date & Date <= last_date)

df_Q_mo <-
  df_Q_day %>% 
  mutate(Year = year(Date), 
         Month = month(Date)) %>% 
  dplyr::group_by(Year, Month) %>% 
  dplyr::summarize(date_mid = mean(Date),
                   n_noflow = sum(discharge_cms == 0),
                   n_total = sum(is.finite(discharge_cms)),
                   prc_noflow = n_noflow/n_total)

# set wd to rstars folder
setwd(file.path("code", "rstars"))

# define modified rstars parameters
df_in <- df_Q_mo
data.timeseries <- data.frame(cbind(df_in$date_mid, df_in$prc_noflow))
l.cutoff <- 43
IP4 <- F
preWhitening <- F
FilteredData <- F
save.path <- file.path("..", "..", "data")

#### rstars package starts here

####definition of the parameters####

TS <- data.timeseries
l <- l.cutoff
SubsampleSize <- (l + 1) / 3
Plots <- show.plot
Nsub <- SubsampleSize

#definition of prewhitening

if (preWhitening == T){
  if (OLS == F & MPK == F &  IP4 == F)
  {
    stop("preWhitening = T specify OLS, MPK or IP4")
  }
}

if (preWhitening == F){
  FilteredData = F
  DT = 0
  if (OLS == T | MPK == T |  IP4 == T)
  {
    stop("preWhitening = F")
  }
}

if (preWhitening == TRUE){
  RSI_mat = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
  TabTSpw = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
  TSpw = vector(length = length(TS[, 1]) - 1)
  RMean_mat = matrix(0, nrow = length(TS[, 1]) - 1, length(TS[1, ]))
}

if (preWhitening == FALSE){
  RSI_mat = matrix(0, nrow = length(TS[, 1]), length(TS[1, ]))
  RMean_mat = matrix(0, nrow = length(TS[, 1]), length(TS[1, ]))
}


#### attaching the data set and removing of red noise ####
TIMESERIESindex <- 2  # SCZ: modified loop. entire timeseries has to be in a single column

X = ts(TS[, TIMESERIESindex])

N = length(X)

if (N < l)  stop("CutOff cannot be > Time series length")

#test the subsample size (Nsub) limits
if (Nsub < 5 &  MPK == TRUE){
  warning("The subsample size is too small. Automatically corrected - minimum value = 5")
  Nsub = 5
}

if (Nsub < 3 & (IP4 == TRUE | OLS == TRUE)){
  Nsub = 3
  warning("The subsample size is too small. Automatically corrected - minimum value = 3")
}

if (Nsub > N){
  Nsub = N
}

#-------------------------------------------------------------------------------------------------------
# Use prewhitening to remove red noise x(t) = x(t) - alpha * x(t-1)

if (OLS == T | MPK == T | IP4 == T){
  alpha = AlphaEstf(X,N, Nsub,MPK,IP4,OLS)
}

if (preWhitening == TRUE){
  for (i in 2:length(X))
  {
    TSpw[i - 1] = X[i] - (alpha * X[(i - 1)])
  }
  X = TSpw
  TabTSpw[, TIMESERIESindex] = TSpw
}

#===================#
####  STARS 3.2  ####
#===================#

#freedom degree
df = 2 * l - 2

#two tailed test
t_stu = abs(qt(pValue / 2, df))

#Variance and Sigma calcualation for DIFF formula
A = var(X[1:l])


for (i in 2:(length(X) - l + 1)){
  B = var(X[i:(i + l - 1)])
  A = rbind(A, B)
}

#Sigma square
Sigma_s = mean(A)

#between mean values of two subsequent regimes that would be statistically
#significant according to the Student’s t-test
diff = t_stu * sqrt((2 * Sigma_s) / l)

#====================#
#     core steps     #
#====================#

vRMean = 0
RSI = seq(0, 0, length.out = length(X))


R1 = X[1:l]
RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
changepoint = 1
n1 = 0

for (intYear in 2:length(X)){
  
  if (is.na(RegimeMean) || RegimeMean == '')
  {
    break
  }
  
  
  if (Endfunction == T & intYear == (length(X) - l + 1))
  {
    if (preWhitening == F)
    {
      RSI[(length(X) - l + 1):length(X)] == seq(0, 0, length.out = l)
      break
    }
    
    if (preWhitening == T)
    {
      RSI[(length(X) - l + 1):(length(X) - 1)] == seq(0, 0, length.out = (l - 1))
      break
    }
  }
  
  if (X[intYear] > (RegimeMean + diff))
  {
    sumofWeights = 0
    cusumUP = 0
    Xdev = 0
    for (t in intYear:(intYear + l - 1))
    {
      if (t > length(X))
      {
        if (sumofWeights > 0)
        {
          break
        }
      }
      
      Xdev = (X[t] - RegimeMean - diff) / sqrt(Sigma_s)
      
      #determine the weight of the normalized deviation
      if (Xdev == 0)
      {
        Xweight = 1
      }
      
      else if (Xdev != 0)
      {
        Xweight = min(1, (Huber / abs(Xdev)))
      }
      
      #sum weights and weighed values
      sumofWeights = sumofWeights + Xweight
      cusumUP = cusumUP + (Xdev * Xweight)
      
      #check if cusum turns zero
      if (cusumUP < 0)
      {
        cusumUP = 0
        break
      }
    }
    cusumUP = cusumUP / sumofWeights
    
    RSI[intYear] = cusumUP
  }
  
  else if (X[intYear] < (RegimeMean - diff))
  {
    sumofWeights = 0
    cusumDown = 0
    Xdev = 0
    for (t in intYear:(intYear + l - 1))
    {
      if (t > length(X))
      {
        if (sumofWeights > 0)
        {
          break
        }
      }
      
      Xdev = (X[t] - RegimeMean + diff) / sqrt(Sigma_s)
      #determine the weight of the normalized deviation
      if (Xdev == 0)
      {
        Xweight = 1
      }
      else if (Xdev != 0)
      {
        Xweight = min(1, (Huber / abs(Xdev)))
      }
      
      #sum weights and weighed values
      sumofWeights = sumofWeights + Xweight
      cusumDown = cusumDown + (Xdev * Xweight)
      
      #check if cusum turns zero
      if (cusumDown > 0)
      {
        cusumDown = 0
        break
      }
    }
    cusumDown = cusumDown / sumofWeights
    RSI[intYear] = cusumDown
  }
  
  
  else if (RegimeMean - diff <= X[intYear] &
           X[intYear] <= RegimeMean + diff)
  {
    RSI[intYear] = 0
  }
  
  #check for the situation when the test is not over for the last
  #change point, but we are too close to the end of the time series
  if (abs(RSI[intYear] > 0 & intYear > (length(X) - l + 1)))
  {
    break
  }
  #------------------------------------------------------------------#
  
  if (RSI[intYear] == 0)
    #intYear is not a new changepoint
  {
    if ((changepoint + l) <= intYear)
    {
      #recalculate regime mean and Diff
      #currently Diff remains constant for the entire process /series
      n1 = intYear - changepoint + 1
      for (n in 1:n1)
      {
        R1[n] = X[changepoint + n - 1]
      }
      RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
    }
  }
  
  
  if (RSI[intYear] != 0)
    #regime shift is detected
    #intYear is a new changepoint
  {
    changepoint = intYear
    #recalculate regime mean and Diff
    #currently Diff remains constant for the entire process /series}
    R1 = 0
    for (n in 1:l)
    {
      R1[n] = X[changepoint + n - 1]
    }
    RegimeMean = WeightedAverage(R1,Sigma_s,Huber)
    
  }
}

#Series of RegimeMeans
if (FilteredData == T){
  S = 1
  for (i in 1:length(RSI))
  {
    if (RSI[i] != 0)
    {
      E = (i - 1)
      MeanRegime = WeightedAverage(X[S:E],Sigma_s,Huber)
      vRMean1 = rep(MeanRegime, length(X[S:E]))
      vRMean = c(vRMean, vRMean1)
      S = i
    }
    if (i == length(RSI))
    {
      E = (length(RSI))
      MeanRegime = WeightedAverage(X[S:E],Sigma_s,Huber)
      vRMean1 = rep(MeanRegime, length(X[S:E]))
      vRMean = c(vRMean, vRMean1)
    }
  }
}

if (FilteredData == F){
  X1 = TS[, TIMESERIESindex]
  S = 1
  for (i in 1:length(RSI))
  {
    if (RSI[i] != 0)
    {
      E = (i - 1)
      MeanRegime = WeightedAverage(X1[S:E],Sigma_s,Huber)
      vRMean1 = rep(MeanRegime, length(X1[S:E]))
      vRMean = c(vRMean, vRMean1)
      S = i
    }
    if (i == length(RSI))
    {
      E = (length(RSI))
      MeanRegime = WeightedAverage(X1[S:E],Sigma_s,Huber)
      vRMean1 = rep(MeanRegime, length(X1[S:E]))
      vRMean = c(vRMean, vRMean1)
    }
  }
}

vRMean = vRMean[-1]
RSI_mat[, TIMESERIESindex] = RSI
RMean_mat[, TIMESERIESindex] = vRMean


#### Save output

colnames(RMean_mat) <- colnames(data.timeseries)
colnames(RSI_mat) <- colnames(data.timeseries)

if (preWhitening == T){
  zeri = seq(0, 0, length.out = length(TS[1, ]))
  RSI_mat = rbind(zeri, RSI_mat)
  
  empties = rep(NA, length(TS[1, ]))
  RMean_mat = rbind(empties, RMean_mat)
  
  
  TabTSpw = rbind(empties, TabTSpw)
  colnames(TabTSpw) <- colnames(data.timeseries)
}


# RMean_mat is the mean of each regime
# RSI_mat is the index values showing where shifts occur
# TabTSpw is filtered timeseries


# RMean_mat is the mean of each regime
# RSI_mat is the index values showing where shifts occur
# TabTSpw is filtered timeseries

if (FilteredData == T){
  df_out <- tibble(date_mid = df_in$date_mid,
                   ts_raw = data.timeseries[,2],
                   ts_filtered = TabTSpw[,2],
                   ts_regime_mean = RMean_mat[,2],
                   ts_regime_shift = RSI_mat[,2])
  
  ggplot(df_out, aes(x = date_mid)) +
    geom_line(aes(y = ts_raw)) +
    geom_line(aes(y = ts_filtered), color = "blue") +
    geom_line(aes(y = ts_regime_mean), color = "red")
  
} else {
  df_out <- tibble(date_mid = df_in$date_mid,
                   ts_raw = data.timeseries[,2],
                   ts_regime_mean = RMean_mat[,2],
                   ts_regime_shift = RSI_mat[,2])
  
  ggplot(df_out, aes(x = date_mid)) +
    geom_line(aes(y = ts_raw)) +
    geom_line(aes(y = ts_regime_mean), color = "red")
}

write_csv(df_out, file.path(save.path, "RegimeShifts_rstars.csv"))

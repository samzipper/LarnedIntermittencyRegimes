
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import pastas as ps

# Import input data time series and squeeze to Series object
inputdata = pd.read_csv('../data/Timeseries_InputData.csv', parse_dates=['date_ghcn'],
                        index_col='date_ghcn', squeeze=True)
print('The data type of the series is: %s' % type(inputdata))

# show names of columns
print('Variables are: ', list(inputdata))

# separate into water level data and meteorological data
hydrodata = inputdata[["LWPH4a", "LWPH4b", "LWPH4c", "stage_masl"]]
hydrodata.plot(figsize=(10, 4), subplots=True)
plt.title('Water Level Data');
plt.ylabel('Water Level [masl]');
plt.xlabel('Time [daily data]');

metdata = inputdata[["prcp_mm", "ETo_mm"]]
metdata.plot(figsize=(10, 4))
plt.title('Meteorological Data');
plt.ylabel('Flux [mm/day]');
plt.xlabel('Time [daily data]');

wusedata = inputdata[["WaterUse_m3d"]]/1e4
wusedata.plot(figsize=(10, 4))
plt.title('Pumping Data');
plt.ylabel('Flux [x1e4 m3/day]');
plt.xlabel('Time [annual data distributed within pumping season]');

## build model for full period (not separate wet/dry)
# identify min groundwater level
gw_min = hydrodata["LWPH4b"].min()

# normalize groundwater levels to min value
lwph4b = (hydrodata["LWPH4b"] - gw_min).asfreq("D")

# Create a model object by passing it the observed series
ml_lwph4b = ps.Model(lwph4b, name="LWPH4b")

## build stress models:
# recharge
precSurplus_mm = metdata["prcp_mm"] - metdata["ETo_mm"]
#sm_rech = ps.RechargeModel(metdata["prcp_mm"], metdata["ETo_mm"], rfunc=ps.Gamma, name="recharge")
sm_rech = ps.RechargeModel(metdata["prcp_mm"], metdata["ETo_mm"], rfunc=ps.Exponential, 
                           recharge=ps.rch.FlexModel(), name="recharge")

# pumping
wuse = ps.StressModel(wusedata, rfunc=ps.Hantush, name="well", settings="well", up=False)

# river stage
river = (hydrodata["stage_masl"] - hydrodata["stage_masl"].min()).asfreq("D").fillna(0)
sm_river = ps.StressModel(river, rfunc=ps.One, name="river",
                           settings="waterlevel")

# HPA water levels
lwph4c = (hydrodata["LWPH4c"] - hydrodata["LWPH4c"].min()).asfreq("D") # normalized to minimum observed value
sm_lwph4c = ps.StressModel(lwph4c, rfunc=ps.Exponential, name="LWPH4c",
                           settings="waterlevel")

## add models
ml_lwph4b.add_stressmodel(sm_rech)
ml_lwph4b.add_stressmodel(wuse)
ml_lwph4b.add_stressmodel(sm_river)
#ml_lwph4b.add_stressmodel(sm_lwph4c)

## add threshold transform: https://pastas.readthedocs.io/en/latest/examples/008_threshold_non_linear.ipynb.html
#ml_lwph4b.add_transform(ps.ThresholdTransform()) # slightly better AIC without

# solve
ml_lwph4b.solve(noise=False, report=False) # Solve first without noise model to improve initial parameters
ml_lwph4b.solve(noise=True, initial=False)

# plot
ml_lwph4b.plots.results(figsize=(10, 10))

# diagnostic plots
ml_lwph4b.plots.diagnostics()
ml_lwph4b.stats.diagnostics()

## build separate model for wet conditions, following same workflow and using same stressors
lwph4b_wet = (hydrodata.loc[inputdata["regime_category"] == "Wet", "LWPH4b"] - gw_min).asfreq("D")
plt.plot(lwph4b_wet)

ml_lwph4b_wet = ps.Model(lwph4b_wet, name="LWPH4b_wet")

ml_lwph4b_wet.add_stressmodel(sm_rech)
ml_lwph4b_wet.add_stressmodel(wuse)
ml_lwph4b_wet.add_stressmodel(sm_river)
#ml_lwph4b_wet.add_stressmodel(sm_lwph4c)
#ml_lwph4b_wet.add_transform(ps.ThresholdTransform())

# solve
ml_lwph4b_wet.solve(noise=False, report=False) # Solve first without noise model to improve initial parameters
ml_lwph4b_wet.solve(noise=True, initial=False)

# plot
ml_lwph4b_wet.plots.results(figsize=(10, 10))

# diagnostic plots
ml_lwph4b_wet.plots.diagnostics()
ml_lwph4b_wet.stats.diagnostics()

## build separate model for dry conditions, following same workflow and using same stressors
lwph4b_dry = (hydrodata.loc[inputdata["regime_category"] == "Dry", "LWPH4b"] - gw_min).asfreq("D")
plt.plot(lwph4b_dry)

ml_lwph4b_dry = ps.Model(lwph4b_dry, name="LWPH4b_dry")

ml_lwph4b_dry.add_stressmodel(sm_rech)
ml_lwph4b_dry.add_stressmodel(wuse)
ml_lwph4b_dry.add_stressmodel(sm_river)
#ml_lwph4b_dry.add_stressmodel(sm_lwph4c)
#ml_lwph4b_dry.add_transform(ps.ThresholdTransform())

# solve
ml_lwph4b_dry.solve(noise=False, report=False) # Solve first without noise model to improve initial parameters
ml_lwph4b_dry.solve(noise=True, initial=False)

# plot
ml_lwph4b_dry.plots.results(figsize=(10, 10))

# diagnostic plots
ml_lwph4b_dry.plots.diagnostics()
ml_lwph4b_dry.stats.diagnostics()

## mess around with exporting data
ml_lwph4b_dry.get_contribution("well")
ml_lwph4b_dry.get_contributions("well")
ml_lwph4b_dry.get_block_response("well")
dry_obs = ml_lwph4b_dry.observations()
dry_resid = ml_lwph4b_dry.residuals()

# can't figure out how to get simulated timeseries; instead, get observations and residuals
pd.DataFrame({'obs':  ml_lwph4b_dry.observations(),
              'resid': ml_lwph4b_dry.residuals()}).to_csv("../data/Timeseries_Output_DryModel.csv")

pd.DataFrame({'obs':  ml_lwph4b_wet.observations(),
              'resid': ml_lwph4b_wet.residuals()}).to_csv("../data/Timeseries_Output_WetModel.csv")

pd.DataFrame({'obs':  ml_lwph4b.observations(),
              'resid': ml_lwph4b.residuals()}).to_csv("../data/Timeseries_Output_FullModel.csv")


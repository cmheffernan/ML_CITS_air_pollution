# ML_CITS_air_pollution

Code for the paper "A causal machine-learning framework for studying policy impact on air pollution: A case-study in COVID-19 lockdowns"

The following code produces the analyses of the paper "A causal machine-learning framework for studying policy impact on air pollution: A case-study in COVID-19 lockdowns". 

 ## data folder contains all the data used in the analyses

all_data_5_cities_interpolated_by_monitor.RData contains all NO2 and covariates measured at each site in Boston, New York City, Philadelphia, Baltimore, and Washington DC, in the years 2013-2020
columns: Time (hourly local time), NO2 (NO2 concentration), Latitude/Longitude for that site, State.Name (state in which site is located), monitor (differentiates monitors within a state), month/day/hour/year components of Time, RH (relative humidity), Temp (temperature), wind (wind speed), pressure, daylight (indicator based on sunrise/sunset times in each city on each date), day_index (continuous time variables where 0 corresponds to March 1, and 1 day_index corresponds to 1 day), dow (day of week), weekend (indicator), hod (hour of day), WS (wind speed for Philadelphia), theta (wind direction as an angle), pblh (mixing height), prate (precipitation rate), lag1-lag7 (time lags of precipitation rate), Date_2020 (dates of each timepoint with the year set to 2020)

2020_US_Region_Mobility_Report.csv contains the Google mobility data from 2020 

## code folder contains all codes

### run_models folder

performs main analyses of paper; should be run first 

linear_models.R: fit Linear Constant (LC) and Linear Time-Varying (LTV) models

mean_difference_methods.R: fit Difference between Pre and Post Cutoff (DPPC), Difference between Intervention and Baseline years (DIB), and Difference in Differences (DiD) models

ML_models.R: fit random forest (RF), gradient boosting with 2 packages (GB), and BART models

ML_models.sh: runs ML_models.R in parallel

calculate_ATT_linear_and_ML.R: combines linear and ML results and calculates the ATTs, as well as saving analytical p-values where available (must be run after linear_model.R and ML_models.R)

### bootstrap folder

does hourly and block bootstrapping for the machine learning models 

BB_2models_constant_validation_set.R: block bootstrap for GB and RF at daily, 3 day, and weekly block level

BB_2models_constant_validation_set.sh: run BB_2models_constant_validation_set.R in parallel

NB_2models_constant_validation_set_hourly.R: run naive hourly bootstrap

NB_2models_constant_validation_set_hourly.sh: run NB_2models_constant_validation_set_hourly.R in parallel

### figures folder

makes most of the figures for the paper; some of these files depend on results from code in the run_models folder or the bootstrap folder

ATT_figures.R: plot ATTs with and without significance, and plot R2 values (must be run after calculate_ATT_linear_and_ML.R and mean_difference_methods.R) 

bart_intervals.R: make figure of the BART intervals (must be run after ML_models.R)

block_bootstrap_figs.R: make figures involving bootstrapped intervals (must be run after BB_2models_constant_validation_set.R, NB_2models_constant_validation_set_hourly.R, and calculate_ATT_linear_and_ML.R)

gbm_v_xgb.R: make figure of comparing packages for boosting (must be run after calculate_ATT_linear_and_ML.R)

mobility.R: plots mobility data

philly_supplement_figures.R: plots site locations in Philadelphia

prediction_figures_wholeperiod.R: makes prediction plots (must be run after linear_model.R and ML_models.R)

residuals.R: plots out of sample residuals (must be run after linear_model.R and ML_models.R)

runtime_boosting.R: track runtime for the two packages for boosting

runtime_boosting.sh: shell file to run runtime_boosting.R on cluster

summary_table_and_fig.R: create exploratory plots and table

### residual_autocorrelation folder

calculates order of autocorrelation of residuals from linear and machine learning CITS

residual_autocorrelation_interpolate_data.R: interpolates data to allow for calculation of residual autocorrelation

residual_autocorrelation.R: calculate the order of autoregression of the residuals from fitting RF, GB, LC, LTV (must be run after residual_autocorrelation_interpolate_data.R)

residual_autocorrelation_fig.R: make figure of the order of autocorrelation (must be run after residual_autocorrelation.R)

### sensitivity folder

perform analysis using 3 baseline years, and leaving out monitors in each city

sensitivity_linear_3years.R: run linear models with only 3 baseline years

sensitivity_linear_monitor.R: run linear models leaving out one monitor in the city

sensitivity_mean_difference_methods_3yrs.R: run mean difference models with only 3 baseline years

sensitivity_mean_difference_methods_monitor.R: run mean difference models leaving out one monitor in the city

sensitivity_ML_3years.R: run machine learning models with only 3 baseline years

sensitivity_ML_3years.sh: run sensitivity_ML_3years.R in parallel

sensitivity_ML_monitor.R: run machine learning models leaving out one monitor in the city

sensitivity_ML_monitor.sh: run sensitivity_ML_monitor.R in parallel

sensitivity_figs.R: make figures to summarize sensitivity analyses (must be run after sensitivity_linear_3years.R, sensitivity_linear_monitor.R, sensitivity_mean_difference_methods_3yrs.R, sensitivity_mean_difference_methods_monitor.R, sensitivity_ML_3years.R, sensitivity_ML_monitor.R, calculate_ATT_linear_and_ML.R, mean_difference_methods.R)

### simulation folder

simulation study to check performance of methods

simulation.R: simulates data from a time series with several covariates and fits all methods to it

simulation.sh: run simulation.R in parallel

simulation_combine.R: combine simulation results into one dataframe (must be run after simulation.R)

simulation_combine.sh: run simulation_combine.R on cluster

simulation_figures.R: make figures to summarize simulation study (must be run after simulation_combine.R)

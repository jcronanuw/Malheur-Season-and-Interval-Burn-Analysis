#############################################################################################
# Dead Fuel Moisture From Hourly Weather Data
# Author: Jim Cronan
# Derive dead fuel moisture from hourly weather data using the Nelson-Carlson model.
#Script was created by AI and modified.
#############################################################################################

#---------------------------------------------------------------------------------------------
#Load libraries
#

#---------------------------------------------------------------------------------------------
#Load data
#Data
# Map usernames to file paths
user_paths_data <- c(
  Nat   = "",
  Becky = "",
  jcronan = "C:/Users/jcronan/Box/SIB/Cronan Wade/3_Data/01_Raw_Data/RAWS/"
)

# Detect current user
current_user <- Sys.info()[["user"]]

# Check if user exists in mapping for data files
if (!current_user %in% names(user_paths_data)) {
  stop("No file path configured for this user: ", current_user)
}

#Load data

#RAWS data
raws <- read.table(paste(user_paths_data[current_user],
                         "crow_flat_weather_20140413_20140515.txt",
                         sep = ""), row.names=NULL, header = T, skip = 0)

#Burn schedule data
fm <- read.table(paste(user_paths_data[current_user],
                       "crow_flat_fuel_moisture_20140413_20140515.txt",
                       sep = ""), row.names=NULL, header = T, skip = 0)

#---------------------------------------------------------------------------------------------
#1. Define Model Constants
#First, we establish the physical parameters for the fuel stick. This example uses 
#standard values calibrated for a 10-hour timelag fuel stick (approx. 0.95 cm diameter).

# Nelson Model physical constants for a 10-hour fuel stick
ALPHA_DESORP <- 0.051    # Desorption rate coefficient (dry environment)
ALPHA_ADSORP <- 0.042    # Adsorption rate coefficient (humid environment)
STICK_RADIUS_1 <- 0.1    # Radius of a 1-hr fuel stick in cm
STICK_RADIUS_10 <- 1.59    # Radius of a 10-hr fuel stick in cm
STICK_RADIUS_100 <- 5.09    # Radius of a 100-hr fuel stick in cm
STICK_RADIUS_1K <- 24.00    # Radius of a 1000-hr fuel stick in cm

#---------------------------------------------------------------------------------------------
#2. Calculate Equilibrium Moisture Content (EMC). 
#The model computes the target equilibrium moisture content based on the Simpson 
#formulation, accounting for temperature and relative humidity.

calculate_emc <- function(temp_c, rh) {
  # Convert Celsius to Fahrenheit for standard operational EMC equations
  temp_f <- (temp_c * 9/5) + 32
  h <- rh / 100
  
  if (h < 0.1) {
    emc <- 0.03229 + 0.281073 * h - 0.000578 * h * temp_f
  } else if (h < 0.5) {
    emc <- 0.22239 + 0.170254 * h - 0.000732 * h * temp_f
  } else {
    emc <- 0.21115 + 0.293698 * h - 0.000527 * h * temp_f
  }
  
  # Return EMC as a fraction (e.g., 0.12 for 12%)
  return(max(0.01, emc))
}

#---------------------------------------------------------------------------------------------
#3. Build Iterative Nelson Engine
#This function takes the previous hour's fuel moisture, updates the fuel stick 
#surface temperature using solar radiation (energy balance), and steps the 
#differential moisture equation forward.

nelson_step <- function(prev_fm, temp_c, rh, solar_rad, precip_mm) {
  # 1. Estimate fuel surface temperature based on solar radiation balance
  # General approximation: 100 W/m^2 elevates stick temp by ~0.5°C above air temp
  fuel_temp <- temp_c + (solar_rad * 0.005)
  
  # 2. Calculate current EMC
  emc <- calculate_emc(fuel_temp, rh)
  
  # 3. Handle a Wet-Fuel Phase (Precipitation)
  if (precip_mm > 0) {
    # Direct physical absorption during rain up to fiber saturation (~30%)
    new_fm <- prev_fm + (0.12 * precip_mm / STICK_RADIUS_1)
    new_fm <- min(new_fm, 0.35) # Cap at maximum saturation threshold
    return(new_fm)
  }
  
  # 4. Handle a Dry-Fuel Phase (Diffusion)
  # Select rate coefficient based on adsorption vs desorption direction
  if (emc < prev_fm) {
    alpha <- ALPHA_DESORP
  } else {
    alpha <- ALPHA_ADSORP
  }
  
  # Nelson differential time-step equation (integrated across 1 hour)
  # dM/dt = -alpha * (M - Emc)
  new_fm <- emc + (prev_fm - emc) * exp(-alpha * 1.0)
  
  # Ensure fuel moisture stays within physical bounds (1% to 35%)
  return(max(0.01, min(new_fm, 0.35)))
}

#---------------------------------------------------------------------------------------------
#4. Vectorize Over Weather Data Vector.
#This loop tracks the "memory" of the fuel by feeding the output moisture of hour
#\(t\) as the input moisture for hour \(t+1\).

predict_fuel_moisture <- function(weather_df, initial_fm = 0.15) {
  n <- nrow(weather_df)
  fm_predictions <- numeric(n)
  
  current_fm <- initial_fm
  
  for (i in 1:n) {
    current_fm <- nelson_step(
      prev_fm = current_fm,
      temp_c  = weather_df$temp_c[i],
      rh      = weather_df$rh[i],
      solar_rad = weather_df$solar_rad[i],
      precip_mm = weather_df$precip_mm[i]
    )
    fm_predictions[i] <- current_fm
  }
  
  # Append results as a percentage column to the original data frame
  weather_df$predicted_fm_pct <- fm_predictions * 100
  return(weather_df)
}

#---------------------------------------------------------------------------------------------
#5. Run an Example Simulation.
#Copy and paste this mock dataset to test your newly built R framework:

# Create a sample 24-hour weather dataframe
hourly_data <- data.frame(
  hour = 1:24,
  temp_c = c(15,14,13,12,12,13,15,18,21,24,26,27,28,28,27,25,23,21,19,18,17,16,16,15),
  rh = c(80,82,85,88,88,85,75,65,55,45,40,35,33,33,35,42,50,60,68,72,75,78,79,80),
  solar_rad = c(0,0,0,0,50,200,400,600,750,850,900,920,880,750,550,350,150,20,0,0,0,0,0,0),
  precip_mm = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
)

# Run the predictive model
results <- predict_fuel_moisture(hourly_data, initial_fm = 0.18)

# View the output
print(results[, c("hour", "temp_c", "rh", "predicted_fm_pct")])

#---------------------------------------------------------------------------------------------
#6. Run Nelson-Carlson model in RAWS weather data from the
#Crow Flats RAWS using 1-month of data preceeding spring burns in 2014.
#Data range (hourly): April 12 - May 15, 2014.

#Convert temperature to Celsius
#While parts of this formula will convert temperature to Fahrenheit other
#parts depend on Celsius as an input.
temp_c <- ((raws$Temp_F - 32) * 0.5555556)

#Convert precipitation from inches to millimeters.
precip_mm <- (raws$Precip_in * 25.4)

#Create an column with hours from 0 to most recent date.
hours <- length(raws$Hour):1

crow_flat <- data.frame(hour = hours,
                        temp_c = temp_c,
                        rh = as.numeric(raws$RH), 
                        solar_rad = as.numeric(raws$SR_Wm2), 
                        precip_mm = precip_mm)

#Order so time is from earliest time to latest.
crow_flat_1 <- crow_flat[order(crow_flat$hour),]

# Run the predictive model
results <- predict_fuel_moisture(crow_flat_1, initial_fm = 0.135)

# View the output
print(results[, c("hour", "temp_c", "rh", "predicted_fm_pct")])

#Convert dates from character to date.
fm$date_ymd <- as.Date(fm$Date, "%m-%d-%Y")

#Reverse order of NFDRS fuel moisture data so it can be compared
#with the Nelson-Carlson model outputs in this script.
crow_flat_fm <- data.frame(hour = hours, fm)
cf_fm_1 <- crow_flat_fm[order(crow_flat_fm$hour),]

plot(cf_fm_1$FM_1hr, type = "l", xaxt = "n", col = "red")
axis(1, seq(1,length(cf_fm_1$date_ymd),
            length(cf_fm_1$date_ymd)/length(unique(cf_fm_1$date_ymd))), 
     unique(cf_fm_1$date_ymd))
lines(results$predicted_fm_pct, col = "green")


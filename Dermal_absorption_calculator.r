
## Calculation of dermal absorption for in vitro studies based on BFR template accessible at: https://www.efsa.europa.eu/en/applications/pesticides/tools 
## EFSA Panel on Plant Protection Products and their Residues (PPR); Guidance on Dermal Absorption. EFSA Journal 2012;10(4):2665. [30 pp.] doi:10.2903/j.efsa.2012.2665.
## Available online: www.efsa.europa.eu/efsajournal.htm



# Convert matrix to data frame for easier manipulation
data <- as.data.frame(data)

# Calculate T 0.5 absorption
# Variables for calculating T 0.5 absorption are: Receptor fluid and T 0.5 receptor fluid 
T.5_Abs <- (data["T.5_receptor_fluid", ] / data["receptor_fluid", ]) * 100

# Add "T.5_Abs" as a new row in the data frame
data <- rbind(data, T.5_Abs = T.5_Abs)


# If > 75% of total absorption occurs within half of the study duration, tape-stripped material can be excluded
# Variables for calculating absorption sum are absorbed dose, strips 3 +, and skin preparation
# Define the function to calculate the absorption sum
calculate_D_Abs <- function(data) {
  D_Abs <- numeric(ncol(data))
  for (i in 1:ncol(data)) {
    if (data["T.5_Abs", i] <= 75) {
      F[i] <- data["Abs_dose", i] + data["Strips_3_plus", i] + data["Skin_prep", i]
    } else {
      F[i] <- data["Abs_dose", i] + data["Skin_prep", i]
    }
  }
  return(D_Abs)
}

# Calculate absorption sum and add it to the data frame
data["D_Abs", ] <- calculate_D_Abs(data)


# Define the function to calculate the new variable dermal absorption
# If mean total recovery is < 95% and replicate total recovery is < 95%: the dermal absorption value is normalized 
# If the absorption sum is < 5% and total recovery is < 95%, the missing recovery percentage is added to the dermal absorption value
calculate_derm_Abs <- function(data) {
  Total_recovery_mean <- mean(data["Total_recovery", ], na.rm = TRUE)
  derm_Abs <- numeric(ncol(data))
  for (i in 1:ncol(data)) {
    if (Total_recovery_mean < 95 && data["Total_recovery", i] < 95) {
      if (data["D_Abs", i] < 5) {
        derm_Abs[i] <- data["D_Abs", i] + (100 - data["Total_recovery", i])
      } else {
        derm_Abs[i] <- (data["D_Abs", i] / data["Total_recovery", i]) * 100
      }
    } else {
      derm_Abs[i] <- data["D_Abs", i]
    }
  }
  return(derm_Abs)
}

# Calculate dermal absorption and add it to the data frame
data["derm_Abs", ] <- calculate_derm_Abs(data)


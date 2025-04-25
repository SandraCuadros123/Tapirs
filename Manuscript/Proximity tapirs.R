install.packages("ctmm")
library('dplyr')
library('ctmm')

# Load overlap data
t_relatedness <- read.csv("C:/Users/sandracd/OneDrive - UBC/Documents/Tapirs project/t_relatedness_updated.csv")
t_relatedness$proximity_low <- NA  #create columns with NA values that will be replaced later
t_relatedness$proximity_est <- NA
t_relatedness$proximity_high <- NA


# Load telemetry data
tapirs <-
  read.csv('C:/Users/sandracd/OneDrive - UBC/Documents/Tapirs project/pantanal.csv') %>%
  as.telemetry(timeformat = '%Y-%m-%d %H:%M')

# Load models data
FITS <- readRDS("C:/Users/sandracd/OneDrive - UBC/Documents/Tapirs project/Tapirs_FITS.rds")

# Using tryCatch

for(i in 1:nrow(t_relatedness)){
      ANIMAL_1 <- t_relatedness[i,'tapir_1']
      ANIMAL_2 <- t_relatedness[i,'tapir_2']
      TRACKING_DATA <- tapirs[c(ANIMAL_1, ANIMAL_2)]
      MODELS <- list(FITS[ANIMAL_1][[1]], FITS[ANIMAL_2][[1]])
      PROXIMITY <- tryCatch(
        {
      PROXIMITY <- proximity(data = TRACKING_DATA, CTMM = MODELS , GUESS=ctmm(error=FALSE))},
      error=function(err){
      PROXIMITY <- c(NA,NA,NA)
      return(PROXIMITY)
      }
      )
      t_relatedness[i, c("proximity_low", "proximity_est", "proximity_high")] <- PROXIMITY
      write.csv(t_relatedness, "C:/Users/sandracd/Downloads/t_proxfinal.csv", row.names = FALSE)
    }
    
#Errors for ind 
#12 ,14 17,19,22,23,25,27,29,30, 32, 36,37,38,40,46,47,49,56,57,59, 61, 62, 63, 64, 66

#new errors found in....:
#67

#For loops to run proximity manually - no tryCatch
for(i in 1:nrow(t_relatedness)){
  ANIMAL_1 <- t_relatedness[i,'tapir_1']
  ANIMAL_2 <- t_relatedness[i,'tapir_2']
  TRACKING_DATA <- tapirs[c(ANIMAL_1, ANIMAL_2)]
  MODELS <- list(FITS[ANIMAL_1][[1]], FITS[ANIMAL_2][[1]])
  PROXIMITY <- proximity(data = TRACKING_DATA, CTMM = MODELS , GUESS=ctmm(error=FALSE))
  t_relatedness[i, c("proximity_low", "proximity_est", "proximity_high")] <- PROXIMITY
  write.csv(t_relatedness, "C:/Users/sandracd/OneDrive - UBC/Documents/Tapirs project/t_proximity_new2.csv", row.names = FALSE)
}



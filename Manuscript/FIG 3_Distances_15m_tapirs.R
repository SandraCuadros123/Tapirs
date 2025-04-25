# Distance ----
library('ggplot2')
library('dplyr')
library('ctmm')
library('readr')
theme_set(theme_classic())

# Load overlap data
t_relatedness <- read.csv("C:/Users/sandracd/OneDrive - UBC (1)/GitHub/Tapirs/t_relatedness_updated.csv")
#rename connection column to pair_ID so that colum name is the same for both dfs and rearrange
t_relatedness$pair_ID <- paste(t_relatedness$tapir_1, t_relatedness$tapir_2, sep = "_")

# Load telemetry data
tapirs <-
  read.csv('C:/Users/sandracd/OneDrive - UBC (1)/GitHub/Tapirs/pantanal.csv') %>%
  as.telemetry(timeformat = '%Y-%m-%d %H:%M')
# Load models data
FITS <- readRDS("C:/Users/sandracd/OneDrive - UBC (1)/GitHub/Tapirs/Tapirs_FITS.rds")


#Calculate the distance statistics
RES <- list()

for (i in 1:nrow(t_relatedness)) {
  ANIMAL_1 <- as.character(t_relatedness[i, 'tapir_1']) 
  ANIMAL_2 <- as.character(t_relatedness[i, 'tapir_2'])
  TRACKING_DATA <- tapirs[c(ANIMAL_1, ANIMAL_2)]
  MODELS <- list(FITS[[ANIMAL_1]], FITS[[ANIMAL_2]])
  
  DISTANCES_RES <- tryCatch({
    distances_result <- distances(data = TRACKING_DATA, CTMM = MODELS, GUESS = ctmm(error = FALSE))
    data.frame(pair_ID = paste(ANIMAL_1, ANIMAL_2, sep = "_"),
               distance_low = distances_result$low, 
               distance_est = distances_result$est, 
               distance_high = distances_result$high,
               t = distances_result$t,
               timestamp = distances_result$timestamp)
  }, error = function(err) {
    data.frame(pair_ID = paste(ANIMAL_1, ANIMAL_2, sep = "_"),
               distance_low = NA,
               distance_est = NA,
               distance_high = NA,
               t = NA, 
               timestamp = NA)
  })
  
  RES[[i]] <- DISTANCES_RES
  
  #Save the results on the fly
  #save(RES, "C:/Users/sandracd/Downloads/tapir_distances.Rda")
}



#Turn the list of list into a data frame to save it
DATA_distance <- do.call(rbind, RES)
saveRDS(DATA_distance, file = "C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/DATA_distance.RDS")

#import the distances
DATA_distance <- readRDS("C:/Users/sandracd/OneDrive - UBC (1)/GitHub/Tapirs/DATA_distance.RDS")






#calculate the number of encounters for each unique pair
t_relatedness$encounter_count <- NA
unique_pairs <- unique(t_relatedness$pair_ID)
for (i in 1:length(unique_pairs)){
  subset_A <- DATA_distance[DATA_distance$pair_ID == unique_pairs[i],]
  
  # Count the number of times "distance_est" is below 100m
  encounter_count <- sum(subset_A$distance_est < 100)
  
  #save results
  t_relatedness[t_relatedness$pair_ID == unique_pairs[i], "encounter_count"] <- encounter_count
  
}


aggregate(encounter_count ~ Relatedness, data = t_relatedness, mean)




# Export the final results
write.csv(DATA_distance, "C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/Data_distancetapirs.csv", row.names = FALSE)

#Plot data
ggplot(data = t_relatedness, aes(x = value, y = encounter_count, col = Relatedness)) +
  geom_point() +
  scale_y_log10()+
  ylab("Encounters")+
  xlab("Homerange overlap")+
  scale_color_manual(values=c("#0066CC", "#FF9966"))
  #+
#scale_x_log10()


ggplot(data = t_relatedness, aes(x = value, y = encounter_count, color = Relatedness)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +  # Add linear models per group
  scale_y_log10() +
  ylab("Encounters") +
  xlab("Homerange overlap") +
  scale_color_manual(values = c("#0066CC", "#FF9966"))


ggsave(plot = last_plot(), filename = "C:/Users/sandracd/OneDrive - UBC (1)/GitHub/Tapirs/Manuscript/FIG 3.png", device = NULL,
       path = NULL, scale = 1, width = 6.86, height = 6, units = "in", dpi = 600)


#Correlation values
t_relatedness %>%
  filter(!is.na(value), !is.na(encounter_count)) %>%
  group_by(Relatedness) %>%
  summarise(
    n = n(),
    correlation = if (n() > 1) cor(value, encounter_count, method = "pearson") else NA_real_,
    p_value = if (n() > 1) cor.test(value, encounter_count, method = "pearson")$p.value else NA_real_
  )

#Find 0 values for encounters
t_relatedness %>%
  filter(!is.na(encounter_count)) %>%
  group_by(Relatedness) %>%
  summarise(
    total = n(),
    zero_encounters = sum(encounter_count == 0),
    proportion_zero = zero_encounters / total
  )


#THIS SECTION IS NOT NEEDED
#check for NA values
any(is.na(distance_df))

#locate NA values within the dataframe
distance_df[!complete.cases(distance_df), ]
distance_df <- na.omit(distance_df)
#Verify no missing values in cleaned data
any(is.na(distance_df))

#Compare values 100m in family units
sum(distance_df$distance_est < 100)
sum(distance_df$distance_est[distance_df$Relatedness == "Within" & distance_df$distance_est] < 100)
sum(distance_df$distance_est[distance_df$Relatedness == "Between" & distance_df$distance_est] < 100)












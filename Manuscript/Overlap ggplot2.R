library('dplyr')   # for data wrangling (e.g., tibble, %>%, transmute())
library('purrr')   # for functional programming (e.g., map_***(), map2_***())
library('tidyr')   # for data wrangling (e.g., pivot_*())
library('ggplot2') # for fancy plots
library('ctmm') # using the github version (0.6.1)
library('tidyverse')
library('viridis')
install.packages("ggrepel")

FITS <- lapply(1:46, function(i) ctmm.select(tapirs[[i]],GUESS[[i]]) )


FITS <- readRDS("C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/Tapirs_FITS.rds")
UDS <- readRDS("C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/Tapirs_UDS.rds")

#####
library('lubridate')
library('stats')


############# Running overlap on an array of data ###################
# Importing data
tapirs <-
  read.csv('C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/pantanal.csv') %>%
  as.telemetry(timeformat = '%Y-%m-%d %H:%M')


# Gaussian overlap between these two buffalo
overlap(FITS)

# Create aligned UDs
UDS <- akde(tapirs[1:46],FITS)

# Evaluate overlap
over <- overlap(UDS)


# Evaluating the values of your array to check they are the targeted ones
all(over$CI[ , , 1] < over$CI[ , , 2])
all(over$CI[ , , 1] > over$CI[ , , 2])
over$CI[ , , 2]
estimates <- over$CI[ , , 2] # take the second matrix of the 3d array

# Set all values not in the lower triangle to NA
estimates[! lower.tri(estimates)] <- NA 

# Change from matrix to data frame
estimates.df <- data.frame(estimates) 

# Add a column of the tapir row names
estimates.df$tapir_1 <- rownames(estimates.df) 

# Pivot to long format and drop all the NA values
estimates.df2 <- pivot_longer(estimates.df, cols = -c(tapir_1), values_drop_na = TRUE) 


################### Plotting my data #############
##Create dfs
estimates.df2 <- rename(estimates.df2, tapir_2=name)
tapirtibble <- mutate(estimates.df2, connections=paste(tapir_1, 'to', tapir_2))



tapirtibble <- write.csv()


tapirID <- estimates.df2$tapir_1
tapirIDunique <- c(unique(tapirID), "PA_03_BANDAID")

# Run function to identify angles and positions for data
add_coords <- function(.t, colname, unique_labels, rad_offset = 0) {
  colnames(.t)[colnames(.t) == colname] <- 'label'
  
  .t <-
    .t %>%
    mutate(label_angle =
             map_dbl(label,
                     function(x) {
                       # find position
                       i <- which(unique_labels == x) - 1
                       
                       # find angle on unit circle
                       angle <- 0.5 - 2 / length(unique_labels) * i + rad_offset
                       return(angle)
                     }),
           label_x = cospi(label_angle),
           label_y = sinpi(label_angle))
  
  label_indices <- which(grepl('label', colnames(.t)))
  label_cols <- colnames(.t)[label_indices]
  new_cols <- stringi::stri_replace(label_cols, replacement = colname, regex = 'label')
  
  colnames(.t)[label_indices] <- new_cols
  
  return(.t)
}



# Organize the data in your df and add coordinates

t <- tapirtibble %>% 
  add_coords(colname = 'tapir_1', unique_labels = tapirIDunique) %>% # add coords on unit circle
    add_coords(colname = 'tapir_2', unique_labels = tapirIDunique)



# find all vertices for labels
vertices <- tibble(label = unique(c(t$tapir_1, t$tapir_2))) %>%
  add_coords(colname = 'label', unique_labels = tapirIDunique)



#Add family units to vertices
#cant bind them bcs one tapir has 2 families.. cheater
familyunits <- read.csv('C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/family units hierarchy2.csv')
vertices <- left_join(vertices, rename(familyunits, label=tapir), by='label') %>% 
  mutate(label=substr(x = label, start = nchar("PA_xx_")+1, stop =nchar(label)))

# basic plot with segments
ggplot() +
  coord_equal() +
  geom_segment(aes(x = tapir_1_x, xend = tapir_2_x, y = tapir_1_y,
                   yend = tapir_2_y, alpha = value),
               t, lwd = 1) +
  geom_point(aes(label_x, label_y), vertices) +
  geom_label(aes(label_x, label_y, label = label, color = ), vertices, show.legend = FALSE) +
  scale_alpha(range = c(0, 1), limits = c(0, 1))

# Organize data and plot it
t %>%
  select(-c(tapir_1_angle, tapir_2_angle)) %>%
  pivot_longer(c(tapir_1_x, tapir_1_y, tapir_2_x, tapir_2_y),
               names_pattern = '(.+)_(.+)', 
               names_to = c('position', 'coord'),
               values_to = 'coordinate') %>%
  pivot_wider(names_from = coord, values_from = coordinate) %>%
  bind_rows(tibble(connections = unique(t$connections),
                   x = 0,
                   y = 0,
                   position = 'center') %>%
              left_join(select(t, connections, value), by = 'connections')) %>%
  mutate(position = factor(position, levels = c('tapir_1', 'center', 'tapir_2'))) %>%
  arrange(connections, position) %>%
  ggplot() +
  coord_equal() +
  ggforce::geom_bezier((aes(x, y, group = connections, alpha = value, linewidth = value, color = value)), show.legend = FALSE)+
  geom_label(aes(label_x, label_y, label = label, fill = family), vertices,
             size=2L, label.padding = unit(0.10, "lines"), show.legend = FALSE, alpha = 0.5)+
  scale_alpha_continuous(range = c(0, 1), limits = c(0, 1))+
  scale_size_continuous(range = c(0, 1), limits = c(0, 1))+
  scale_fill_viridis_d('Overlap', option = 'A', direction = -1, begin = 0.1)+
  theme_void()



#Using geom_label
t %>%
  select(-c(tapir_1_angle, tapir_2_angle)) %>%
  pivot_longer(c(tapir_1_x, tapir_1_y, tapir_2_x, tapir_2_y),
               names_pattern = '(.+)_(.+)', 
               names_to = c('position', 'coord'),
               values_to = 'coordinate') %>%
  pivot_wider(names_from = coord, values_from = coordinate) %>%
  bind_rows(tibble(connections = unique(t$connections),
                   x = 0,
                   y = 0,
                   position = 'center') %>%
              left_join(select(t, connections, value), by = 'connections')) %>%
  mutate(position = factor(position, levels = c('tapir_1', 'center', 'tapir_2'))) %>%
  arrange(connections, position) %>%
  ggplot() +
  coord_equal() +
  ggforce::geom_bezier((aes(x, y, group = connections, alpha = value, size = value, color = value)), show.legend = FALSE)+
  geom_label( #dont use geom_label_repel bcs labels float too much
    aes(label_x, label_y, label = label, fill = family),
    vertices,size=1.5L, label.padding = unit(0.10, "lines"), 
    show.legend = FALSE, alpha = 0.3)+
  scale_color_gradient2(midpoint =0.5, low = "white", mid ="grey50", high = "red", na.value =  NA)+
  scale_size_continuous(range = c(0, 1), limits = c(0, 1))+
  scale_fill_viridis_d('Overlap', option = 'A', direction = -1, begin = 0.1)+
  theme_void()





library(ggplot2)
library(ggforce)
library(dplyr)
library(tidyr)
library(viridis)

# Create the plot and assign it to a variable
plot <- t %>%
  select(-c(tapir_1_angle, tapir_2_angle)) %>%
  pivot_longer(c(tapir_1_x, tapir_1_y, tapir_2_x, tapir_2_y),
               names_pattern = '(.+)_(.+)', 
               names_to = c('position', 'coord'),
               values_to = 'coordinate') %>%
  pivot_wider(names_from = coord, values_from = coordinate) %>%
  bind_rows(tibble(connections = unique(t$connections),
                   x = 0,
                   y = 0,
                   position = 'center') %>%
              left_join(select(t, connections, value), by = 'connections')) %>%
  mutate(position = factor(position, levels = c('tapir_1', 'center', 'tapir_2'))) %>%
  arrange(connections, position) %>%
  ggplot() +
  coord_equal() +
  ggforce::geom_bezier(
    aes(x = x, y = y, group = connections, alpha = value, size = value, color = value), 
    show.legend = FALSE
  ) +
  geom_label( # Use geom_label instead of geom_label_repel
    aes(x = label_x, y = label_y, label = label, fill = family),
    vertices,
    size = 3, 
    label.padding = unit(0.10, "lines"), 
    show.legend = FALSE, 
    alpha = 0.3
  ) +
  scale_color_gradient2(midpoint = 0.5, low = "white", mid = "grey50", high = "red", na.value = NA) +
  scale_size_continuous(range = c(0, 1), limits = c(0, 1)) +
  scale_fill_viridis_d('Overlap', option = 'A', direction = -1, begin = 0.1) +
  theme_void()

# Save the plot to a file
ggsave("FIG 1.png", path = 'C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/Manuscript', plot = plot, width = 10, height = 8, dpi = 300, bg = 'white')


#trial according to Mr G with geom text
t %>%
  select(-c(tapir_1_angle, tapir_2_angle)) %>%
  pivot_longer(c(tapir_1_x, tapir_1_y, tapir_2_x, tapir_2_y),
               names_pattern = '(.+)_(.+)', 
               names_to = c('position', 'coord'),
               values_to = 'coordinate') %>%
  pivot_wider(names_from = coord, values_from = coordinate) %>%
  bind_rows(tibble(connections = unique(t$connections),
                   x = 0,
                   y = 0,
                   position = 'center') %>%
              left_join(select(t, connections, value), by = 'connections')) %>%
  mutate(position = factor(position, levels = c('tapir_1', 'center', 'tapir_2'))) %>%
  arrange(connections, position) %>%
  ggplot() +
  coord_equal() +
  ggforce::geom_bezier(
    aes(x = x, y = y, group = connections, alpha = value, size = value, color = value),  
    show.legend = FALSE
  ) +
  geom_text(
    aes(x = label_x, y = label_y, label = label, colour = label),  # Ensure 'family' is discrete
    size = 1.5, 
    vertices,
    show.legend = FALSE, 
    alpha = 0.3
  ) +
  scale_color_gradient2(midpoint = 0.5, low = "white", mid = "grey50", high = "red", na.value = NA) +  # For continuous 'value'
  scale_size_continuous(range = c(0, 1), limits = c(0, 1)) +
  scale_colour_brewer(palette = "Set1") +  # For discrete 'family'
  theme_void()


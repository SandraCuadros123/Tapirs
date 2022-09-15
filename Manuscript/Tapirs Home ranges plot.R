install.packages('sf')
install.packages('lmPerm')
install.packages('ggpubr') 
library('dplyr')   # for data wrangling (e.g., tibble, %>%, transmute())
library('sf')
library('scales')
library('tidyr')   # for data wrangling (e.g., pivot_*())
library('viridis')
library('purrr') # for map_***()
library('ggplot2')
library('tidyverse')
library('gridExtra')
library('RColorBrewer')
library('lmPerm') # for aovp analysis
library('ggpubr') # for arranging plots
library('ctmm') # using the github version (0.6.1)


#CREATING UDS AND PLOTTING########################
# create familyunits ----

familyunits <- read.csv("C:/Users/sandracd/Documents/Tapirs project/family units hierarchy.csv")

#load UDS object
UDS <- readRDS("C:/Users/sandracd/Documents/Tapirs project/Tapirs_UDS.rds") %>%
  map(\(ud) { 
    SpatialPolygonsDataFrame.UD(ud) %>% #ud into spatial polygon
      st_as_sf() %>% #string as a simple feature
      st_transform(crs = '+proj=longlat') %>% #remove projections from UD
      return()
  }) %>% #
  bind_rows() %>%  #bind rows into one list
  mutate(est = grepl(pattern = 'est', x = name),
         tapir = map_chr(name, function(.name) {
           substr(x = .name,
                  start = 1,
                  stop = gregexpr(pattern = ' ', text = .name)[[1]][1] - 1)
         })) %>% #identify everything after the tapir name, identify the first element in the list and dropeverything else
  filter(est) %>% # remove CIs
  left_join(y = familyunits, by = 'tapir') # add familyunits to UDs

#Plot UDS colored by tapir
ggplot(UDS) +
  geom_sf(aes(color = tapir), fill = 'transparent') +
  scale_color_viridis_d(option = 'A', direction = -1, begin = 0.1)+
  theme_bw()

#Plot UDS colored by family units
FIG <- ggplot(UDS) +
  geom_sf(aes(color = family), fill = 'transparent') +
  scale_color_viridis_d(option = 'A', direction = -1, begin = 0.1)+
  theme_bw()



# CREATING BOXPLOTS################
#Load data
t_relatedness <- read.csv("C:/Users/sandracd/Documents/Tapirs project/t_relatedness.csv")

#Plot data basic details
ggplot(t_relatedness, aes(x= Relatedness, y= value))+
  geom_boxplot(color="black", fill="light green", alpha=0.2)+
  theme_bw()

#Plot data
FIG_family <- ggplot(t_relatedness, aes(x= Relatedness, y= value)) +
  geom_boxplot(color="black", alpha=0.2, aes(fill=Relatedness), show.legend = FALSE) +
  scale_fill_manual(values = c("#FFFF99", "blue")) +
  labs(x=NULL, y=NULL)+
  theme_bw()


#Load data
t_relatedness_detailed <- read.csv("C:/Users/sandracd/Documents/Tapirs project/t_relatedness_detailed.csv")

#Plot data 
ggplot(t_relatedness_detailed, aes(x= Relationship, y= value)) +
  geom_boxplot(color="black", alpha=0.5, aes(fill=Relationship), show.legend = FALSE) +
  scale_fill_manual(values = c("light blue", "#FFCCFF", "gray")) +
  labs(x=NULL, y=NULL)+
  theme_bw()

#Extract stats data
#Family units
t_relatedness_Within <- t_relatedness [which(t_relatedness$Relatedness=='Within'),]
summary(t_relatedness_Within)

t_relatedness_Between <- t_relatedness [which(t_relatedness$Relatedness=='Between'),]
summary(t_relatedness_Between)






#Type of relationship
t_relationship_pair <- t_relatedness_detailed [which(t_relatedness_detailed$Relationship =='Pair'),]
summary(t_relationship_pair)
t_relationship_offs <- t_relatedness_detailed [which(t_relatedness_detailed$Relationship =='Offspring'),]
summary(t_relationship_offs)
t_relationship_parent_off <- t_relatedness_detailed [which(t_relatedness_detailed$Relationship =='Parents_offsp'),]
summary(t_relationship_parent_off)


#plot both  figs together
FIG2 <- grid.arrange(FIG, FIG_family, ncol = 2)
FIG2 <- ggarrange(ggarrange(FIG, FIG_family, ncol=2, heights = c(1,1), widths = c(5,2), legend = 'none'))

ggsave(FIG2, file = "HR_Fig2.png", width = 6.86, height = 8, units = "in", dpi = 600)


#Calculating significant differences##############
summary(aovp(value~Relatedness,t_relatedness))

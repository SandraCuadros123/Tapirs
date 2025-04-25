library('ggplot2')
library('dplyr')
library('ctmm')
theme_set(theme_classic())

# Load the data
t_proximity <- read.csv("C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/t_proxfinal.csv")


# plot the data
FIG4 <- ggplot(t_proximity)+ geom_point(aes(x=value, y=proximity_est, color=Relatedness))+
  geom_segment(aes(x=value, xend=value, y=proximity_low, yend=proximity_high, color=Relatedness))+
  geom_smooth(aes(x=value, y=proximity_est, color=Relatedness), se=FALSE, span = 0.2)+
  geom_hline(aes(yintercept=1))+ #adding a model
  scale_color_manual(values=c("#0066CC", "#FF9966"))+
  ylab("Proximity ratio")+ #relabel
  xlab("Overlap")+
  scale_y_log10()


ggsave(FIG4, file = "FIG 4.png", width = 6.86, height = 8, units = "in", dpi = 600, path = "C:/Users/sandracd/OneDrive - UBC/GitHub/Tapirs/Manuscript")




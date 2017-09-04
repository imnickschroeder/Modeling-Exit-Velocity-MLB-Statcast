# Data for Trout
#Trout <- read.csv("~/Capstone/Data/545361_data.csv")
Trout <- read.csv("~/Capstone/Data_Final/545361_data.csv")

# Packages
library(dplyr)

# filter each where balls in play
Trout_1 <- filter(Trout,hit_distance_sc != "null")

Trout_2 <- Trout_1[,c("pitch_type","start_speed","x0","z0","spin_dir","spin_rate","break_angle","break_length",
                        "zone","p_throws","hit_location","bb_type","balls","strikes","pfx_x","pfx_z","px","pz",
                        "outs_when_up","inning","hc_x","hc_y","vx0","vy0","vz0","ax","ay",
                        "az","sz_top","sz_bot","hit_distance_sc","hit_speed","hit_angle","effective_speed","release_spin_rate",
                        "release_extension")]

# How to properly change a variable from factor to numeric
# Copy and paste this, then hit control find to replace properly
# as.numeric(levels(Trout_2$))[Trout_2$] 

# we need to check the format on each of the variables
Trout_2$pitch_type <- as.factor(Trout_2$pitch_type)
Trout_2$start_speed <- as.numeric(levels(Trout_2$start_speed))[Trout_2$start_speed]
Trout_2$x0 <- as.numeric(levels(Trout_2$x0))[Trout_2$x0]
Trout_2$z0 <- as.numeric(levels(Trout_2$z0))[Trout_2$z0]
Trout_2$spin_dir <- as.numeric(levels(Trout_2$spin_dir))[Trout_2$spin_dir]
Trout_2$spin_rate <- as.numeric(levels(Trout_2$spin_rate))[Trout_2$spin_rate]
Trout_2$break_angle <- as.numeric(levels(Trout_2$break_angle))[Trout_2$break_angle]
Trout_2$break_length <- as.numeric(levels(Trout_2$break_length))[Trout_2$break_length]
Trout_2$hit_location <- as.factor(Trout_2$hit_location)
Trout_2$bb_type <- as.factor(Trout_2$bb_type)
Trout_2$balls <- as.factor(Trout_2$balls)
Trout_2$strikes <- as.factor(Trout_2$strikes)
Trout_2$pfx_x <- as.numeric(levels(Trout_2$pfx_x))[Trout_2$pfx_x]
Trout_2$pfx_z <- as.numeric(levels(Trout_2$pfx_z))[Trout_2$pfx_z]
Trout_2$px <- as.numeric(levels(Trout_2$px))[Trout_2$px]
Trout_2$pz <- as.numeric(levels(Trout_2$pz))[Trout_2$pz]
#Trout_2$on_3b <- as.factor(Trout_2$on_3b)
#Trout_2$on_2b <- as.factor(Trout_2$on_2b)
#Trout_2$on_1b <- as.factor(Trout_2$on_1b)
Trout_2$outs_when_up <- as.factor(Trout_2$outs_when_up)
Trout_2$inning <- as.factor(Trout_2$inning)
Trout_2$vx0 <- as.numeric(levels(Trout_2$vx0))[Trout_2$vx0]
Trout_2$vy0 <- as.numeric(levels(Trout_2$vy0))[Trout_2$vy0]
Trout_2$vz0 <- as.numeric(levels(Trout_2$vz0))[Trout_2$vz0]
Trout_2$ax <- as.numeric(levels(Trout_2$ax))[Trout_2$ax]
Trout_2$ay <- as.numeric(levels(Trout_2$ay))[Trout_2$ay]
Trout_2$az <- as.numeric(levels(Trout_2$az))[Trout_2$az]
Trout_2$hit_distance_sc <- as.numeric(levels(Trout_2$hit_distance_sc))[Trout_2$hit_distance_sc]
Trout_2$hit_speed <- as.numeric(levels(Trout_2$hit_speed))[Trout_2$hit_speed]
Trout_2$hit_angle <- as.numeric(levels(Trout_2$hit_angle))[Trout_2$hit_angle]
Trout_2$effective_speed <- as.numeric(levels(Trout_2$effective_speed))[Trout_2$effective_speed]
Trout_2$release_spin_rate <- as.numeric(levels(Trout_2$release_spin_rate))[Trout_2$release_spin_rate]
Trout_2$release_extension <- as.numeric(levels(Trout_2$release_extension))[Trout_2$release_extension]

str(Trout_2)

Trout_2$count <- paste(Trout_2$balls,Trout_2$strikes,sep="-")

# PCA for hit quality: Can we reduce the three hit quality variables to better understand a well-hit ball?
pca_data <- Trout_2[,c("hit_distance_sc","hit_speed","hit_angle")]

# First, some plots to look at how the variables correlate
plot(hit_distance_sc~hit_angle,data=pca_data)
plot(hit_distance_sc~hit_speed,data=pca_data)
plot(hit_speed~hit_angle,data=pca_data)

# First, scale the data 
pca.scale <- scale(pca_data)
pca.scale
pca.trout <- princomp(pca.scale)
pca.trout
pca.trout$loadings
summary(pca.trout)
pca.trout$scores

plot(pca.trout$scores,xlim=c(-4,6),ylim=c(-15,15))
points(0,0,pch="+",cex=1.5)
title("Plot of PC Scores")

# scree plot
plot(pca.trout,type="line")

# 3d plots
# install.packages("scatterplot3d")
library(scatterplot3d)
scatterplot3d(pca_data$hit_distance_sc,pca_data$hit_speed,pca_data$hit_angle)
# Spinning 3d Scatterplot
# install.packages("rgl")
library(rgl)
plot3d(pca_data$hit_distance_sc,pca_data$hit_speed,pca_data$hit_angle)

# COmpute the PCA scores 
# What we will compute
pca.trout$scores

# loadings or coefficents
pca.trout$loadings

# First score column
pca_hit_1 <- pca.scale%*%pca.trout$loadings[,1]
pca_hit_2 <- pca.scale%*%pca.trout$loadings[,2]

pca_hit_1_2 <- data.frame(pca_hit_1,pca_hit_2)

pca_plot_data <- data.frame(Trout_2,pca_hit_1_2)

# Plot the two scores and color by pitch type
plot(pca_hit_1~pca_hit_2,data=pca_plot_data,pch=19,col=pitch_type)
legend(2.5,3,legend=pca_plot_data$pitch_type)

# We have a lot of colors, let's create a new variable and break these into smaller groups
# There has to be a better way....
pca_plot_data$pitch_fast_slow <- ifelse(pca_plot_data$pitch_type=="FA","Fastball Type",
                                        ifelse(pca_plot_data$pitch_type=="FF","Fastball Type",
                                               ifelse(pca_plot_data$pitch_type=="FT","Fastball Type",
                                                      ifelse(pca_plot_data$pitch_type=="FC","Fastball Type",
                                                             ifelse(pca_plot_data$pitch_type=="FS","Fastball Type",
                                                                    ifelse(pca_plot_data$pitch_type=="SI","Fastball Type",
                                                                           ifelse(pca_plot_data$pitch_type=="SF","Fastball Type","Off-Speed Type")))))))

library(ggplot2)
qplot(pca_hit_1,pca_hit_2,data=pca_plot_data,color=pitch_type)

qplot(pca_hit_1,pca_hit_2,data=pca_plot_data,color=pitch_fast_slow)+theme_bw()

# Create plot with color on hit outcome
hit_pitch_pca_data <- data.frame(Trout_1,pca_plot_data)
hit_pitch_pca_data$Event <- ifelse(hit_pitch_pca_data$events=="Double","Hit",
                                    ifelse(hit_pitch_pca_data$events=="Home Run","Home run",
                                           ifelse(hit_pitch_pca_data$events=="Single","Hit",
                                                  ifelse(hit_pitch_pca_data$events=="Triple","Hit",
                                                         ifelse(hit_pitch_pca_data$events=="Grounded Into DP","Groundout",
                                                                ifelse(hit_pitch_pca_data$events=="Field Error","Groundout",
                                                                       ifelse(hit_pitch_pca_data$events=="Fielders Choice Out","Groundout",
                                                                              ifelse(hit_pitch_pca_data$events=="Groundout","Groundout",
                                                                                     ifelse(hit_pitch_pca_data$events=="Forceout","Groundout","Flyout")))))))))
qplot(pca_hit_1,pca_hit_2,data=hit_pitch_pca_data,color=Event)+theme_bw()+ggtitle("First two scores plotted by event")+xlab("Component 1")+ylab("Component 2")
qplot(pca_hit_1,pca_hit_2,data=hit_pitch_pca_data,color=events)


### PCA ON ALL PITCHING VARIABLES
# First, scale the data 
omitted.trout <- na.omit(Trout_2)
pca.scale.pitch <- scale(na.omit(Trout_2[,c('start_speed','x0','z0','spin_dir','spin_rate','break_angle','break_length','pfx_x','pfx_z','px','pz',
                              'hc_x','hc_y','vx0','vy0','vz0','ax','ay','az','sz_top','sz_bot','effective_speed','release_spin_rate',
                              'release_extension')]))
# omitted 5 NA values....
nrow(pca.scale.pitch)

pca.pitch <- princomp(pca.scale.pitch)
pca.pitch
pca.pitch$loadings
summary(pca.pitch)
pca.trout$scores

plot(pca.trout$scores,xlim=c(-4,6),ylim=c(-15,15))
points(0,0,pch="+",cex=1.5)
title("Plot of PC Scores")

plot(pca.pitch,type="line")

pca.pitch.scores.data <- data.frame(pca.pitch$scores)
pca.pitching.plot.data <- data.frame(omitted.trout,pca.pitch.scores.data)

pca.pitching.plot.data$pitch_fast_slow <- ifelse(omitted.trout$pitch_type=="FA","Fastball Type",
                                        ifelse(omitted.trout$pitch_type=="FF","Fastball Type",
                                               ifelse(omitted.trout$pitch_type=="FT","Fastball Type",
                                                      ifelse(omitted.trout$pitch_type=="FC","Fastball Type",
                                                             ifelse(omitted.trout$pitch_type=="FS","Fastball Type",
                                                                    ifelse(omitted.trout$pitch_type=="SI","Fastball Type",
                                                                           ifelse(omitted.trout$pitch_type=="SF","Fastball Type","Off-Speed Type")))))))


qplot(Comp.1,Comp.2,data=pca.pitching.plot.data,color=pitch_type) + theme_bw() + ggtitle('')
qplot(Comp.1,Comp.2,data=pca.pitching.plot.data,color=pitch_fast_slow) + theme_bw()

# Use less variables
pca.spinning <- scale(na.omit(Trout_2[,c('spin_dir','spin_rate','break_angle','break_length','effective_speed','release_spin_rate')]))
pca.spin <- princomp(pca.spinning)
pca.spin$loadings




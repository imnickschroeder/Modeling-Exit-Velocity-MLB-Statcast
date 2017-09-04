# RUN THIS TO GET CLEAN TROUT DATA IN YOUR ENVIRONMENT

# Read data in
#Trout <- read.csv("~/Capstone/Data/545361_data.csv")
Trout <- read.csv("~/Capstone/Data_Final/545361_data.csv")

library(dplyr)
Trout_1 <- filter(Trout,hit_distance_sc != "null")

Trout_2 <- Trout_1[,c("pitch_type","start_speed","x0","z0","spin_dir","spin_rate","break_angle","break_length",
                      "zone","p_throws","balls","strikes","pfx_x","pfx_z","px","pz",
                      "hc_x","hc_y","vx0","vy0","vz0","ax","ay",
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

# Check out Y variable
#hist(Trout_2$hit_speed,breaks=80)
#hist(log(Trout_2$hit_speed),breaks=80) # do we need a more normal distribution?

names(Trout_2)
dim(Trout_2)
sum(is.na(Trout_2$hit_speed))

Trout_2 <- na.omit(Trout_2)
dim(Trout_2)

sum(is.na(Trout_2))

Trout_model <- subset(Trout_2,select=-c(hit_distance_sc,hit_angle,hc_y,hc_x,sz_top,sz_bot))
#Trout_model$count <- as.factor(paste(Trout_model$balls,Trout_model$strikes,sep="-"))
#Trout_model <- subset(Trout_model,select=-c(balls,strikes))
Trout_model$strikes <- as.numeric(Trout_model$strikes)
Trout_model$balls <- as.numeric(Trout_model$balls)


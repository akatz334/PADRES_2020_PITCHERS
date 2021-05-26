library(dplyr)
setwd("~/Documents/BASEBALL/STATCAST")
statcast_data<- read.csv("mlb_2020_statcast_pitcher.csv")

##SPIN RATE LAST YEAR COMPARED TO THIS?

SDP<- statcast_data%>%
  filter(player_name %in%c("Dinelson Lamet", "Zach Davies", "Chris Paddack","Mike Clevinger", "Garrett Richards"))

Lamet<- statcast_data%>%
  filter(player_name == "Dinelson Lamet")

Davies<- statcast_data%>%
  filter(player_name == "Zach Davies")

Paddack<- statcast_data%>%
  filter(player_name == "Chris Paddack")

Clev<- statcast_data%>%
  filter(player_name == "Mike Clevinger")

GR<- statcast_data%>%
  filter(player_name == "Garrett Richards")

SDP_pitch_info<- SDP%>%
  select(player_name, pitch_type, release_speed, release_spin_rate)
 
  
SDP_release_speed<- SDP%>%
  select(player_name, pitch_type, release_speed, release_spin_rate)%>%
  arrange(desc(release_speed))

SDP_FF<- SDP%>%
  select(player_name, pitch_type, release_speed, release_spin_rate)%>%
  filter(pitch_type=="FF")%>%
  arrange(desc(release_speed))


SDP_pitch_count<- SDP%>%
  group_by(player_name, pitch_type)%>%
  summarise('pitch_count'=n())


SDP_avg_velos<- SDP%>%
  group_by(player_name, pitch_type)%>%
  summarise('pitch_count'=n(),
            'min_velocity' = min(release_speed, na.rm = TRUE),
            'max_velocity' = max(release_speed, na.rm = TRUE))





library(ggplot2)
library(ggpubr)
#### Bar Chart ###
FB_over_95<- NL_CY%>%
  filter(pitch_type=='FF')%>%
  group_by(player_name)%>%
  summarise('95+' = sum(release_speed>95, na.rm = TRUE))

#Basic
ggplot(FB_over_95, aes(x=player_name, y='95+'))+
  geom_bar(stat="identity")
#Advanced
ggplot(FB_over_95, aes(x=reorder(player_name, -'95+'), 
                       y='95+', fill = player_name)) + 
  geom_bar(stat = "identity") + 
  labs(x="Pitcher", y= "", title = "Number of fastballs over 95 MPH") +
  theme_bw() +
  theme(legend.position = "none")

#### Density Plot ####

#Dataset
Fastballs_SDP<- SDP%>%
  filter(player_name == 'Dinelson Lamet', pitch_type=="FF")


#Visual
ggplot(Fastballs_SDP, aes(x= release_speed)) +
  geom_density(color = "red", fill = "lightblue") +
  labs(x="Velocity (MPH)", title = "Distribution of Fastball Velocity: Lamet 2020") +
  theme_bw()

#### Box Plot ####

Fastballs_NL_CY<- NL_CY%>%
  filter(pitch_type== "FF")

#Basic
ggplot(Fastballs_NL_CY, aes(x=player_name, y=release_speed)) +
  geom_boxplot()

#Advanced

ggplot(Fastballs_NL_CY, aes(x = player_name, y = release_speed, fill = player_name)) + 
  geom_boxplot(width = 0.5) + 
  labs(x = "Pitcher", y = "Velocity (MPH)", title = "Distribution of Fastball Velocity") + 
  theme_bw() +
  theme(legend.position =  'none')

#### Scatter Plot 1 ####


#Lamet
ggplot(Lamet, aes(x=release_speed, y= release_spin_rate, color=pitch_type)) + 
  geom_point(size=2, na.rm=TRUE) + 
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) + 
  labs(x= "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Lamet Velocity vs. Spin Rate") + 
  theme_bw() + 
  theme(legend.position = 'bottom')

#Davies
ggplot(Davies, aes(x=release_speed, y= release_spin_rate, color=pitch_type)) + 
  geom_point(size=2, na.rm=TRUE) + 
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) + 
  labs(x= "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Davies Velocity vs. Spin Rate") + 
  theme_bw() + 
  theme(legend.position = 'bottom')

#Paddack 

ggplot(Paddack, aes(x=release_speed, y= release_spin_rate, color=pitch_type)) + 
  geom_point(size=2, na.rm=TRUE) + 
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) + 
  labs(x= "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Paddack Velocity vs. Spin Rate") + 
  theme_bw() + 
  theme(legend.position = 'bottom')
#Clevinger
ggplot(Clev, aes(x=release_speed, y= release_spin_rate, color=pitch_type)) + 
  geom_point(size=2, na.rm=TRUE) + 
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) + 
  labs(x= "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Clevinger Velocity vs. Spin Rate") + 
  theme_bw() + 
  theme(legend.position = 'bottom')

#Richards
ggplot(GR, aes(x=release_speed, y= release_spin_rate, color=pitch_type)) + 
  geom_point(size=2, na.rm=TRUE) + 
  scale_x_continuous(limits = c(80,100), breaks = c(80,85,90,95,100)) +
  scale_y_continuous(limits = c(2000,3250), breaks = c(2000,2250,2500,2750,3000,3250)) + 
  labs(x= "Velocity (MPH)", y = "Spin Rate (RPM)", color = "Pitch Type",
       title = "Richards Velocity vs. Spin Rate") + 
  theme_bw() + 
  theme(legend.position = 'bottom')


#### Scatter Plot 2, Correlation ####

# Prepare Data
cor_df<- statcast_data%>%
  filter(pitch_type == "FF")%>%
  group_by(player_name, pitch_type)%>%
  summarise('n'=n(), 
            'mean_spin_rate' = mean(release_spin_rate, na.rm = TRUE),
            'sw_str_percentage' = sum(description %in% c("swinging_strike", "swinging_strike_blocked"))/n()) %>%
  filter(n >100)

#Visual
ggscatter(data = cor_df, x = "mean_spin_rate", y = "sw_str_percentage", 
          add = "reg.line", cor.coef = TRUE, cor.method = "pearson",
          conf.int = TRUE, add.params = list(color = "red", fill = "lightgray"),
          xlab = "Velocity (MPH)", ylab = "Swinging Strike %", 
          title = "Correlation: AVG FB Spin Rate vs. Sw. Strike %")


#### Line Plot ####

#dataset Velo
SDP$game_date<- as.Date(SDP$game_date)
time_df<- SDP %>%
  filter(pitch_type== 'FF' )%>% 
  group_by(player_name, pitch_type, game_date)%>%
  summarise('n' = n(), 
            'mean_release_speed' = mean(release_speed, na.rm = TRUE))


# Visual
ggplot(data = time_df, aes(x = game_date, y = mean_release_speed, group = player_name, color = player_name)) + 
  geom_line(size = 1) + geom_point(size = 3) +
  labs(y = "Velocity (MPH)", title = "Average FF Velocity by Game", color = "") + 
  theme_bw() +
  theme(legend.position = 'bottom', axis.title.x = element_blank())

#dataset Spin
SDP$game_date<- as.Date(SDP$game_date)
time_df<- SDP %>%
  filter(pitch_type == "FF")%>% 
  group_by(player_name, pitch_type, game_date)%>%
  summarise('n' = n(), 
            'mean_spin_rate' = mean(release_spin_rate, na.rm = TRUE))


# Visual
ggplot(data = time_df, aes(x = game_date, y = mean_spin_rate, group = player_name, color = player_name)) + 
  geom_line(size = 1) + geom_point(size = 3) +
  labs(y = "Spin Rate (RPM)", title = "Average FF Spin Rate by Game", color = "") + 
  theme_bw() +
  theme(legend.position = 'bottom', axis.title.x = element_blank())

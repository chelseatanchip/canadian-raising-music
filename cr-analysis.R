# Content belongs to Chelsea Tanchip. Please contact her
# at chelseatanchip@gmail.com if you plan on expanding
# this script or dataset.

# Install the following packages if you haven't already
library(ggplot2)
library(ggpubr) 
library(tidytext) 
library(plyr)
library(dplyr) 
library(tidyr) 
library(cowplot) 
library(magrittr)  
require(lme4)

df_songs <- read.csv("~/cr_dataset.csv")
df_songs.dd <- datadist(df_songs)
options(datadist="df_songs.dd")

df_songs$Raising=as.factor(df_songs$Raising)
df_songs$Year=as.factor(df_songs$Year) 

# Summarize each variable
summary(df_songs)

################################################ 
###### Descriptive analysis/visualization ###### 
################################################ 

# Number of songs per year
(songs_by_year<-df_songs %>% 
   dplyr::count(Singer,Year) %>% 
   ggplot(aes(as.factor(Year), n, fill=Singer)) +
   geom_bar(position="Stack",stat = "identity") +
   geom_text(aes(label = format(n, digits = 2)), 
             position = position_stack(vjust = .5))+ 
   scale_fill_manual(values = c("#a1d76a", "#e9a3c9")) +
   labs(x="Year",y="Number of songs" )+
   theme(axis.text.x = element_text(angle = 0),  
         panel.background = element_blank(), axis.line = element_line(colour = "black"),
         legend.position = "top") )

# Number of tokens per Singer/year
(token_dist<-df_songs %>% 
    dplyr::count(Singer, Year,Token) %>% 
    ggplot(aes(as.factor(Year),n, fill=Singer)) +
    geom_bar(position="Stack",stat = "identity") +
    facet_grid(~Token)+
    geom_text(aes(label = format(n, digits = 2)), 
              position = position_stack(vjust = .5))+ 
    scale_fill_manual(values = c("#a1d76a", "#e9a3c9")) +
    labs(x="Year",y="Number of tokens" )+
    theme(axis.text.x = element_text(angle = 0),  
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "top") )

# F1
Mean_F1<-ddply(df_songs,"Singer", summarise, mean_F1=mean(F1_25))
(F1_dist<-df_songs %>%  
    dplyr::group_by(Singer) %>% 
    ggplot(aes(x=F1_25, fill=Singer)) +
    labs(x="F1 (Hz)")+
    ylim(c(0,0.0045))+
    geom_density(alpha=0.3)+ 
    geom_vline(data=Mean_F1, aes(xintercept=mean_F1, color=Singer),
               linetype="dashed")+
    scale_fill_manual(values=c("#a1d76a", "#e9a3c9"))+
    scale_color_manual(values=c("#a1d76a", "#e9a3c9"))+
    theme(axis.text.x = element_text(angle = 0),  
          panel.background = element_blank(), axis.line = element_line(colour = "black"),
          legend.position = "right") )

# F2
Mean_F2<-ddply(df_songs,"Singer", summarise, mean_F2=mean(F2_25))
F2_dist<-df_songs %>%  
  dplyr::group_by(Singer) %>% 
  ggplot(aes(x=F2_25, fill=Singer)) +
  labs(x="F2 (Hz)")+
  ylim(c(0,0.0045))+
  geom_density(alpha=0.3)+
  geom_vline(data=Mean_F2, aes(xintercept=mean_F2, color=Singer),
             linetype="dashed")+
  scale_fill_manual(values=c("#a1d76a", "#e9a3c9"))+
  scale_color_manual(values=c("#a1d76a", "#e9a3c9"))+
  theme(axis.text.x = element_text(angle = 0),  
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "top") 

# Duration
Mean_Duration<-ddply(df_songs,"Singer", summarise, mean_dur=mean(Duration))
Dur_dist<-df_songs %>%  
  dplyr::group_by(Singer) %>% 
  ggplot(aes(x=Duration, fill=Singer)) +
  labs(x="Duration (sec)")+
  geom_density(alpha=0.3)+
  geom_vline(data=Mean_Duration, aes(xintercept=mean_dur, color=Singer),
             linetype="dashed")+
  scale_fill_manual(values=c("#a1d76a", "#e9a3c9"))+
  scale_color_manual(values=c("#a1d76a", "#e9a3c9"))+
  theme(axis.text.x = element_text(angle = 0),  
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") 


# Intensity
Mean_Intensity<-ddply(df_songs,"Singer", summarise, mean_int=mean(Intensity))
Int_dist<-df_songs %>%  
  dplyr::group_by(Singer) %>% 
  ggplot(aes(x=Intensity, fill=Singer)) +
  geom_density(alpha=0.3)+
  labs(x="Pitch (Hz)")+
  geom_vline(data=Mean_Intensity, aes(xintercept=mean_int, color=Singer),
             linetype="dashed")+
  scale_fill_manual(values=c("#a1d76a", "#e9a3c9"))+
  scale_color_manual(values=c("#a1d76a", "#e9a3c9"))+
  theme(axis.text.x = element_text(angle = 0),  
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") 


# Pitch
Mean_Pitch<-ddply(df_songs,"Singer", summarise, mean_pitch=mean(Pitch))
Pitch_dist<-df_songs %>%  
  dplyr::group_by(Singer) %>% 
  ggplot(aes(x=Pitch, fill=Singer)) +
  geom_density(alpha=0.3)+
  geom_vline(data=Mean_Pitch, aes(xintercept=mean_pitch, color=Singer),
             linetype="dashed")+
  labs(x="Intensity (dB)")+
  scale_fill_manual(values=c("#a1d76a", "#e9a3c9"))+
  scale_color_manual(values=c("#a1d76a", "#e9a3c9"))+
  theme(axis.text.x = element_text(angle = 0),  
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none") 


# Coda 
Coda_dist<-df_songs %>%  
  dplyr::count(Singer, Coda_Type) %>% 
  ggplot(aes(x=Coda_Type, y=n,fill=Singer)) + 
  geom_bar(position="Stack",stat = "identity") + 
  geom_text(aes(label = format(n, digits = 2)), 
            position = position_stack(vjust = .5))+ 
  scale_fill_manual(values = c("#a1d76a", "#e9a3c9")) +
  labs(x="Coda type",y="Number of tokens" )+
  theme(axis.text.x = element_text(angle = 0), 
        legend.position = "none",
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  ) 

# Preceding
Preceding<-df_songs %>%  
  dplyr::count(Singer, Preceding_Segment) %>% 
  ggplot(aes(x=Preceding_Segment, y=n,fill=Singer)) + 
  geom_bar(position="Stack",stat = "identity") + 
  geom_text(aes(label = format(n, digits = 2)), 
            position = position_stack(vjust = .5))+ 
  scale_fill_manual(values = c("#a1d76a", "#e9a3c9")) +
  labs(x="Preceding segment",y="Number of tokens" )+
  theme(axis.text.x = element_text(angle = 0), 
        legend.position = "none",
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )  

# Plot variables together
 
plot_grid(songs_by_year,token_dist,ncol=2)
 
plot_grid(F1_dist,F2_dist)
 
plot_grid(Dur_dist,Pitch_dist,Int_dist)

plot_grid(Preceding,Coda_dist)

################################
##### Statistical modeling #####
################################

# Estimated models

fit.raise<-lme(F1_25~Raising+Singer+Year+Token+Duration+Intensity+
                 Pitch+Preceding_Segment+Coda_Type,
               random=list(Word=~1), data = df_songs, na.action=na.omit)

fit.raise2<-lme(F1_25~Raising+Singer+Year+Token+Duration+Intensity+
                  Pitch+Preceding_Segment+Coda_Type,
                random=list(Word=~1), data = df_songs, na.action=na.omit)

anova(fit.raise)
summary(fit.raise)
anova(fit.raise2)
summary(fit.raise2) 


AL_Data<-df_songs %>% 
  filter(Singer=='Avril')

df_songs$Raising<-as.factor(df_songs$Raising)
df_songs$Raising<-revalue(df_songs$Raising, c("0"="Non-CR", "1"="CR"))

# Overall F1/F2 boxplot

T1<-ggplot(AL_Data, aes(x = as.factor(Token), y = F1_25, fill = as.factor(Raising)))+ 
  geom_boxplot() + 
  labs(x="Token",y="F1 (Hz)",fill="Raising context") + 
  geom_point(position=position_jitterdodge(),alpha=0.2) + 
  geom_smooth(method = "lm",alpha=0.3)+
  theme_classic() +
  scale_fill_manual(values=c("#ef8a62", "#67a9cf")) + 
  theme(axis.text.x = element_text(angle = 0), 
        legend.position = "top",
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  ) 


T2<-ggplot(AL_Data, aes(x = as.factor(Token), y = F2_25, fill = as.factor(Raising)))+ 
  geom_boxplot() + 
  labs(x="Token",y="F2 (Hz)",fill="Raising context") + 
  geom_point(position=position_jitterdodge(),alpha=0.2) +
  scale_fill_manual(values=c("#ef8a62", "#67a9cf")) + 
  geom_smooth(method = "lm",alpha=0.3)+
  theme(axis.text.x = element_text(angle = 0), 
        legend.position = "top",
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

plot_grid(T1,T2,ncol=2) 

# F1 and F2 over time

T3<-ggline(AL_Data, x = "Year", y = "F1_25", 
           ylab="F1 (Hz)", 
           add = c("mean_se", "jitter"),
           facet.by = "Token",
           color = "Raising", alpha = 0.2, palette =c("#ef8a62", "#67a9cf"),
           add.params = list(size = 2, alpha = 0.2))+
  labs(y="F1 (Hz)",color="Raising context")+ 
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


T4<-ggline(AL_Data, x = "Year", y = "F2_25", 
           ylab="F2 (Hz)",
           add = c("mean_se", "jitter"),
           facet.by = "Token",
           color = "Raising",  palette =c("#ef8a62", "#67a9cf"),
           add.params = list(size = 2, alpha = 0.2))+
  labs(y="F2 (Hz)",color="Raising context")+ 
  #theme(legend.position = "top")+
  theme(axis.text.x = element_text(angle = 30, vjust = 0.9, hjust=1), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


plot_grid(T3,T4,ncol=2)   


##### Comparison to American singer ##### 

(F1_comparison<-ggline(data=df_songs, x = "Year", y = "F1_25", 
                      ylab="F1 (Hz)",alpha=0.2,
                      add = c("mean_se", "jitter"),
                      color = "Singer",
                      facet.by = list("Token","Raising"),
                      palette =c("#a1d76a", "#e9a3c9"), 
                      add.params = list(size = 2, alpha = 0.2))) 

(F2_comparison<-ggline(data=df_songs, x = "Year", y = "F2_25", 
                      ylab="F2 (Hz)",alpha=0.2,
                      add = c("mean_se", "jitter"),
                      color = "Singer",
                      facet.by = list("Token","Raising"),
                      palette =c("#a1d76a", "#e9a3c9"),  
                      add.params = list(size = 2, alpha = 0.2)))
 
 

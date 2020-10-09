#Libraries
library(dplyr)
library(ggplot2)
#library(tidyverse)
library(scales) 

#Read data
data <- read.csv("your_directory\\COEVO_Hom_OTA_WR_F.csv", header=TRUE, sep=',')

#Cleaning and organising variables
data<-data[!(data$ps.Content.bias==0.4),]
data<-data[!(data$ps.Content.bias==0.6),]
data<-data[!(data$ps.Confirmation.bias==0.2),]
data<-data[!(data$ps.Confirmation.bias==0.8),]

names(data)[names(data) == 'ps.Generation'] <- 'Round'
names(data)[names(data) == 'ps.Inst_power'] <- 'Inst_power'
names(data)[names(data) == 'ps.Entropy_population'] <- 'Entropy_population'
names(data)[names(data) == 'ps.Content.bias'] <- 'Content_bias'
names(data)[names(data) == 'ps.Conformity.bias'] <- 'Conformity_bias'
names(data)[names(data) == 'ps.Confirmation.bias'] <- 'Confirmation_bias'
names(data)[names(data) == 'ps.Simpson_e_population'] <- 'Simpson_e_population'
names(data)[names(data) == 'ps.Simpson_population'] <- 'Simpson_population'
names(data)[names(data) == 'ps.Richness'] <- 'Richness'
names(data)[names(data) == 'ps.Entropy_Institution'] <- 'Entropy_Institution'
#data <- subset(data,Content_bias==0 | Content_bias==0.2 | Content_bias==0.4 | Content_bias==0.6 | Content_bias==0.8 | Content_bias==1)

##########PLOTS##################
########## Note that you may have to comment or uncomment the lines of code that you need, depending on what you are going to plot (i.e. entropy, simpson's D, etc...)

#mytitle1 <- expression('(H'[n]*') of the set of produced variant tokens at each round (OTA)')
mytitle1 <- expression("Simpson's D")
#y <- data$Entropy_population
#y <- data$Entropy_Institution
y <- data$Simpson_e_population
Inst_power <- as.factor(data$Inst_power)
g <- data$Round
Conformity_bias <- as.factor(data$Conformity_bias)
Content_bias <- as.factor(data$Content_bias)
Confirmation_bias <- as.factor(data$Confirmation_bias)
#z = factor(z, levels = c(1, 0))
data <- data.frame(y,Inst_power,g,Content_bias, Conformity_bias, Confirmation_bias)

data %>%
  group_by(g, Inst_power,Confirmation_bias, Content_bias, Conformity_bias) %>%
  summarise(media = mean(y), 
            desvio = sd(y),                             #Estimaci?n de la media
            error_est = desvio / sqrt(n()),             #Error estandar de la estimaci?n de la media. 
            intervalo_sup = media + (2*error_est),      #Techo del intervalo. 
            intervalo_inf = media - (2*error_est)) %>%  #Piso del intervalo al 95%.
  ggplot(aes(x = g, y = media/max(y), color = Content_bias)) +
  labs(title=mytitle1) +
  #geom_point() +                                        #Para que genere una salida gr?fica cuando s?lo hay un data point.
  geom_line(aes(group =Content_bias), size=0.4) +                       #Las l?neas que unen los puntos de cada grupos xz
  #geom_errorbar(aes(ymax = intervalo_sup,               #Intervalor al 95% para cada punto. 
  #ymin = intervalo_inf),
  #width=0.3) + 
  #theme_minimal() +
  #labs(x = "Round", y = expression('Normalized entropy '*' (H'[n]*')'), color = "Content bias") +
  labs(x = "Round", y = expression("Simpson's D"), color = "Content bias") +
  # scale_color_manual(labels = c("Homogeneity OTA", "Homogeneity C", "Homogeneity PR", "Heterogeneity OTA", "Heterogeneity C", "Heterogeneity PR"), values = c("red","coral", "blue", "cyan", "green", "darkolivegreen1")) +
  #scale_color_brewer(type='seq', palette=2)+
  theme_bw() +
  # scale_fill_brewer(palette="Spectral") +
  scale_x_continuous(breaks = seq(0,100, by=50)) +
  scale_y_continuous(breaks = seq(0,1, by=0.5), limits=c(0,1)) +
  theme(legend.position="bottom", legend.text=element_text(size=12)) +
  guides(color=guide_legend(nrow=1)) +
  theme(axis.text=element_text(size=13),
        axis.title=element_text(size=13))->p

facet1_names <- list(
  "Confirmation_bias:0"=expression(~gamma~ ":0"),
  #"Confirmation_bias:0.2"=expression(~gamma~ ":0.2"),
  "Confirmation_bias:0.5"=expression(~gamma~ ":0.5"),
  #"Confirmation_bias:0.8"=expression(~gamma~":0.8"),
  "Confirmation_bias:1"=expression(~gamma ~":1")
)
facet2_names <- list(
  "Inst_power:0"=expression(~epsilon~ ":0"),
  "Inst_power:0.5"=expression(~epsilon~ ":0.5"),
  "Inst_power:1"=expression(~epsilon~ ":1")
)
facet3_names <- list(
  "Conformity_bias:0"=expression(~kappa~ ":0"),
  "Conformity_bias:0.5"=expression(~kappa~ ":0.5"),
  "Conformity_bias:1"=expression(~kappa~ ":1")
)
pl_labeller <- function(variable,value){
  if (variable=='Confirmation_bias') {
    return(facet1_names[value])
  }
  if (variable=='Inst_power') {
    return(facet2_names[value])
  } 
  if (variable=='Conformity_bias') {
    return(facet3_names[value])
  } 
  else {
    return(as.character(value))
  }
}
p+facet_grid(Inst_power+Conformity_bias ~ Confirmation_bias, labeller = pl_labeller)
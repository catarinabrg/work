library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
#data preparation
rawfile <- read_xlsx("C:/Users/catar/Desktop/Catarina/2. PACTA/data_exposure_exec_summary_v2.xlsx")
rawfile$plan_carsten <- as.numeric(as.character(rawfile$plan_carsten))

df <- rawfile %>%
  mutate(pct = plan_carsten*100) %>%
  mutate(emission = replace(emission, emission == "High Emissions Level", "High Carbon exposure")) %>%
  mutate(emission = replace(emission, emission == "Low Emissions Level", "Low Carbon exposure")) %>%
  mutate(emission = replace(emission, emission == "Total Emissions", "Total sector exposure")) 
  
#
df$portfolio_name <- factor(df$portfolio_name,levels = c("Portfolio","Peers"))
df$emission <- factor(df$emission,levels = c("Low Carbon exposure","High Carbon exposure", "Total sector exposure"))


#OPTION 1
ggplot(df, aes(x=portfolio_name, y=pct))+
  geom_col(aes(fill=emission), color = "black")+
  geom_hline(yintercept = 0, color = "dark gray")+
  facet_grid(asset_class ~ ald_sector)+
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(legend.text = element_text(colour="black", size = 7))+
  theme(legend.position = "bottom")+
  guides(fill=guide_legend(title=NULL))+
  scale_fill_manual(values = c("#00c082","#ff9623","#8597a6"))+
  ylab("Sector exposure")+
  theme(axis.title.x = element_blank())+
  theme(axis.title.y = element_text(size = 7))+
  theme(strip.text = element_text(size = 7))
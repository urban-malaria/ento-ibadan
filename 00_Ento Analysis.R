rm(list=ls())
#memory.limit(size = 50000)
## -----------------------------------------
### Paths
## -----------------------------------------
user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  NuCDir <- file.path(Drive, "my_stuff")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}



##----------------------------------------------------------------------------
#Libraries--------------------------------------------------------------------
##----------------------------------------------------------------------------
library(readxl)
library(sf)
library(vcd)
library(ggplot2)
library(tmap)
library(ggrepel)
library(tidyverse)
library(geometry)
library(dplyr)
library(rgdal)
library(fun)
library(patchwork)

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

con_gplot <-function(df,fill,label){
  ggplot()+
    geom_sf(data=df, mapping=aes(fill = !!fill)) +
    map_theme() +
    geom_text_repel(
      data = df,
      aes(label = !!label, geometry = geometry),color ='black',
      stat = "sf_coordinates",
      min.segment.length = 0, size = 1.5, force = 1, max.overlaps = Inf)+
    xlab('')+
    ylab('')
}

theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"),
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

##----------------------------------------------------------------------------
#Excel files--------------------------------------------------------------------
##----------------------------------------------------------------------------
files <- list.files(path = EntoDat , pattern = ".xlsx", full.names = TRUE, recursive = F)
dat <- sapply(files, read_xlsx, simplify = F)
names(dat)
cdc <- rbind(dat[[1]], dat[[2]])


##Indoor Transmission Ibadan###
in_cdc <- cdc %>%  filter(Location == "Indoor", State == "Oyo") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = T)) %>% ungroup()
p <- ggplot(data= in_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                                           colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Ibadan)")+
  geom_line()+
   ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over six days, January 12 - Feb 4, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.3, .8),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'indoor_cdc_ibadan.png'), p, width = 8, height = 6)


##Outdoor Transmission- Ibadan
out_cdc <- cdc %>%  filter(Location == "Outdoor", State == "Oyo") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% ungroup()
p <- ggplot(data= out_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Ibadan)")+
  geom_line()+
  ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over six days, January 12 - February 4, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.3, .8),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  )

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'outdoor_cdc_ibadan.png'), p, width = 8, height = 6)



## read ibadan ward shape files
df_ib = st_read(file.path(shapepath, "ibadan_metro_ward_fiveLGAs", "Ibadan_metro_fiveLGAs.shp")) %>%
  mutate(WardName = ifelse(WardName == 'Oranyan' & LGACode == '31007', 'Oranyan_7', WardName))

p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = filter(cdc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 5.0) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-Feb, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_ibadan.png'), p, width = 8, height = 6)



##----------------------------------------------------------------------------
#Kano--------------------------------------------------------------------
##----------------------------------------------------------------------------

##Indoor Transmission Ibadan###
in_cdc <- cdc %>%  filter(Location == "Indoor", State == "Kano") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`, na.rm = T)) %>% ungroup()
p <- ggplot(data= in_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Kano)")+
  geom_line()+
  ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over six days, January 12 - February 7, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0, 2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'indoor_cdc_kano.png'), p, width = 8, height = 6)


##Outdoor Transmission- Ibadan
table(out_cdc$Day, out_cdc$Month)
out_cdc <- cdc %>%  filter(Location == "Outdoor", State == "Kano") %>%  group_by(`Settlement Classification`, `Time of Collection`) %>% 
  summarise(total_mosquitoes = sum(`Total Anopheles`)) %>% ungroup()
p <- ggplot(data= out_cdc, aes(x= `Time of Collection`, y=total_mosquitoes, group = `Settlement Classification`,
                               colour = `Settlement Classification`))+
  scale_x_discrete(limits = c("6-7pm", "7-8pm", "8-9pm", "9-10pm", "10-11pm", "11-12am", "12-1am", "1-2am", "2-3am", "3-4am", "5-6am"))+
  geom_point() +labs(y= "Total number of anopheles mosquitos caught/hr.", x = "Time of collection (Kano)")+
  geom_line()+
  ggtitle("Hourly indoor biting of anopheles mosquito aggregated \n over six days, January 12 - February 7, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'outdoor_cdc_kano.png'), p, width = 8, height = 6)



## read kano ward shape files
df_ko = st_read(file.path(shapepath, "Kano_metro_ward_fiveLGAs", "Kano_metro_ward_fiveLGAs.shp")) 

p <- ggplot(df_ko) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = filter(cdc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 5.0) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Kano showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-Feb, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_cdc_kano.png'), p, width = 8, height = 6)



##PSC - Ibadan
names(dat)
psc <- rbind(dat[[4]], dat[[5]])
names(psc)
table(psc$`An. Gambiae`)
table(psc$An.Funestus)

psc_ib <- psc %>%  filter(State == "Oyo") %>%  group_by(`Settlement Classification`, Month) %>% 
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = T)) %>% ungroup()


p <- ggplot(data=psc_ib , aes(x= Month, y=total_mosquitoes, group = `Settlement Classification`,
                               colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("January", "February")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  geom_line()+
  ggtitle("Anopheles mosquito collected through Pyrethrum Spray Catches, \n January 13 and February 3, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_psc_ibadan.png'), p, width = 8, height = 6)

#locations data collection 
p <- ggplot(df_ib) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = filter(psc, State=="Oyo"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 5.0) +
  geom_text_repel(
    data = df_ib,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-Feb, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_psc_ibadan.png'), p, width = 8, height = 6)




##PSC - Kano
table(psc_ko$Day, psc_ko$Month)
psc_ko <- psc %>%  filter(State == "Kano") %>%  group_by(`Settlement Classification`, Month) %>% 
  summarise(total_mosquitoes = sum(`An. Gambiae`, na.rm = T)) %>% ungroup()


p <- ggplot(data=psc_ko, aes(x= Month, y=total_mosquitoes, group = `Settlement Classification`,
                              colour = `Settlement Classification`))+
  scale_x_discrete(limits=c("January", "February")) +
  geom_point() +labs(y= "Total number of anopheles mosquitos caught", x = "Month of Collection (Ibadan)")+
  geom_line()+
  ggtitle("Anopheles mosquito collected through Pyrethrum Spray Catches, January 13 and February 5, 2023")+geom_point(size = 3.0) +
  theme(plot.title = element_text(size = 12))+
  theme_manuscript() +
  theme(
    legend.position = c(.95, .95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6)
  ) +
  ylim(0,2)

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'mosquitoes_collected_psc_kano.png'), p, width = 8, height = 6)

#locations data collection 
p <- ggplot(df_ko) +
  geom_sf(fill = NA) +
  #geom_sf_text(data = df, aes(label = WardName), colour = "black")+
  geom_point(data = filter(psc, State=="Kano"), mapping = aes(x = Longitude, y = Latitude), colour = "red", size = 5.0) +
  geom_text_repel(
    data = df_ko,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme()+ 
  labs(title= "Wards in Ibadan showing households visited for indoor \n and outdoor mosquito collection using CDC light trap, Jan-Feb, 2023")+
  coord_sf()

ggsave(paste0(ResultDir,"/", Sys.Date(), "/", 'locations_psc_kano.png'), p, width = 8, height = 6)





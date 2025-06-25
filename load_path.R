## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

user <- Sys.getenv("USER")
if ("ifeomaozodiegwu" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "Library", "CloudStorage", "OneDrive-NorthwesternUniversity", "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
} else if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  NuDir <- file.path(Drive, "urban_malaria")
  EntoDat <- file.path(NuDir, "data", "nigeria", "kano_ibadan", "kano_ibadan_ento", "Osun-excel")
  WetData <- file.path(NuDir, "data", "nigeria", "kano_ibadan", "kano_ibadan_ento", "Wet Season Data_Ibadan")
  ResultDir <- file.path(NuDir, "projects/Manuscripts/ongoing/dry season entomology_manuscript/Grace/figures/plots")
  NewFigDir <- file.path(NuDir, "projects/Manuscripts/ongoing/dry season entomology_manuscript/Grace/updated/figures/pdf")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan/kano_ibadan_shape_files")
} else {
  user <- Sys.getenv("USERNAME")
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  NuDir <- file.path(Drive, "urban_malaria")
  shapepath <- file.path(NuDir,"/data/nigeria/kano_ibadan_shape_files")
  NuCDir <- file.path(Drive, "my_stuff")
  ProjectDir <- file.path(NuDir, "data", 'nigeria','nigeria_dhs' , 'data_analysis')
  EntoDat <- file.path(NuDir, "data", "nigeria",  "kano_ibadan", "kano_ibadan_ento", "Osun-excel")
  ResultDir <-file.path(NuDir, "projects/project_implementation/analysis_output/ento_plots")
  DataDir <- file.path(ProjectDir, 'data', 'DHS', 'Downloads')
}


## =========================================================================================================================================
### Required Libraries and Functions
## =========================================================================================================================================

# load necessary libraries
library(readxl)
library(sf)
#library(vcd)
library(ggplot2)
#library(tmap)
library(ggrepel)
library(tidyverse)
#library(geometry)
library(dplyr)
#library(rgdal)
library(fun)
library(patchwork)
#library(rgeos)
#library(maptools)
library(purrr)
#library(DescTools)
library(conflicted)
library(stringr)
library(readxl)
library(dplyr)
library(gridExtra)
library(sf)
library(ggrepel)
library(car)
library(officer)
library(openxlsx)
library(ggforce)
library(scatterpie)
library(ggnewscale)
library(writexl)

# define a custom theme for maps
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

# function to create a ggplot object for geographic data
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

# define a custom theme for manuscript-style plots
theme_manuscript <- function(){
  theme_bw() +
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"),
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}


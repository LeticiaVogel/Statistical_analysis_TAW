#### Box plot of average TAW across different biomes, land cover types, and land uses

# Library
library(ggstatsplot)
library(palmerpenguins)
library(tidyverse)
library(remotes)

# Set Directory
setwd("Directory")
list.files()
df <- read.csv("data_file.csv", sep = ",", header = TRUE)
str(df)
library(ggplot2)

# Check the name of LU Classes
unique(df$LU_Class)

# Cores personalizadas para LULC
#Caatinga ######################################################
# Change the name of LU Classes
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",               
                           "Savanna_Formation" = "SF",             
                           "Mangrove" = "MANG",                       
                           "Forest_Plantation" = "FP",             
                           "Grassland" = "GRASS",                      
                           "Pasture" = "PAS",                        
                           "Sugar_cane" = "SC",                     
                           "Mosaic_of_Uses" = "MSC",                
                           "Non_vegetated_area" = "NV",             
                           "Rocky_Outcrop" = "RO",                 
                           "Water" = "WAT",                          
                           "Hypersaline_Tidal_Flat" = "HTF",        
                           "Soybean" = "SOY",                        
                           "Other_Temporary_Crops" = "OTC",
                           "Coffee" = "COF",                         
                           "Other_Perennial_Crops" = "OPC",         
                           "Herbaceous_Sandbank_Vegetation" = "HSV"))
# Change Colors
custom_colors_Caatinga <- c("FF" = "#1f8d49",               
                                   "SF" = "#7dc975",             
                                   "MANG" = "#04381d",                       
                                   "FP" = "#7a5900",             
                                   "GRASS" = "#d6bc74",                      
                                   "PAS" = "#edde8e",                        
                                   "SC" = "#db7093",                     
                                   "MSC" = "#ffefc3",                
                                   "NV" = "#d4271e",             
                                   "RO" = "#ffaa5f",                 
                                   "WAT" = "#2532e4",                          
                                   "HTF" = "#fc8114",        
                                   "SOY" = "#f5b3c8",                        
                                   "OTC" = "#f54ca9",
                                   "COF" = "#d68fe2",                         
                                   "OPC" = "#e6ccff",         
                                   "HSV" = "#ad5100")
#Amazon Forest #####################################################
# Change the name of LU Classes
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",
                           "Savanna_Formation" = "SF",
                           "Mangrove" = "MANG",
                           "Floodable_Forest" = "FLOD",
                           "Forest_Plantation" = "FP",
                           "Wetland" = "WET",
                           "Grassland" = "GRASS",
                           "Pasture" = "PAS",
                           "Sugar_cane" = "SC",
                           "Non_vegetated_area" = "NV",
                           "Rocky_Outcrop" = "RO",
                           "Water" = "WAT",
                           "Hypersaline_Tidal_Flat" = "HTF",
                           "Palm_Oil" = "PO",
                           "Soybean" = "SOY",
                           "Other_Temporary_Crops" = "OTC",
                           "Cotton_beta" = "COT"
  ))
# Change Colors
custom_colors_Amazon <- c("FF" = "#1f8d49",
                                   "SF" = "#7dc975",
                                   "MANG" = "#04381d",
                                   "FLOD" = "#007785",
                                   "FP" = "#7a5900",
                                   "WET" = "#519799",
                                   "GRASS" = "#d6bc74",
                                   "PAS" = "#edde8e",
                                   "SC" = "#db7093",
                                   "NV" = "#d4271e",
                                   "RO" = "#ffaa5f",
                                   "WAT" = "#2532e4",
                                   "HTF" = "#fc8114",
                                   "PO" = "#9065d0",
                                   "SOY" = "#f5b3c8",
                                   "OTC" = "#f54ca9",
                                   "COT" = "#ff69b4"
) 


#Cerrado #####################################################
# Change the name of LU Classes
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",
                           "Savanna_Formation" = "SF",
                           "Mangrove" = "MANG",
                           "Forest_Plantation" = "FP",
                           "Wetland" = "WET",
                           "Grassland" = "GRASS",
                           "Pasture" = "PAS",
                           "Sugar_cane" = "SC",
                           "Mosaic_of_Uses" = "MSC",
                           "Non_vegetated_area" = "NV",
                           "Rocky_Outcrop" = "RO",
                           "Water" = "WAT",
                           "Soybean" = "SOY",
                           "Rice" = "RIC",
                           "Other_Temporary_Crops" = "OTC",
                           "Coffee" = "COF",
                           "Citrus" = "CIT",
                           "Other_Perennial_Crops" = "OPC",
                           "Cotton_beta" = "COT"
  ))
# Change Colors
custom_colors_Cerrado <- c("FF" = "#1f8d49",
                                   "SF" = "#7dc975",
                                   "MANG" = "#04381d",
                                   "FP" = "#7a5900",
                                   "WET" = "#519799",
                                   "GRASS" = "#d6bc74",
                                   "PAS" = "#edde8e",
                                   "SC" = "#db7093",
                                   "MSC" = "#ffefc3",
                                   "NV" = "#d4271e",
                                   "RO" = "#ffaa5f",
                                   "WAT" = "#2532e4",
                                   "SOY" = "#f5b3c8",
                                   "RIC" = "#c71585",
                                   "OTC" = "#f54ca9",
                                   "COF" = "#d68fe2",
                                   "CIT" = "#9932cc",
                                   "OPC" = "#e6ccff",
                                   "COT" = "#ff69b4") 
#Atlantic Forest #####################################################
# Change the name of LU Classes
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",
                           "Savanna_Formation" = "SF",
                           "Mangrove" = "MANG",
                           "Forest_Plantation" = "FP",
                           "Wetland" = "WET",
                           "Grassland" = "GRASS",
                           "Pasture" = "PAS",
                           "Sugar_cane" = "SC",
                           "Mosaic_of_Uses" = "MSC",
                           "Non_vegetated_area" = "NV",
                           "Rocky_Outcrop" = "RO",
                           "Water" = "WAT",
                           "Hypersaline_Tidal_Flat" = "HTF",
                           "Soybean" = "SOY",
                           "Rice" = "RIC",
                           "Other_Temporary_Crops" = "OTC",
                           "Coffee" = "COF",
                           "Other_Perennial_Crops" = "OPC",
                           "Wooded_Sandbank _Vegetation" = "WSV",
                           "Herbaceous_Sandbank_Vegetation" = "HSV"
  ))
# Change Colors
custom_colors_Atlantic <- c( "FF" = "#1f8d49",
                                    "SF" = "#7dc975",
                                    "MANG" = "#04381d",
                                    "FP" = "#7a5900",
                                    "WET" = "#519799",
                                    "GRASS" = "#d6bc74",
                                    "PAS" = "#edde8e",
                                    "SC" = "#db7093",
                                    "MSC" = "#ffefc3",
                                    "NV" = "#d4271e",
                                    "RO" = "#ffaa5f",
                                    "WAT" = "#2532e4",
                                    "HTF" = "#fc8114",
                                    "SOY" = "#f5b3c8",
                                    "RIC" = "#c71585",
                                    "OTC" = "#f54ca9",
                                    "COF" = "#d68fe2",
                                    "OPC" = "#f54ca9",
                                    "WSV" = "#02d659",
                                    "HSV" = "#ad5100") 
#Pampa #####################################################
# Change the name of LU Classes
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",
                           "Forest_Plantation" = "FP",
                           "Wetland" = "WET",
                           "Grassland" = "GRASS",
                           "Mosaic_of_Uses" = "MSC",
                           "Non_vegetated_area" = "NV",
                           "Rocky_Outcrop" = "RO",
                           "Water" = "WAT",
                           "Soybean" = "SOY",
                           "Rice" = "RIC",
                           "_Other_Temporary_Crops" = "OTC",
                           "Wooded_Sandbank_Vegetation" = "WSV",
                           "Herbaceous_Sandbank_Vegetation" = "HSV"
  ))
# Change Colors
custom_colors_Pampa <- c( "FF" = "#1f8d49",
                                    "FP" = "#7a5900",
                                    "WET" = "#519799",
                                    "GRASS" = "#d6bc74",
                                    "MSC" = "#ffefc3",
                                    "NV" = "#d4271e",
                                    "RO" = "#ffaa5f",
                                    "WAT" = "#2532e4",
                                    "SOY" = "#f5b3c8",
                                    "RIC" = "#c71585",
                                    "OTC" = "#f54ca9",
                                    "WSV" = "#02d659",
                                    "HSV" = "#ad5100") 
#Pantanal #####################################################
df <- df %>%
  mutate(LU_Class = recode(LU_Class,
                           "Forest_Formation" = "FF",
                           "Savanna_Formation" = "SF",
                           "Wetland" = "WET",
                           "Grassland" = "GRASS",
                           "Pasture" = "PAS",
                           "Urban_Area" = "UA",
                           "Non_vegetated_area" = "NV",
                           "Water" = "WAT",
                           "Other_Temporary_Crops" = "OTC"
                           ))
# Change Colors
custom_colors_Pantanal <- c("FF" = "#1f8d49",
                                   "SF" = "#7dc975",
                                   "WET" = "#519799",
                                   "GRASS" = "#d6bc74",
                                   "PAS" = "#edde8e",
                                   "UA" = "#d4271e",
                                   "NV" = "#d4271e",
                                   "WAT" = "#2532e4",
                                   "OTC" = "#f54ca9") 
######################################################

# Plot Boxplot
# Order LU_Class by decreasing median (or mean) value of 'mean'
order_classes <- df %>%
  group_by(LU_Class) %>%
  summarize(median_val = median(mean, na.rm = TRUE)) %>%
  arrange(desc(median_val)) %>%
  pull(LU_Class)

# Convert LU_Class to factor with levels ordered
df$LU_Class <- factor(df$LU_Class, levels = order_classes)

# Create boxplot
setwd("Directory")
tiff("Boxplot-Amazon.tif", width = 3000, height = 1500, res = 300)
ggplot(df, aes(x = LU_Class, y = mean, fill = LU_Class)) +
  geom_boxplot(color = "black", 
               outlier.shape = 21, 
               outlier.fill = NA,  
               outlier.color = "gray50") + 
    scale_fill_manual(values = custom_colors_Amazon) +
  theme(panel.background = element_rect(fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "black", face ="bold"), 
        axis.text.y = element_text(size = 16, color = "black"), 
        axis.title = element_text(size = 16, face = "bold"), 
        legend.title = element_text(size = 0),
        legend.text = element_text(size = 18),                         
        legend.position = "none") +
  labs(x = "Land Use Class", y = expression("TAW (mm " * m^{-1} * ")"))
dev.off()









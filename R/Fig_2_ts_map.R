#'---------------------------------------------------------
#'title: Fig2 - Time series and publication distribution
#'author: Gurdeep
#'date: 19/08/2021
#'---------------------------------------------------------
rm(list=ls()) # Clear the current environment

#############################################
#            INSTALL PACKAGES               #
#############################################
library(tidyverse)
library(ggthemes) 
library(ggplot2)
library(sf)
library(sp) # library(rgdal)
library(rworldmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(cleangeo)
library(gridExtra)
library(ggpubr)
library(mapproj)
library(RColorBrewer)
library(patchwork)  

#############################################
#                IMPORT DATA                #
#############################################
# Currently you use two different files - but it should be easy to just use the SAME file
SR <- read.csv(file = "Data/Global_distribution_fig2b.csv") # data is from Gurdeep's systematic review 19/10/20
TS <- read.csv("Data/Fig_2a_Time_series_globally.csv")

dim(TS); dim(SR)
TS2 <- SR %>% 
  group_by(Title) %>% 
  filter(row_number() == 1)
dim(TS); dim(TS2) # Check if timeseries data (unique papers listed by country maps to map data - is this also papers by country?)

# Download world map
world_map <- getMap() # plot(world_map)
world <- ne_countries(returnclass = "sf") 
world <- ne_countries(scale = "medium", returnclass = "sf") 

ggplot(data = world) +
  geom_sf() 

#############################################
#              CLEANING DATA                #
#############################################
# Find which country names do not match between data and map file
map_countries <- as.character(world_map$ADMIN)
map_countries2 <- as.character(world$admin)

systematic_countries <- unique(SR$Country)
no_match <- setdiff(systematic_countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))
no_match <- setdiff(systematic_countries, map_countries2); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))
# Fixing mis-matched countries 
SR$Country[grepl("West Indies", SR$Country)] <- "Dominican Republic" # The West Indes are NOT a country! so this has not worked - introduced Dominican republic instead

# How many entries are there for the remaining mis-matches?
table(SR$Country[which(SR$Country %in% no_match)])

# Have a quick look at the number of papers per country, and the total number of countries
country_table <- table(SR$Country); country_table
length(country_table)

#############################################
#           PROCESS THE DATA                #
#############################################
systematic_countries <- SR %>% 
  # filter(Lab_based == "y") %>%
  group_by(Country) %>%
  summarise(n=n()) 
systematic_countries # Table of each country and the number of papers associated with it

systematic_asia <- SR %>% 
  filter(Continent == "Asia") %>%
  group_by(Country) %>%
  summarise(n=n())
systematic_asia$Region <- "Asia"

systematic_africa <- SR %>% 
  filter(Continent == "Africa") %>%
  group_by(Country) %>%
  summarise(n=n()) 
systematic_africa$Region <- "Africa"

systematic_americas <- SR %>% 
  filter(Continent == "Americas") %>%
  group_by(Country) %>%
  summarise(n=n()) 
systematic_americas$Region <- "Americas"

systematic_global <- rbind(systematic_africa, systematic_asia, systematic_americas)

# Join with world map
# world_map <- clgeo_Clean(world_map) # Clean shapefile - if you run the shp_to_df function without this, you get an error (not normally required!)
# world_map_data <- merge(world_map, systematic_countries, by.x="ADMIN", by.y="Country") # Merge summary data with world map
# world_map_data[is.na(world_map_data$n)] <- 0 # did we need to use factors for countries!
# table(world_map_data$n, useNA="always") # Set number of sequences==NA as 0
# world_map_data_df <- shp_to_df(world_map_data) # Transform the spatialPolygonsDataframe to a dataframe, with coordinates stored in the df

# Join with world map
world_map <- merge(world, systematic_global, by.x="admin", by.y="Country", all = T) # Merge summary data with world map
world_map$n[is.na(world_map$n)] <- 0 
table(world_map$n, useNA="always") # Set number of sequences==NA as 0

# world_map_asia <- merge(world_map, systematic_asia, by.x="ADMIN", by.y="Country") # Merge summary data with world map
# world_map_asia[is.na(world_map_asia$n)] <- 0 # did we need to use factors for countries!
# world_map_asia_df <- shp_to_df(world_map_asia) # Transform the spatialPolygonsDataframe to a dataframe, with coordinates stored in the df
# 
# world_map_africa <- merge(world_map, systematic_africa, by.x="ADMIN", by.y="Country") # Merge summary data with world map
# world_map_africa[is.na(world_map_africa$n)] <- 0 # did we need to use factors for countries!
# world_map_africa_df <- shp_to_df(world_map_africa) # Transform the spatialPolygonsDataframe to a dataframe, with coordinates stored in the df
# 
# world_map_americas <- merge(world_map, systematic_americas, by.x="ADMIN", by.y="Country") # Merge summary data with world map
# world_map_americas[is.na(world_map_americas$n)] <- 0 # did we need to use factors for countries!
# world_map_americas_df <- shp_to_df(world_map_americas) # Transform the spatialPolygonsDataframe to a dataframe, with coordinates stored in the df
#############################################
#               MAP THE DATA                #
#############################################
br <- c(0,1,2,6,11,100)
col_length <- length(br)
summary(world_map$n)

catscale <- cut(world_map$n, breaks = br,
                  labels = c("0", "1-2", "2-6", "6-11", "11-100"), right = FALSE)
world_map$cat_scale <- as.character(catscale)
world_map$global_scale <- as.character(catscale)
world_map$global_scale[which(world_map$n == 0)] <-  0
world_map$global_scale[which(world_map$cat_scale == "1-2")] <-  1
world_map$global_scale[which(world_map$cat_scale == "2-6")] <-  5
world_map$global_scale[which(world_map$cat_scale == "6-11")] <-  10
world_map$global_scale[which(world_map$cat_scale == "11-100")] <-  50
world_map$global_scale <- as.numeric(world_map$global_scale )
df_catscale <- world_map


df_catscale$global_scale[which(df_catscale$Region == "Asia")] <-  df_catscale$global_scale[which(df_catscale$Region == "Asia")]*100
df_catscale$global_scale[which(df_catscale$Region == "Americas")] <-  df_catscale$global_scale[which(df_catscale$Region == "Americas")]*10000
df_catscale$global_scale <- as.factor(df_catscale$global_scale)

colsAs = c(colorRampPalette(brewer.pal(6,"Greens"))(col_length-1))[2:5]
colsAf = c(colorRampPalette(brewer.pal(6,"Oranges"))(col_length-1))[2:5]
colsAm = c(colorRampPalette(brewer.pal(6,"Blues"))(col_length-1))[2:5]
cols_global <- c("white", colsAf, colsAs, colsAm)
# colsAm = c("transparent", colorRampPalette(brewer.pal(9,"Blues"))(col_length))
# colsAs = c("transparent", colorRampPalette(brewer.pal(9,"Greens"))(col_length))
# colsAf = c("transparent", colorRampPalette(brewer.pal(9,"Oranges"))(col_length))

# Quick summary of the data - this will give the max number of papers - necessary for setting the breaks!


# Set the breaks appropriately and use the function to create a colour scale
# Plot the map including the data and a colour scale indicating the number of papers, and give it a title
systematic_map <-
  ggplot(data = world) +
  geom_sf(data=world, fill="white")+
  geom_sf(data=df_catscale, aes(fill = global_scale), inherit.aes = FALSE) +
  scale_fill_manual(name="Papers", values=cols_global, 
                    labels = c("0","1","2-5","6-10",">10", "1","2-5","6-10",">10", "1","2-5","6-10",">10")) +
  theme_void() +
  coord_equal() +
  geom_text(label = "B", x = 2002, y = 20) +
  # lims(x = c(-170, 180), y = c(-60,90)) +
  coord_sf(xlim = c(-110, 140), ylim = c(-52, 75), expand = T) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y=element_blank(),
        axis.title.x=element_blank()) + 
  ggtitle("B")
systematic_map


# # zoom in on a map we need to set the xy limits of the plot
# Afrasia <- bbox(world_map_data[which(world_map_data$continent=="Africa","Asia"),])# bounding box for the continent of Africa
# # replot but use the bounding box to set the xy limits
# Afrasia <- ggplot() +
#   geom_polygon(data = df_catscale, aes(x=long, y=lat, group=group, fill=cat_scale), col="black") +
#   scale_fill_manual(name="Publications with sequence data", values=cols, labels = c("0","1","2-5","6-10",">10")) +
#   theme_void() + 
#   coord_equal() +
#   coord_map(xlim = Africa[1,], ylim =  Asia[2,]) + # DOESN"T WORK BECAUSE LIMITS ARE NOT SET
#   ggtitle("B") 
# Afrasia
# 
# ggsave("Figures/Fig2b.pdf", width = 8, height = 6) # Some additional pointers here: https://stackoverflow.com/questions/40078257/setting-map-limits-in-ggplot2-with-mercator-projection
# 

#############################################
# Add a theme to make it easier to format
theme <- theme_clean() + theme(axis.title = element_text(size=10), axis.text=element_text(size=8), 
                               legend.text = element_text(size=8), legend.title=element_text(size=9), 
                               legend.background = element_rect(color = NA),
                               plot.background = element_rect(color = "white"), 
                               strip.text = element_text(size=6))

yrs = levels(factor(2000:2023)) # make years from 2000 to 2020 a factor!

TS_region <-  TS %>% 
  group_by(Continent, Year_of_publication) %>%
  summarise(n=n()) 
TS_region$Region <- TS_region$Continent

# Note that I have simplified the axes labels, reformatted the ticks and labels
publication_TS <- ggplot(TS_region, aes(x=Year_of_publication, y=n, fill = Region)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_manual(values=c(colsAf[3], colsAm[3], colsAs[3])) +
  geom_text(label = "A", x = 2000, y = 23) + 
  labs(x="Year", y="Publications") +
  scale_x_continuous(breaks=seq(2000,2023,2), labels=seq(2000,2023,2)) +
  ggtitle("A") +
  theme
publication_TS

# Save the plots
ggsave("Figures/fig2_ts_map.pdf", 
       plot = publication_TS / systematic_map + plot_layout(heights=c(1,1.8)), 
       width = 9, height = 6, units="in")

# Save the object to the right size for publication!
# ggsave(plot = publication_TS, path = "Figures", filename = "Fig2a.pdf", width = 12, height = 6, units = "cm")
# 
# # Arrange the combined plot
# fig2 <- grid.arrange(arrangeGrob(publication_TS, systematic_map, nrow=2))
# ggsave(plot = fig2, path = "Figures", filename = "Fig2_combined.pdf", width = 16, height = 16, units = "cm")




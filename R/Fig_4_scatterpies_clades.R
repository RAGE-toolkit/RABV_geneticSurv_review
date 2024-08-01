#'---------------------------------------------------------
#'title: Fig 4, clade piecharts + underlying #seq/ country
#'author: Kirstyn Brunker
#'date: 16/11/2023
#'edited: 10/05/2024
#'---------------------------------------------------------

rm(list=ls()) # This clears anything currently in the environment

#############################################
#            INSTALL PACKAGES               #
#############################################
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)
library(sf)
library(gridExtra)
library(cowplot)
library(ggpubr)
library(ggnewscale)
library(dplyr)
library(patchwork)   
library(RColorBrewer)
library(pals)

#############################################
#                IMPORT DATA                #
#############################################
# Data formatted from summarise_SR_clades.R code
# One row per country with columns for total publications, total sequences and breakdown per clade
metadata <- read.csv("Data/SR_90cov_noBat_summary_forPlot.csv")
# change name Country to country to match shapefile
names(metadata)[1]="country"
names(metadata)=gsub("."," ", names(metadata), fixed=T)
# change name of RABV clade to Unclassified
names(metadata)[grep("RABV",names(metadata))]="Unclassified"
# reorder cols to alphabetical
# indicate which columns are the clade data
clade_cols <-which(grepl("cosmo|asian|arctic|indian|africa|unclassified", colnames(metadata),ignore.case = TRUE))
continue=max(clade_cols)+1
# sort
sorted_col_names <- sort(names(metadata)[clade_cols])
# custom order
sorted_col_names2 <-sorted_col_names [c(3:18,1:2,19:35)]
# Reorder the dataframe with columns 1 to 3, the sorted columns, and then the remaining columns
metadata_reordered <- metadata[, c(names(metadata)[1:3], sorted_col_names2, names(metadata)[continue:ncol(metadata)])]

# Check the reordered dataframe
names(metadata_reordered)



#############################################
#              MAPS               #
#############################################
# base map of the world
world <- ne_countries() 

#' Find which country names do not match between data and map file
map_countries <- as.character(world$admin)
countries <- unique(metadata$country)
no_match <- setdiff(countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))

# correct the issues
metadata$country[grepl("Tanzania", metadata$country)] <- "United Republic of Tanzania"
#metadata$country[grepl("Korea", metadata$country)] <- "South Korea"
metadata$country[grepl("Lao", metadata$country)] <- "Laos"
metadata$country[grepl("Côte d'Ivoire", metadata$country)] <- "Ivory Coast"

#' How many entries are there for the remaining mis-matches?
table(metadata$country[which(metadata$country %in% no_match)])

# List all the countries included in the metadata
countries <- unique(metadata$country)

# Have a quick look at the number of sequences per country, and the total number of countries
country_table<-table(metadata$country); country_table
length(country_table)

# Replace NAs with zeros
metadata[is.na(metadata)] <- 0

#############################################
#       ADD GEO DATA         #
#############################################

# Add columns to the table ready to be filled in with the Lat and Long values for each country
metadata$LAT<-NA
metadata$LON<-NA

# Assuming metadata$country contains country names and world is an sf object with country polygons

# Iterate over each country in metadata
for (i in 1:length(metadata$country)) {
  tryCatch({
    country_polygon <- world[world$admin == metadata$country[i],]
    centroid <- st_coordinates(st_centroid(country_polygon))
    metadata$LAT[i] <- centroid[2]
    metadata$LON[i] <- centroid[1]
  }, error = function(e) {
    # Handle the error here, or just do nothing to ignore it
    # print(paste("Error occurred for country", metadata$country[i], ": ", e$message))
  })
}


# The data needs to be processed in a way that can be read into the maps

# Count the number of rows (countries) in the table
n <- nrow(metadata)
# Add a column that gives a number to each country
metadata$region <- factor(1:n)

# Add a column called "radius" to allow size of pies to correspond to number of publications
metadata$radius<-NA
#round(quantile(metadata$Number_of_Publication))
# Catergorise radii into groups to size piecharts sensibly 
metadata$radius[which(metadata$Number_of_Publication %in% 1)]<-1
metadata$radius[which(metadata$Number_of_Publication %in% 2:5)]<-2
metadata$radius[which(metadata$Number_of_Publication %in% 6:10)]<-3
metadata$radius[which(metadata$Number_of_Publication> 10)]<-4


#############################################
#              MAKE THE MAPS                #
#############################################

### WORLD MAPS

# WORLD- add centroid points
world <- ne_countries(returnclass = "sf") 
world_points<- st_point_on_surface(world)
world_points <- cbind(world, st_coordinates(st_point_on_surface(world$geometry)))

## some manual fixes to centroid points: 

metadata$LON[which(metadata$country=="Sudan")]=world_points$X[world_points$name=="Sudan"]
metadata$LAT[which(metadata$country=="Sudan")]=world_points$Y[world_points$name=="Sudan"]
metadata$LON[which(metadata$country=="Russia")]=world_points$X[world_points$name=="Russia"]
metadata$LAT[which(metadata$country=="Russia")]=55 #world_points$Y[world_points$name=="Russia"]
metadata$LON[which(metadata$country=="South Africa")]=24
world_points$X[world_points$name=="South Africa"]=metadata$LON[which(metadata$country=="South Africa")]
world_points$Y[world_points$name=="South Africa"]=metadata$LAT[which(metadata$country=="South Africa")]

# merge metadata to centroids
world2 <- merge(world_points, metadata, by.x = "admin", by.y="country")

# Plot a basic map which we will add pies to
plot_world<-
  ggplot(data = 
           world2) +
  geom_sf(data=world, fill="white")+
  geom_sf(data=world2,aes(fill = Total_Number_of_Sequences))+
  scale_fill_gradient2(low='grey96', high='grey20',guide = guide_colourbar(title.position = "top",direction = "vertical"))+
  theme(panel.grid.major = element_blank())+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
   #this bit removes the axis lables     
        axis.title.y=element_blank(),
        axis.title.x=element_blank(),
   panel.background = element_rect(fill = "aliceblue"))+ 
  labs(fill = "Number of sequences")
plot_world

## colours for lots of data
n <- 35
col_vector=polychrome(36)[-1]
pie(rep(1, n), col=col_vector)
names(col_vector) <- sorted_col_names2
col_vector[names(col_vector)=="Unclassified"]="white"


colScale <- scale_fill_manual(name = "Phylogenetic Clade (Major_Minor)",values = col_vector,label=sorted_col_names2, breaks=sorted_col_names2 )


# add country names for reference on map
name_country <- c("Brazil", "Mexico", "China", "Indonesia", "South Africa", "Kenya", "Iran", "India", "Argentina", "Russia", "Niger", "Puerto Rico", "Sri Lanka")

## add scatterpies on top of shaded map
plot_world2<-plot_world + 
  new_scale("fill")+
 geom_scatterpie(aes(x=LON, y=LAT,group=region, r=(radius+0.1)), data=metadata, cols=sorted_col_names2, fill="darkblue",color=NA, alpha=1)+
  new_scale_fill()+
  geom_scatterpie(aes(x=LON, y=LAT, group=region, r=radius), data=metadata, cols=sorted_col_names2, color=NA, alpha=1,sorted_by_radius = T)+
  colScale+
  geom_text(data=subset(world2, name %in% name_country),aes(x=X, y=Y, label=name),color = "darkblue", fontface = "bold", check_overlap = F, size=3, nudge_y = -1.5)+
  geom_text(data=subset(world2, name=="China"),aes(x=X, y=Y, label=name),color = "lightsteelblue1", fontface = "bold", check_overlap = TRUE, size=3, nudge_y = -1.5)+
 coord_sf(xlim = c(-106, 140), ylim = c(-52, 55 ), expand = T)+
 # labs(fill = "Phylogenetic Clade (Major_Minor)") +
  guides(fill=guide_legend(nrow=5,title.position = "top"))+theme(legend.position="bottom")
plot_world2

# can edit geom_scatterpie_legend function to improve aesthetics. This is a manual thing:
# uncomment and paste this into lines 37:43to implement same (in pop up box in next line of code):
# list(geom_arc_bar(aes_(x0 = ~x, y0 = ~y, r0 = ~r, r = ~r,
# start = ~start, end = ~end), color="darkblue",linewidth=0.1,data = dd, inherit.aes = FALSE),
# geom_segment(aes_(x = ~x, xend = ~sign(x, maxr * 1.5),
#                   y = ~y + r, yend = ~y + r),linetype="dotted",color="darkblue",data = dd, inherit.aes = FALSE),
# geom_text(aes_(x = ~sign(x, maxr * 1.6), y = ~y + r,
#                label = ~label),color="darkblue",data = dd,size=2.5,fontface = "bold",hjust = hjust, inherit.aes = FALSE,
#           ... = ...))
scatter_leg_fix <-   fix(geom_scatterpie_legend)
# add the scatterpie legend
names=c("1","2-5","6-10",">10")
r=unique(metadata$radius)
plot_world2 +
  scatter_leg_fix(radius=r, n=length(unique(names)),x=-100,y=-50,labeller= function(x) x=c(names))

# Save the plots
ggsave("Figures/fig4_scatterpie_clade_90per.pdf", plot =plot_world2 +
         scatter_leg_fix(radius=r, n=length(unique(names)),x=-100,y=-50,labeller= function(x) x=c(names))
       , width = 11.69, height = 8.27, units="in")


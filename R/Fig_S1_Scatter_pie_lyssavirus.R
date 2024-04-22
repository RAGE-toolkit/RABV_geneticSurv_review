#'---------------------------------------------------------
#'title: R Pie Chart- Circulating lineages in Africa
#'author: Gurdeep Jaswant
#'date: 20/08/2021
#'---------------------------------------------------------

rm(list=ls())
# This clears anything currently in the environment

#############################################
#            INSTALL PACKAGES               #
#############################################
library(rnaturalearth)
library(rnaturalearthdata)
library(scatterpie)

#############################################
#                IMPORT DATA                #
#############################################
metadata<-read.csv(file = "Fig_S1_Scatter_pie_lyssavirus.csv")
# This data is taken from the RABV_GLUE website (http://rabv-glue.cvr.gla.ac.uk/)
# This contains metadata for all available whole genome sequences in the Cosmopolitan clade

#############################################
#              CLEANING DATA                #
#############################################
world<-ne_countries()
# Extract information about the map

#' Find which country names do not match between data and map file
map_countries <- as.character(world$admin)
countries <- unique(metadata$country)
no_match <- setdiff(countries, map_countries); message(length(no_match), " countries are mis-matched: \n", paste0(no_match, collapse="\n"))

# Fix the names
# systematic_data$Country[grepl("", systematic_data$Country)] <- ""

map_countries
metadata$country[grepl("DR Congo", metadata$country)] <- "Democratic Republic of the Congo"
#' How many entries are there for the remaining mis-matches?
table(metadata$country[which(metadata$country %in% no_match)])

# List all the countries included in the metadata
countries <- unique(metadata$country)

# Have a quick look at the number of sequences per country, and the total number of countries
country_table<-table(metadata$country); country_table
length(country_table)

#############################################
#           PROCESS THE DATA                #
#############################################

# Make a table with a column for each subclade (+1 for countries) and a row for each country
lineage_table <- data.frame(matrix(ncol = (length(unique(metadata$assignment))+1), nrow = length(countries)))

# Extract all the different subclades in the metadata, and assign these as column names
x <- c("country", unique(metadata$assignment))
colnames(lineage_table) <- x

# Add all the countries from the previous step to the "country" column
lineage_table$country<-countries

for (i in 1:length(countries)) {
  country_place<-which(lineage_table$country == countries[i])
  # For each country, on order, find which row of the lineage_table corresponds to it
  lineages<-unique(metadata$assignment[which(metadata$country == countries[i])])
  # For this country, search the metadata and see which subclades have been seen in this country
  # Store this information
  
  for (j in 1:length(lineages)) {
    country_lineage<-which(colnames(lineage_table) == lineages[j])
    # For each subclade identified in the previous step in order, see which column corresponds to it
    lineage_table[country_place, country_lineage]<-
      length(which((metadata$assignment[which(metadata$country == countries[i])]) == lineages[j]))
    # Count how many sequences in the metadata there are for both the country AND subclade of interest
    # This value is put into the corresponding box in the table
  }
}

# There are many boxes in the table that are NA - a particular subclade isn't seen in that country
# Turn these into 0's
lineage_table[is.na(lineage_table)] <- 0

#############################################
#       EXTRACT GEOGRAPHICAL INFO           #
#############################################

# Add columns to the table ready to be filled in with the Lat and Long values for each country
lineage_table$LAT<-NA
lineage_table$LON<-NA

# Remove problem values from the table as there will be no Lat and Long values for these
# Unclear why Grenada is a problem, but this was identified in the cleaning step
#lineage_table<-lineage_table[-c(which(lineage_table$country == "-")),]
#lineage_table<-lineage_table[-c(which(lineage_table$country == "Grenada")),]

# For each country in the lineage table, search for it in the 'world' map object we created earlier
# Find the corresponding polygon (spatial info is stored!) for the country
# The labpt option in the polygon contains the lat and long values in a single vector
# Therefore, these need to be indexed ([1] and [2])
# The values are then put in the corresponding LAT and LON columns
for (i in 1:length(lineage_table$country)) {
  lineage_table$LAT[i]<-world@polygons[which(world$admin == lineage_table$country[i])][[1]]@labpt[2]
  lineage_table$LON[i]<-world@polygons[which(world$admin == lineage_table$country[i])][[1]]@labpt[1]
}

# The data needs to be processed in a way that can be read into the maps

# Count the number of rows (countries) in the table
n <- nrow(lineage_table)
# Add a column that gives a number to each country
lineage_table$region <- factor(1:n)
# Add a column called "radius" which will be filled with information about the number of sequences
# This is so the size of the pies corresponds to the number of sequences
lineage_table$radius<-NA
# For each country, count the total number of sequences across all subclades
# Enter this information into the radius column
# The sum index looks a little odd; this is because we don't want to include the LAT, LON, region and radius values
# Therefore, we want to only use the columns that are for lineages
for (i in 1:length(lineage_table$country)) {
  lineage_table$radius[i]<-sum(lineage_table[i,2:(1+length(unique(metadata$assignment)))])
}

# Some of these radii are BIG; these would result in pies bigger than the map!
# Therefore, edit this so those with more sequences are larger, but reasonably sized
#lineage_table$radius[which(lineage_table$radius %in% 7:10)]<-8
#lineage_table$radius[which(lineage_table$radius %in% 11:20)]<-7
#lineage_table$radius[which(lineage_table$radius %in% 21:50)]<-8
#lineage_table$radius[which(lineage_table$radius %in% 51:100)]<-9
#lineage_table$radius[which(lineage_table$radius %in% 101:200)]<-10
#radius[which(lineage_table$radius > 200)]<-11

# Now we've settled on values, divide them all to make them small enough to look nice
lineage_table$radius<-lineage_table$radius/2

#############################################
#              MAKE THE MAPS                #
#############################################

# The world data now needs to be in a slightly different form to plot it
world <- ne_countries(scale = "medium", returnclass = "sf")
# Plot a basic map which we will add pies to
plot<-
  ggplot(data = world) +
  geom_sf() +
  #kb: have added this code to remove all the grid lines and bkg colour
  theme(panel.grid.major = element_blank())+ 
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
   #this bit removes the axis lables     
        axis.title.y=element_blank(),
        axis.title.x=element_blank())
plot

# This adds the pies!
# We are taking the basic map from the last step and telling geom_scatterpie there is data in the lineage_table
# It then uses the LON, LAT, region and radius columns we made in the last step
# We also tell it what the names of the different elements in the pies are (the subclades)
plot_world<-plot + geom_scatterpie(aes(x=LON, y=LAT, group=region, r=radius),
                                   data=lineage_table, cols=c(colnames(lineage_table)[2:(1+length(unique(metadata$assignment)))]), color=NA, alpha=.8)

plot_world

# Some of the areas are difficult to see clearly on the world map, so we can zoom in with an extra argument
# We can also adjust the radii to be smaller so it's easier to distinguish countries when we zoom in
#lineage_table$radius<-lineage_table$radius/1.5

plot_zoom<-plot + geom_scatterpie(aes(x=LON, y=LAT, group=region, r=radius),
                                  data=lineage_table, cols=c(colnames(lineage_table)[2:(1+length(unique(metadata$assignment)))]), color=NA, alpha=.8)+
  geom_scatterpie_legend(radius=lineage_table$radius, n=10, x=-12,y=-20,labeller= function(x) x=unique(lineage_table$region)[c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,7.5)])+
  theme(legend.position = c(-15.55, 0.4))+theme(legend.position = "right")+
coord_sf(xlim = c(-20,60), ylim = c(-50,38))
plot_zoom

# Save the plots! 
ggsave("Figures/world_pie_map_updated.png", plot = plot_world, width = 45, height = 20)
ggsave("Figures/world_pie_map_zoom_updated.png", plot = plot_zoom)


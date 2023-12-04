#'---------------------------------------------------------
#'title: Fig 4, clade piecharts + underlying #seq/ country
#'author: Kirstyn Brunker
#'date: 16/11/2023
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

#############################################
#                IMPORT DATA                #
#############################################
# Data formatted from summarise_SR_clades.R code
# One row per country with columns for total publications, total sequences and breakdown per clade
metadata <- read.csv("Data/Rabv_seq/SR_90cov_noBat_summary_forPlot.csv")
# change name Country to country to match shapefile
names(metadata)[1]="country"
# change name of RABV clade to Unclassified
names(metadata)[grep("RABV",names(metadata))]="Unclassified"


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

# Just remove Grenada and French Guiana for now (can be plotted but need tiny countries data and not worth it for now)
#metadata=metadata[-grep("Grenada", metadata$country),]
#metadata=metadata[-grep("French Guiana", metadata$country),]

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

# For each country in the lineage table, search for it in the 'world' map object we created earlier
# Find the corresponding polygon (spatial info is stored!) for the country
# The labpt option in the polygon contains the lat and long values in a single vector
# Therefore, these need to be indexed ([1] and [2])
# The values are then put in the corresponding LAT and LON columns
for (i in 1:length(metadata$country)) {
  metadata$LAT[i]<-world@polygons[which(world$admin == metadata$country[i])][[1]]@labpt[2]
  metadata$LON[i]<-world@polygons[which(world$admin == metadata$country[i])][[1]]@labpt[1]
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

## some aesthetic fixes to centroid points fix centroid for chile, phl and malaysia: 

# world_points$X[world_points$name=="Chile"]=metadata$LON[which(metadata$country=="Chile")]
# world_points$Y[world_points$name=="Chile"]=metadata$LAT[which(metadata$country=="Chile")]
# #world_points$X[world_points$name=="Philippines"]=metadata$LON[which(metadata$country=="Philippines")]
# #world_points$Y[world_points$name=="Philippines"]=metadata$LAT[which(metadata$country=="Philippines")]
# metadata$LON[which(metadata$country=="Malaysia")]=world_points$X[world_points$name=="Malaysia"]
# metadata$LAT[which(metadata$country=="Malaysia")]=world_points$Y[world_points$name=="Malaysia"]
world_points$X[world_points$name=="South Africa"]=metadata$LON[which(metadata$country=="South Africa")]
world_points$Y[world_points$name=="South Africa"]=metadata$LAT[which(metadata$country=="South Africa")]

# merge metadata to centroids
world2 <- merge(world_points, metadata, by.x = "admin", by.y="country")
# world2$LON[world2$name=="Malaysia"]=world2$X[world2$name=="Malaysia"]
# world2$LAT[world2$name=="Malaysia"]=world2$Y[world2$name=="Malaysia"]


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
#+ guides(fill = guide_colourbar(title.position = "top"))
plot_world

## colours for lots of data
#n <- 50
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
#col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
## have to manually set the palette based on the above code so as to be able to remove the grey colours
set_pal=c( "#7FC97F" ,"#FDC086" ,"#FFFF99" ,"#F0027F" ,"#BF5B17" ,"#1B9E77" ,"#D95F02"  ,"#E7298A" ,"#66A61E", "#E6AB02","#A6761D" ,"#1F78B4" ,"#B2DF8A" ,"#33A02C" ,"#FB9A99" ,"#E31A1C" ,"#FDBF6F" ,"#FF7F00" ,"#CAB2D6" ,"#6A3D9A" ,"#FFFF99" ,"#B15928", "#FBB4AE" ,"#B3CDE3" ,"#CCEBC5" ,"#DECBE4" ,"#FED9A6" ,"#FFFFCC" ,"#E5D8BD" ,"#FDDAEC" ,"#B3E2CD" ,"#FDCDAC" ,"#CBD5E8" ,"#F4CAE4" ,"#E6F5C9", "#FFF2AE" ,"#F1E2CC" ,"#E41A1C" ,"#377EB8" ,"#4DAF4A", "#984EA3" ,"#FF7F00" ,"#FFFF33" ,"#A65628" ,"#F781BF" ,"#66C2A5" ,"#FC8D62", "#8DA0CB" ,"#E78AC3" ,"#A6D854" ,"#FFD92F", "#E5C494" ,"#8DD3C7" ,"#FFFFB3" ,"#BEBADA" ,"#FB8072" ,"#80B1D3" ,"#FDB462" ,"#B3DE69" ,"#FCCDE5", "#BC80BD" ,"#CCEBC5" ,"#FFED6F")
pie(rep(1,length(set_pal)), col=sample(set_pal, length(set_pal)))

# add country names for reference on map
name_country <- c("Brazil", "Mexico", "Haiti", "China", "Indonesia", "South Africa", "Kenya", "Iran", "India", "Nigeria", "Argentina", "Algeria")

# indicate which columns are the clade data
clade_cols <-which(grepl("cosmo|asian|arctic|indian|africa|unclassified", colnames(metadata),ignore.case = TRUE))

# Add scatterpies (iso_a3: can be used instead of country names)

plot_world2<-plot_world + 
  new_scale("fill")+
 geom_scatterpie(aes(x=LON, y=LAT, group=region, r=(radius+0.1)), data=metadata, cols=c(colnames(metadata)[clade_cols]), fill="darkblue",color=NA, alpha=1)+
  geom_scatterpie(aes(x=LON, y=LAT, group=region, r=radius), data=metadata, cols=c(colnames(metadata)[clade_cols]), color=NA, alpha=0.8,sorted_by_radius = T)+
  scale_fill_manual(values=set_pal)+
  geom_text(data=subset(world2, name %in% name_country),aes(x=X, y=Y, label=name),color = "darkblue", fontface = "bold", check_overlap = F, size=3, nudge_y = -1.5)+
  geom_text(data=subset(world2, name=="China"),aes(x=X, y=Y, label=name),color = "lightsteelblue1", fontface = "bold", check_overlap = TRUE, size=3, nudge_y = -1.5)+
 coord_sf(xlim = c(-106, 140), ylim = c(-52, 55 ), expand = T)+
  labs(fill = "Phylogenetic clade") +
  guides(fill=guide_legend(nrow=5,title.position = "top"))+theme(legend.position="bottom")
plot_world2

# can edit geom_scatterpie_legend function to improve aesthetics. This is a manual thing:
## uncomment and paste this into lines 37:43to implement same (in pop up box in next line of code):
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


## save legend separately (need for regional maps)
## Extract the legend. Returns a gtable
leg <- get_legend(plot_world2)
leg2 <- get_legend(plot_world2+
  guides(fill=guide_legend(ncol=2,title.position = "top")))
# Convert to a ggplot and print
leg_plot <- as_ggplot(leg)
leg_plot2 <- as_ggplot(leg2)

## world map without legend (for regional maps code)
plot_world3 <- plot_world2+theme(legend.position="none")
plot_world3 


### REGIONAL MAPS

# asia
asia <- ne_countries(continent="asia",returnclass = "sf") 
bbox.asia<-st_bbox(asia)
# need to mask other continents 
mask_africa <- world[world$continent == 'Africa', ]
# which blocks israel so have to re-plot
israel <- world[world$name == 'Israel', ]

plot_asia=plot_world3+  
  geom_sf(data=mask_africa, fill="aliceblue", col="aliceblue", lwd=2) +
  coord_sf(xlim = c(bbox.asia[1], bbox.asia[3]), ylim = c(bbox.asia[2], bbox.asia[4]), expand = F)+
geom_scatterpie(aes(x=LON, y=LAT, group=region, r=radius), data=subset(metadata,country=="Israel"), cols=clade_cols, color=NA, alpha=0.8)+
  geom_text(data=subset(world2, name=="Israel"),aes(x=X, y=Y, label=name),color = "darkblue", fontface = "bold", check_overlap = TRUE, size=3)
plot_asia=plot_asia+
  scatter_leg_fix(radius=r, n=length(unique(names)),x=40,y=-6,labeller= function(x) x=c(names))


# africa
africa <- ne_countries(continent="africa",returnclass = "sf") 
bbox.africa<-st_bbox(africa)
plot_africa=plot_world3 +
  coord_sf(xlim = c(bbox.africa[1], bbox.africa[3]), ylim = c(bbox.africa[2], bbox.africa[4]), expand = F)
plot_africa

# lac
lac <- world[world$region_wb == 'Latin America & Caribbean'| world$continent=="South America", ]
bbox.lac<-st_bbox(lac)
plot_lac=plot_world3 +
  coord_sf(xlim = c(bbox.lac[1], bbox.lac[3]), ylim = c(bbox.lac[2], bbox.lac[4]), expand = F)
plot_lac

#grid.arrange(plot_africa, plot_lac,plot_asia,nrow=2, ncol=2)
grid.arrange(plot_africa, plot_lac,plot_asia, leg_plot,layout_matrix = matrix(c(1, 3, 2, 3,4, 4), nrow = 2))
grid.arrange(plot_africa, plot_lac,plot_asia, leg_plot2,layout_matrix = matrix(c(1, 3, 2, 3,4, 4), nrow = 2))

##OR use patchwork (bit easier to understand I think)
(plot_africa+plot_lac)/plot_asia|leg_plot2

# Save the plots
ggsave("Figures/fig4_scatterpie_clade_90per.pdf", plot =plot_world2 +
         scatter_leg_fix(radius=r, n=length(unique(names)),x=-100,y=-50,labeller= function(x) x=c(names))
       , width = 11.69, height = 8.27, units="in")
ggsave("Figures/fig4_alt_scatterpie_clade_90per.pdf", plot =(plot_africa+plot_lac)/plot_asia|leg_plot2, width = 11.69, height = 8.27, units="in")


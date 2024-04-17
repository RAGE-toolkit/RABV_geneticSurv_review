#'---------------------------------------------------------
#'title: R Heat map
#'author: Gurdeep Jaswant
#'date: 20/08/2021
#'---------------------------------------------------------
rm(list=ls())

# Libraries
library(tidyverse)
library(reshape)
library(ggplot2)
library(RColorBrewer)
library(patchwork)  
# library(hrbrthemes)
# library(plotly)
# #devtools::install_github("talgalili/d3heatmap")
# library(d3heatmap)
# library(heatmaply)

# Load data 
# data <- read.csv("Data/globally_heatmap_fig3.csv")
dat <- read.csv("Data/heatmap_fig3.csv")

# Remove or rename some data/ country names
dat <- dat[-which(dat$Country == "West Indies"),]
dat$Country[which(dat$Country == "United Arab Emirates")] <- "UAE"
dat$Country[which(dat$Country == "United Republic of Tanzania")] <- "Tanzania"
dat$Country[which(dat$Country == "Democratic Republic of the Congo")] <- "DRC"
dat$Country[which(dat$Country == "Central African Republic")] <- "CAR"

# Matrix format
global_mat <- as.matrix(dat[,-c(1,2)])
rownames(global_mat) <- dat$Country
n = rowSums(global_mat)
N = colSums(global_mat)

# Set up the regional matrices
# Asia
asia_dat <- subset(dat, Region == "Asia")[,-c(1,2)]
N = colSums(asia_dat)
names(asia_dat) <- paste0(colnames(asia_dat), ", N=", N)
asia_mat <- as.matrix(asia_dat)
rownames(asia_mat) <- subset(dat, Region == "Asia")$Country

# Africa
africa_dat <- subset(dat, Region == "Africa")[,-c(1,2)]
N = colSums(africa_dat)
names(africa_dat) <- paste0(colnames(africa_dat), ", N=", N)
africa_mat <- as.matrix(africa_dat)
rownames(africa_mat) <- subset(dat, Region == "Africa")$Country

# Americas
americas_dat <- subset(dat, Region == "Americas")[,-c(1,2)]
N = colSums(americas_dat)
names(americas_dat) <- paste0(colnames(americas_dat), ", N=", N)
americas_mat <- as.matrix(americas_dat)
rownames(americas_mat) <- subset(dat, Region == "Americas")$Country

# Colours
colsAs = c(colorRampPalette(brewer.pal(6,"Greens"))(6))[6]
colsAf = c(colorRampPalette(brewer.pal(6,"Oranges"))(6))[6]
colsAm = c(colorRampPalette(brewer.pal(6,"Blues"))(6))[6]

# Transform the matrix in long format
# Americas
colnames(americas_mat) <- names(americas_dat)
rownames(americas_mat) <- subset(dat, Region == "Americas")$Country
df <- melt(americas_mat)
colnames(df) <- c("x", "y", "value")

am_hm  <- ggplot(df, aes(x = y, y = x, fill = value)) + 
  geom_tile() +
  xlab("") + ylab("Country") + ggtitle("Americas") +
  scale_fill_continuous(breaks=seq(0,60, by=10),low='white',high=colsAm, name="Studies") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
am_hm

# Africa
colnames(africa_mat) <- names(africa_dat)
rownames(africa_mat) <- subset(dat, Region == "Africa")$Country
df <- melt(africa_mat)
colnames(df) <- c("x", "y", "value")

af_hm  <- ggplot(df, aes(x = y, y = x, fill = value)) + 
  geom_tile() +
  xlab("") + ylab("") + ggtitle("Africa") +
  scale_fill_continuous(breaks=seq(0,60, by=10),low='white',high=colsAf, name="Studies") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
af_hm

# Asia
colnames(asia_mat) <- names(asia_dat)
rownames(asia_mat) <- subset(dat, Region == "Asia")$Country
df <- melt(asia_mat)
colnames(df) <- c("x", "y", "value")

as_hm  <- ggplot(df, aes(x = y, y = x, fill = value)) + 
  geom_tile() +
  xlab("") + ylab("") + ggtitle("Asia") +
  scale_fill_continuous(breaks=seq(0,55, by=10),low='white',high=colsAs, name = "Studies") +
  theme(axis.text.x=element_text(angle=45,hjust=1))
as_hm

ggsave("Figures/fig3_heatmap.pdf", 
       plot = am_hm + af_hm + as_hm, 
       width = 11, height = 6, units="in")



# Some code for interactive heatmaps
# d3heatmap(mat, scale="column", dendrogram = "none", width="800px", height="80Opx", colors = "Blues")
global_heatmap <- heatmaply(
  global_mat, dendrogram = "none",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low = "white", high = "red", limits = c(0, 300)))
global_heatmap

# Asia
asia_hm <- heatmaply(
  asia_mat, dendrogram = "none",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low = "white", high = "red", limits = c(0, 55))
)
asia_hm

# Africa
africa_hm <- heatmaply(
  africa_mat, dendrogram = "none",
  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
    low ="white", high = "red", limits = c(0, 55))
)
africa_hm

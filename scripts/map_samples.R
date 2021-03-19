#### Map sampling sites ####
#### https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html

library("ggplot2")
library("dplyr")

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("ggspatial")
library("maps")
library("rgeos")
library("tools")

library("ggrepel")
library("ggtext")
library("svglite")

### Download these data describing river paths
## note there are two: one broader scale and one finer scale
# https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-rivers-lake-centerlines/
# unzip("ne_10m_rivers_lake_centerlines.zip", exdir = "ne_10m_rivers_lake_centerlines")
# unzip("ne_10m_rivers_north_america.zip", exdir = "ne_10m_rivers_north_america")

#### Collections
sample_meta.dat <- read.csv("Git_Repo/info/pop_metadata.csv",
                            header = TRUE, stringsAsFactors = FALSE)

#### Release data from Ellyn
Bitume_release.dat <- read.csv("subset_EBitume Draft Table of Diorhabda spp releases 2018_11_20.xlsx - Table.csv",
                               na.strings = "")
Bitume_release.dat$GPS.Lat <- as.numeric(as.character(Bitume_release.dat$GPS.Lat))
Bitume_release.dat$GPS.Long <- as.numeric(as.character(Bitume_release.dat$GPS.Long))

#### Release data from Knutson
release.dat <- read.csv("Knutsen_2019_suppdata.csv")
release.dat %>%
  select(Species, ID, Latitude, Longitude)

table(Bitume_release.dat$Species)
table(release.dat$Species)

### Color choices
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
spp <- c('carinata', 'elongata', 'sublineata', 'carinulata', 'elongata_sub', 'carinulata_sub', 'other')
human_colors <- c('blue', 'orange', 'green', 'yellow', 'dark blue', 'red', 'pink')
legend_colors <- as.data.frame(cbind(cbbPalette, human_colors, spp), stringsAsFactors = F)
legend_colors


#### Map data
shapeData <- rgdal::readOGR("map/ne_10m_rivers_lake_centerlines/",
                            "ne_10m_rivers_lake_centerlines")
shapeData@data$id <- rownames(shapeData@data)
watershedPoints <- fortify(shapeData, region = "id")
watershedDF <- merge(watershedPoints, shapeData@data, by = "id")


### Focusing on N America
NA_shapeData <- rgdal::readOGR("map/ne_10m_rivers_north_america/", "ne_10m_rivers_north_america")
NA_shapeData@data$id <- rownames(NA_shapeData@data)
NA_watershedPoints <- fortify(NA_shapeData, region = "id")
NA_watershedDF <- merge(NA_watershedPoints, NA_shapeData@data, by = "id")

NA_watershedDF$scalerank <- as.numeric(NA_watershedDF$scalerank)

NAwater <- NA_watershedDF %>%
  dplyr::filter(long < -95 & long >-120 & lat > 25 & lat <44.8)
NA_water <- NA_watershedDF %>%
  dplyr::filter(long < -95 & long >-120 & lat > 25 & lat <44.8)

## state and county polygons
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
states$ID <- toTitleCase(states$ID)
world <- ne_countries(scale = "medium", returnclass = "sf")

### Focusing on samples collected in N America
NAsamples <- sample_meta.dat %>%
  dplyr::filter(Longitude < -95 & Longitude >-120 & Latitude > 25 & Latitude <50)

theme_set(theme_bw())

release_sampling_map <- ggplot(data = world) +
  geom_sf(fill= "white") + # white land
  theme(panel.grid.major = element_line(color = gray(.5),  # panel settings
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"), # ocean
        legend.position = "bottom",
        legend.box="vertical", legend.margin=margin(),
        legend.text = element_markdown()) + # from ggtext to italicize spp names
  geom_path(data=watershedDF, # rivers
            aes(x = long, y = lat, group = group),
            color = 'lightblue', size=as.numeric(watershedDF$scalerank*.1)) +
  geom_path(data=NA_water, # more rivers
            aes(x = long, y = lat, group = group),
            color = 'lightblue', size=as.numeric(NA_water$scalerank*.06)) +
  geom_sf(data = states, fill = NA) + # state boundaries
  geom_jitter(data=release.dat, # knutson release data
             aes(x=Longitude, y=Latitude, fill = Species),
             shape = 21, size =6,  alpha = 0.5)  +
  geom_jitter(data=Bitume_release.dat, # bitume release data
             aes(x=GPS.Long, y=GPS.Lat, fill = Species),
             shape = 21, size =6,  alpha = 0.5)  +
  geom_point(data=NAsamples, # samples collected in North America
             aes(x=Longitude, y=Latitude, shape = Grp),
             size =3, fill = "black") +
  scale_shape_manual(name= "Sampled localities", # differentiate the carinulata sampling and hybrid zone
                     values = c(15, 16, 17, 1),
                     labels = c("*D. carinulata* Range", "Suspected Hybrid Zone")) +
  geom_label_repel(data=NAsamples, point.padding = .2, # label points
                   segment.size  = .5,
                   segment.color = "black",
                   nudge_x = .15,
                   nudge_y = .1,
                   size = 3.7,
                   aes(x=Longitude, y=Latitude, label= popID),
                   ylim=c(29,44.8)) + # lat limits for ggrepel
  ylab("Latitude") + # y axis label
  xlab("Longitude") + # x axis label
  scale_fill_manual(name = expression(~italic("Diorhabda")~"species field-released"), # mardown doesn't work here to italicize
                    values=cbbPalette[c(1,4,2,3)],
                    labels = c("*D. carinata*", "*D. carinulata*", "*D. elongata*", "*D. sublineata*")) + # mardown works here
  coord_sf(xlim = c(-119, -98), ylim = c(28.5, 46),
           expand = FALSE)

release_sampling_map

ggsave("releasedat_2-11-20_sampling_map.jpg", width = 8, height = 7, dpi =300)
ggsave("releasedat_2-11-20_sampling_map.svg", width = 8, height = 7, dpi =300) # svg for later editing


### Native range map
dplyr::filter(sample_meta.dat, cat == "native")

## source collection data
native_range <- read.csv("source_populations.csv") %>%
  mutate(Grp = paste0("source_",Species)) %>%
  rename(popID = Pop) %>%
  full_join(
    dplyr::filter(sample_meta.dat, cat == "native")
  )

elongata_native <- native_range[grep("elongata", native_range$Grp),]

GRmap <- ggplot(data = world) +
  geom_sf(fill= "white") +
  geom_path(data=watershedDF,
            aes(x = long, y = lat, group = group),
            color = 'lightblue', size=as.numeric(watershedDF$scalerank*.1)) +
  geom_label_repel(data=elongata_native,
                   segment.color = "black",
                   aes(x=Longitude, y=Latitude, label= popID)) +
  geom_point(data=dplyr::filter(elongata_native,Species == "elongata"),
             aes(x=Longitude, y=Latitude, shape = Grp, fill = Grp),
             size =4, alpha = 0.7) +
  geom_point(data=dplyr::filter(elongata_native,cat == "native"),
             aes(x=Longitude, y=Latitude, shape = Grp, fill = Grp),
             size =3) +
  scale_fill_manual(name= "",
                    values= c("black", "#E69F00"),
                    labels = c("Native range samples", "Original *D. elongata* source collections")) +
  scale_shape_manual(name= "",
                     values = c(17,21),
                     labels = c("Native range samples", "Original *D. elongata* source collections")) +
  theme(legend.key = element_rect(fill = "transparent", colour = "transparent"),
        legend.position = "right") +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(20, 27), ylim = c(34, 41.5),
           expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(.5),
                                        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"),
        legend.text = element_markdown()) +
  xlab("Longitude") + ylab("Latitude")
GRmap
ggsave("GR_sampling_map.jpg", width = 9, height = 9, dpi = 300)

### D. carinulata native range sampling and source populations

carinulata_native <- native_range[grep("carinulata", native_range$Grp),]
carinulata_native <- carinulata_native[-2,] # drop Turpan because it's not addressed here
CHmap <- ggplot(data = world) +
  geom_sf(fill= "white") +
  geom_label_repel(data=carinulata_native,
                   segment.color = "black",
                   aes(x=Longitude, y=Latitude, label= popID)) +
  geom_point(data=dplyr::filter(carinulata_native,Species == "carinulata"),
             aes(x=Longitude, y=Latitude, shape = Grp, fill = Grp),
             size =4, alpha = 0.7) +
  geom_point(data=dplyr::filter(carinulata_native,cat == "native"),
             aes(x=Longitude, y=Latitude, shape = Grp, fill = Grp),
             size =3) +
  scale_fill_manual(name= "",
                    values= c("black", "#F0E442"),
                    labels = c("Native range samples", "Original *D. carinulata* source collections")) +
  scale_shape_manual(name= "",
                     values = c(17,21),
                     labels = c("Native range samples", "Original *D. carinulata* source collections")) +
  geom_path(data=watershedDF,
            aes(x = long, y = lat, group = group),
            color = 'lightblue', size=as.numeric(watershedDF$scalerank*.01)) +
  annotation_scale(location = "bl", width_hint = 0.25) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_x = unit(0.4, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(75,95), ylim = c(40, 50),
           expand = FALSE) +
  theme(panel.grid.major = element_line(color = gray(.5),
        linetype = "dashed", size = 0.5),
        panel.background = element_rect(fill = "aliceblue"),
        legend.text = element_markdown()) +
  xlab("Longitude") + ylab("Latitude")
CHmap
ggsave("China_sampling_map.jpg", width = 9, height = 9, dpi = 300)

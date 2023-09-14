
# Load libraries
################
library(tidyverse)
library(tidyterra)
library(sf)
library(camcorder)
library(terra)

# Set plot size and resolution
##############################
gg_record(
  dir = file.path(tempdir(),"recording"), 
  device = "png", 
  width = 60, 
  height = 60, 
  units = "cm", 
  dpi = 600 
)

# Load spatial files
####################

# Create a list with all Combined Drought Index rasters from 2022
# (Data downloaded at https://edo.jrc.ec.europa.eu/gdo/php/index.php?id=2112 
# and uploaded to github for reproducibility)
# year 2022 was a severe year for drought (https://climate.copernicus.eu/esotc/2022/drought)

rst<-c(
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220101_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220111_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220121_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220201_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220211_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220221_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220301_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220311_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220321_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220401_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220411_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220421_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220501_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220511_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220521_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220601_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220611_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220621_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220701_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220711_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220721_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220801_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220811_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220821_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220901_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220911_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20220921_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221001_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221011_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221021_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221001_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221011_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221021_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221101_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221111_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221121_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221201_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221211_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221221_t.tif')
)

# Load basemap with country borders
# (data from Natural Earth : https://www.naturalearthdata.com/downloads/50m-cultural-vectors/)
world <- read_sf('data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp')%>%
  st_transform(crs(rst[[1]]))

# Data cleaning
###############

# There is many levels in the CDIndex (see here for more info: https://nhess.copernicus.org/articles/21/481/2021/)
# As the aim here is to produce a "little picture", I've chosen to reduce the complexity 
# - by focusing on level 3, which is the most severe (alert);
# - and producing a single map for the whole year

# For the 1st date, I modify the raster to set pixels to 0 if they are not in alert state
cdi_2022 <- ifel(rst[[1]]!=3,0,rst[[1]])
names(cdi_2022)<-"CDI"

# I will then do the same thing for all dates, combining all infos on a single raster

for (i in 2:dim(rst)[[3]]){
  r<-ifel(rst[[i]]!=3,0,rst[[i]])
  names(r)<-"CDI"
  cdi_2022<-cdi_2022+r
}

# Again, for simplicity's sake, I'm not studying the duration of droughts, just the spatial extent of the phenomena in 2022.
# This way, the 0-coded pixels will be the ones never to have experienced a drought alert during 2022 (1 otherwise)
simp_2022 <- ifel(cdi_2022>0,1,cdi_2022)

# To aggregate the pixels, I will create a grid based on the raster extsension
# Extract raster extension:
bound<-ext(simp_2022)
# Make grid:
grd<-tibble(
  lon=c(bound[1],bound[2]),
  lat=c(bound[3],bound[4])
)%>%
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = crs(clc_2000)
  ) %>% 
  st_bbox()%>% 
  st_make_grid(
    n=c(60,60),
    square=FALSE
  )%>%
  st_as_sf()

# To compute the area affect by drought on each cell of the grid,
# I compute (i) the total number of pixels per cell and (ii) the number of pixels affected by drought
ct <- zonal(ifel(cdi_2022>=0,1,cdi_2022),vect(grd),fun="sum")
tst<-zonal(simp_2022,vect(grd),fun="sum")

# Then, to make the graph more accessible to everyone, I've converted the grid to centroids, 
# so that I can work with valued points later (a bit like Jacques Bertin's way) 
# instead of color gradient (that are more difficult to read for people with visual impairments).
cent<-bind_cols(grd,tst,ct)%>%
  st_centroid()

colnames(cent)<-c('geometry','dry','all')
st_geometry(cent)<-'geometry'

cent<-cent%>%
  mutate(ratio_dry=dry/all)%>%
  mutate(dry_cl=case_when(
    ratio_dry<0.05~"A",
    ratio_dry<0.25~"B",
    ratio_dry<0.5~"C",
    ratio_dry<0.75~"D",
    TRUE~"E"
  ))


# Make plot
###########

# offset values to extent the map beyond raster boundaries
off_x <- 100000
off_y <- 500000

# points color
alp <- 1
col_dry <- alpha("#ff1b6b",alp)

# background and country colors
back<-"#191930"
country<-"#151529"

# plot
ggplot()+
  geom_sf(
    world,mapping=aes(geometry=geometry),
    fill=country,color=country
  )+
  geom_sf(
    data=cent%>%drop_na(ratio_dry),aes(size=dry_cl),
    color=col_dry,pch=19
  )+
  guides(size="none")+
  scale_size_manual(values=c(0,1.5,3,4.5,6))+
  scale_x_continuous(limits=c(bound[1]-off_x,bound[2]+off_x))+
  scale_y_continuous(limits=c(bound[3]-off_y,bound[4]+off_y))+
  theme_void()+
  theme(plot.background=element_rect(fill=back,color=NA))


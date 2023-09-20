# Load libraries
################
library(tidyverse)
library(tidyterra)
library(sf)
library(camcorder)
library(terra)
library(showtext)

# Set fonts
font_add_google("Raleway","ral")
font_add_google("Bitter","bit")
showtext_auto()

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
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221101_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221111_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221121_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221201_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221211_t.tif'),
  rast('https://github.com/BjnNowak/drought/raw/main/data/cdinx_m_euu_20221221_t.tif')
)

# Vector with dates
dts <- c(
  'January 1, 2022',
  'January 11, 2022',
  'January 21, 2022',
  'February 1, 2022',
  'February 11, 2022',
  'February 21, 2022',
  'March 1, 2022',
  'March 11, 2022',
  'March 21, 2022',
  'April 1, 2022',
  'April 11, 2022',
  'April 21, 2022',
  'May 1, 2022',
  'May 11, 2022',
  'May 21, 2022',
  'June 1, 2022',
  'June 11, 2022',
  'June 21, 2022',
  'July 1, 2022',
  'July 11, 2022',
  'July 21, 2022',
  'August 1, 2022',
  'August 11, 2022',
  'August 21, 2022',
  'September 1, 2022',
  'September 11, 2022',
  'September 21, 2022',
  'October 1, 2022',
  'October 11, 2022',
  'October 21, 2022',
  'November 1, 2022',
  'November 11, 2022',
  'November 21, 2022',
  'December 1, 2022',
  'December 11, 2022',
  'December 21, 2022'
)

# Load basemap with country borders
# (data from Natural Earth : https://www.naturalearthdata.com/downloads/50m-cultural-vectors/)
world <- read_sf('data/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp')%>%
  st_transform(crs(rst[[1]]))

# To aggregate the pixels, I will also create a grid based 
# on the raster exstension (to be used later in the process)

# Extract raster extension:
bound<-ext(rst[[1]])
# Make grid:
grd<-tibble(
  lon=c(bound[1],bound[2]),
  lat=c(bound[3],bound[4])
)%>%
  st_as_sf(
    coords = c("lon", "lat"), 
    crs = crs(rst[[1]])
  ) %>% 
  st_bbox()%>% 
  st_make_grid(
    n=c(60,60),
    square=FALSE
  )%>%
  st_as_sf()

# Creating a list to store centroids object
cent<-list(1:dim(rst)[3])

# Data cleaning
###############

# There is many levels in the CDIndex (see here for more info: https://nhess.copernicus.org/articles/21/481/2021/)
# As the aim here is to produce a "little picture", I've chosen to reduce the complexity by focusing on level 3, which is the most severe (alert);

for (i in 1:dim(rst)[[3]]){
  
#for (i in 1:1){

# For the 1st date, I modify the raster to set pixels to 
# - 0 if they are not in alert state
# - 1 if alert state 
  rst[[i]] <- ifel(rst[[i]]!=3,0,1)
  names(rst[[i]])<-"CDI"

  ct <- zonal(ifel(rst[[i]]>=0,1,rst[[i]]),vect(grd),fun="sum")
  tst <- zonal(rst[[i]],vect(grd),fun="sum")

  cent[[i]]<-bind_cols(grd,tst/ct)%>%
    st_centroid()

  st_geometry(cent[[i]])<-'x'

  cent[[i]]<-cent[[i]]%>%
    mutate(dry_cl=case_when(
      CDI<0.05~"A",
      CDI<0.25~"B",
      CDI<0.5~"C",
      CDI<0.75~"D",
      TRUE~"E"
    ))

}


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


for (i in 1:dim(rst)[[3]]){

  # plot
  pl<-ggplot()+
    geom_sf(
      world,mapping=aes(geometry=geometry),
      fill=country,color=country
    )+
    geom_sf(
      data=cent[[i]]%>%drop_na(CDI),aes(size=dry_cl),
      color=col_dry,pch=19
    )+
    annotate(
      'text',label='Drought in Europe',x=bound[1],y=bound[4],
      family='bit',size=120,color=col_dry,hjust=0,fontface='bold'
    )+
    annotate(
      'text',label=dts[i],x=bound[1],y=bound[4]-off_y/2,
      family='ral',size=110,color=col_dry,hjust=0
    )+
    guides(size="none")+
    scale_size_manual(values=c(0,1.5,3,4.5,6))+
    scale_x_continuous(limits=c(bound[1]-off_x,bound[2]+off_x))+
    scale_y_continuous(limits=c(bound[3]-off_y,bound[4]+off_y))+
    theme_void()+
    theme(plot.background=element_rect(fill=back,color=NA))
  
  print(pl)
  
}

# Make animation
gg_playback(
  name = file.path(tempdir(), "recording", "drought.gif"),
  first_image_duration = 1,
  last_image_duration = 1,
  frame_duration = 2,
  image_resize = 1200
)


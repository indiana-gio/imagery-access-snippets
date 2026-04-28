library(arcgis)
library(tmap)
library(sf)




spencer_extent <- list(
  xmin=-9660204.6847, 
  ymin=4761312.6569,
  xmax=-9655985.7878,
  ymax=4763804.2792,
  spatialReference=list(wkid=3857)
)

bbox <- arcgisutils::from_envelope(spencer_extent)

flood_url <- "https://gisdata.in.gov/server/rest/services/Hosted/FloodHazard_BestAvai_DNR_Water_PROD/FeatureServer/0"
flood_fs <- arc_open(flood_url)
flood_fields <- c("objectid", "fld_zone","zone_subty")

flood_sdf <- arc_select(flood_fs,
  fields=flood_fields,
  filter_geom=bbox,
  spatialReference=3857)
flood_sdf$geometry <- st_transform(flood_sdf$geometry, 3857)

parcel_url <- "https://gisdata.in.gov/server/rest/services/Hosted/Parcel_Boundaries_of_Indiana_2025/FeatureServer/0"
parcel_fs <- arc_open(parcel_url)
parcel_fields <- c("objectid", "state_parcel_id")

parcel_sdf <- arc_select(parcel_fs,
  fields=parcel_fields,
  filter_geom=bbox)
parcel_sdf$geometry <- st_transform(parcel_sdf$geometry, 3857)


attach(flood_sdf)
floodways <- flood_sdf[fld_zone == "A" | fld_zone == "AE",]
small_risk <- flood_sdf[zone_subty == "0.2 PCT ANNUAL CHANCE FLOOD HAZARD",]
min_risk <- flood_sdf[zone_subty == "AREA OF MINIMAL FLOOD HAZARD",]
detach()

parcel_sdf$risk <- 0
idx <- st_intersects(parcel_sdf$geometry,st_combine(min_risk), sparse = FALSE)[TRUE,1]
parcel_sdf$risk[idx] <- 0

idx <- st_intersects(parcel_sdf$geometry,st_combine(small_risk), sparse = FALSE)[TRUE,1]
parcel_sdf$risk[idx] <- 1

idx <- st_intersects(parcel_sdf$geometry,st_combine(floodways), sparse = FALSE)[TRUE,1]
parcel_sdf$risk[idx] <- 2




img_url <- "https://di-ingov.img.arcgis.com/arcgis/rest/services/DynamicWebMercator/Indiana_2023_Imagery/ImageServer"
img_fs <- arc_open(img_url)
base <- arc_raster(img_fs, 
  xmin = bbox[[1]],
  ymin = bbox[[2]],
  xmax = bbox[[3]],
  ymax = bbox[[4]],
  bbox_crs = 3857,
  width = (bbox[[3]] - bbox[[1]]) / 2.2,
  height = (bbox[[4]] - bbox[[2]]) / 2.2
)

tmap_mode('plot')
map <- tm_shape(base) + tm_rgb(tm_vars(x = c('Band_1', 'Band_2', 'Band_3'), multivariate = TRUE))
map <- (map 
  + tm_shape(parcel_sdf[parcel_sdf$risk > 0,]) 
  + tm_fill(fill = 'risk', 
      fill.scale = tm_scale_categorical(values = c("goldenrod1", "coral1")),
      fill_alpha = 0.3) 
  + tm_borders(col_alpha=0.4)
)
map <- map + tm_shape(parcel_sdf[parcel_sdf$risk == 0,]) + tm_borders(col_alpha = 0.4)
map


tmap_mode('view')
map <- tm_shape(base) + tm_rgb(tm_vars(x = c('Band_4', 'Band_3', 'Band_2'), multivariate = TRUE))
map <- map + tm_shape(flood_sdf) + tm_fill(fill = "zone_subty", fill_alpha=0.3)
map <- (map 
        + tm_shape(parcel_sdf[parcel_sdf$risk > 0,]) 
        + tm_fill(fill = 'risk', 
                  fill.scale = tm_scale_categorical(values = c("goldenrod1", "coral1")),
                  fill_alpha = 0.3) 
        + tm_borders(col_alpha=0.4)
)
map <- map + tm_shape(parcel_sdf[parcel_sdf$risk == 0,]) + tm_borders(col_alpha = 0.8)
map


                   
library(arcgis)
library(tmap)
library(terra)
  

dtown_extent <- list(
  xmin=-9591286.1075, 
  ymin=4832122.9457,
  xmax=-9590849.6718,
  ymax=4832605.7268,
  spatialReference=list(wkid=3857)
)
bbox <- arcgisutils::from_envelope(dtown_extent)

btown_extent <- list(
  xmin = -9634778.3634,
  ymin =  4756930.7589,
  xmax = -9634305.4898,
  ymax = 4757256.2433,
  spatialReference=list(wkid=3857)
)
bbox <- arcgisutils::from_envelope(btown_extent)

img_url <- "https://di-ingov.img.arcgis.com/arcgis/rest/services/DynamicWebMercator/Indiana_2021_Imagery/ImageServer"
img_fs <- arc_open(img_url)
base <- arc_raster(img_fs, 
                   xmin = bbox[[1]],
                   ymin = bbox[[2]],
                   xmax = bbox[[3]],
                   ymax = bbox[[4]],
                   bbox_crs = 3857,
                   width = (bbox[[3]] - bbox[[1]]) / 0.2,
                   height = (bbox[[4]] - bbox[[2]]) / 0.2
)
base


# sobel operator weights
vertical <- matrix(data = c(-1, 0, 1, -2, 0, 2, -1, 0, 1),3, 3)
horizontal <- matrix(data = c(-1, -2, -1, 0, 0, 0, 1, 2, 1), 3, 3)

sobel <- function(band) {
  b_h <- focal(band, w=vertical)
  b_v <- focal(band, w=horizontal)
  result <- sqrt(b_h**2 + b_v**2) #combine directions
  result
}


avg <- mean(base) # 4 band grayscale
sb = sobel(avg$mean)

tmap_mode('view')
sb$blueb <- 0 
map <- tm_shape(base) + tm_rgb(col = tm_vars(x = c('Band_4', 'Band_3', 'Band_2'), multivariate = TRUE))
map <- map + tm_shape(sb) + tm_rgb(col = tm_vars(c('focal_sum','focal_sum','blueb'), multivariate = TRUE), col_alpha = 0.5)
map


tmap_mode('plot')
map <- (tm_shape(base) 
        + tm_rgb(
          col = tm_vars(x = c('Band_1', 'Band_2', 'Band_3'),
          multivariate = TRUE)
        )
  )
map <- map + tm_shape(mask(sb > 150, sb > 150)) + tm_raster(col = 'focal_sum',
  col.scale=tm_scale_categorical(values = c(rgb(0,0,0, alpha = 0), "goldenrod1")))
map

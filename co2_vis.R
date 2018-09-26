a <- read.table("clipboard")
b <- sample(a[[1]], 25)
data.table::data.table(b)
################
library(ggplot2)
library(data.table)
library(gganimate)

plotdata4 <- fread("C:/工作/可视化/CO2/global-carbon-dioxide-emissions-by-sector-gg-co.csv") 
plotdata4 <- plotdata4[Entity == "China", .(region = Entity, Year, Energy, Transport, Industry, `Residential & commercial`)]
plotdata4 <- melt(plotdata4, id = c("region", "Year"))

p <- ggplot(plotdata4[variable == 'Transport'], aes(x = Year, y = value))
p + geom_line() + transition_reveal(Year) + shadow_mark()
ggplot(data, aes(x=Year, y=Value, fill=Sector)) +
  geom_area(colour="black", size=.2, alpha=.4)


colnames(plotdata)[c(1, 4)] <- c("region", "share")
plotdata[, "region"] <- gsub("United States", "USA", plotdata$region)
plotdata[, "region"] <- gsub("Cote d'Ivoire", "Ivory Coast", plotdata$region)
plotdata[, "region"] <- gsub("United Kingdom", "UK", plotdata$region)
plotdata[region == "Taiwan", "share"] <- plotdata[region == "China", share]
plotdata[, "fan"] <- cut(plotdata$share, breaks = seq(2, 7, by = 0.5))
labels <- rep(str_split("2%
2.5%
3%
3.5%
4%
4.5%
5%
5.5%
6%
>6.5%", "\n")[[1]], each = 2)[-c(1,20)] %>% matrix(ncol=2, byrow=T)
labels <- c(paste(labels[, 1], "~", labels[, 2]) %>% str_replace_all(">", ""), "> 6.5%")

labelsdata <- data.table(labels_n = sort(as.numeric(unique(plotdata$fan))), 
                         label = labels)
plotdata[, "labels_n"] <- as.numeric(plotdata$fan)
plotdata <- merge(plotdata, labelsdata, all.x = TRUE)
####################
library(sf)
library(dplyr)
library(ggplot2)
library(gganimate)

## The data

# download shapefiles from:
# https://www.census.gov/geo/maps-data/data/cbf/cbf_counties.html
# then unzip in folder "US_shapes"
us_counties_sp <- rgdal::readOGR(dsn = "US_shapes", layer = "cb_2017_us_county_20m")

# we need appropriate projections for the lower 48 states, for Alaska, and for Hawaii
# ESRI:102003
crs_lower48 <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
# EPSG:3338
crs_alaska <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "
# ESRI:102007
crs_hawaii <- "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-157 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


# aggregate individual counties into states
us_states_sp <- rgeos::gUnaryUnion(us_counties_sp, us_counties_sp$STATEFP)

# collect fips codes; they are the names of the objects after aggregation
us_states_sp$fips_state <- names(us_states_sp)

# convert to sf and project
us_states <- as(us_states_sp, "sf") %>%
  st_transform(crs_lower48) %>%
  filter(fips_state != "72") # remove Puerto Rico

# now remove Alaska and Hawaii for lower 48, and calculate bounding box
us_lower48 <- filter(us_states, !fips_state %in% c("02", "15"))
bb <- st_bbox(us_lower48)


# to move Alaska and Hawaii, we need a helper function that can place an SF geometry object
# at a desired position and scale; the scale option is not used in this code example
place_geometry <- function(geometry, position, scale = 1) {
  (geometry - st_centroid(geometry)) * scale +
    st_sfc(st_point(position))
}

# move Alaska
us_alaska <- filter(us_states, fips_state == "02")
us_alaska2 <- st_transform(us_alaska, crs_alaska)
st_geometry(us_alaska2) <- place_geometry(
  st_geometry(us_alaska2),
  c(bb$xmin - 0*(bb$xmax - bb$xmin),
    bb$ymin - 0*(bb$ymax - bb$ymin))
)
# we're cheating here by assigning a projection that isn't correct; we
# need to do this because the final compound map needs to have a single
# projection
st_crs(us_alaska2) <- crs_lower48

# move Hawaii
us_hawaii <- filter(us_states, fips_state == "15")
us_hawaii2 <- st_transform(us_hawaii, crs_hawaii)
st_geometry(us_hawaii2) <- place_geometry(
  st_geometry(us_hawaii2),
  c(bb$xmin + 0.3*(bb$xmax - bb$xmin),
    bb$ymin + 0.*(bb$ymax - bb$ymin))
)
# cheating, as before
st_crs(us_hawaii2) <- crs_lower48

# now make four data frames:
# 1. original map
# 2. map with Hawaii moved
# 3. map with Hawaii and Alaska moved
# 4. like 3, to generate an additional state in gganimate
# we use prefixes "a_", "b_", etc in the `type` variable as a
# simple cheat to get the animation order right in gganimate
x1 <- us_states
x1$type = "a_original"
x2 <- rbind(us_lower48, us_alaska, us_hawaii2)
x2$type = "b_hawaii"
x3 <- rbind(us_lower48, us_alaska2, us_hawaii2)
x3$type = "c_final"
x4 <- x3
x4$type = "d_final"
x <- rbind(x1, x2, x3, x4)

# bounding boxes for map 1 and 3, needed for zoom
bb1 <- st_bbox(x1)
bb2 <- st_bbox(x3)

# now animate
ggplot(x, aes(group = fips_state)) +
  geom_sf(fill = "#56B4E9", color = "grey30", size = 0.3, alpha = 0.5) +
  transition_states(type, 2, 1) +
  view_zoom_manual(
    2, 1, pause_first = FALSE,
    xmin = c(bb1$xmin, bb1$xmin, bb1$xmin, bb2$xmin),
    ymin = c(bb2$ymin, bb2$ymin, bb2$ymin, bb2$ymin),
    xmax = c(bb1$xmax, bb1$xmax, bb1$xmax, bb2$xmax),
    ymax = c(bb1$ymax, bb1$ymax, bb1$ymax, bb2$ymax)
  )


library(ggplot2)      # For plotting
library(tidycensus)   # For downloading Census data
library(tmap)         # For creating tmap
library(tmaptools)    # For reading and processing spatial data related to tmap
library(dplyr)        # For data wrangling
library(sf)           # For reading, writing and working with spatial objects
library(tidyverse)

#ADD CENSUS KEY
census_api_key("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")


dat12 <- get_acs("county", table = "B27001", year = 2012, 
                 output = "tidy", state = NULL, geometry = FALSE) %>%
  rename(`2012` = estimate) %>%
  select(-NAME, -moe) 

dat16 <- get_acs("county", table = "B27001", year = 2016, 
                 output = "tidy", state = NULL, geometry = TRUE, shift_geo = TRUE) %>%
  rename(`2016` = estimate) %>%
  select(-moe)


# County data
data("county_laea", package = "tidycensus")

# State data
data("state_laea", package = "tidycensus")


dat <- left_join(dat16, dat12, by = c("GEOID", "variable"))
st_geometry(dat) <- NULL # This drops the geometry and leaves a table

head(dat)

dat <- mutate(dat,
              cat = case_when(
                variable %in% paste0("B27001_0",
                                     c("09","12","37","40")) ~ "pop1834",
                variable %in% paste0("B27001_0",
                                     c("11","14","39","42")) ~ "pop1834ni")) %>%
  filter(!is.na(cat))

# Create long version
dat <- tidyr::gather(dat, year, estimate, c(`2012`, `2016`))

# Group the data by our new categories and sum
dat <- group_by(dat, GEOID, NAME, year, cat) %>%
  summarize(estimate = sum(estimate)) %>%
  ungroup() %>%
  tidyr::spread(cat, estimate) 


head(dat)


dat <- mutate(dat, est = (pop1834ni/pop1834) * 100) %>%
  select(-c(pop1834, pop1834ni)) %>%
  tidyr::spread(year, est) %>%
  mutate(diff = `2016`-`2012`)


head(dat)


datlong <- select(dat, -diff) %>%
  tidyr::gather(year, estimate, c(`2012`, `2016`)) %>%
  group_by(year) %>%
  mutate(med = round(median(estimate, na.rm = TRUE), 1))

ggplot(datlong, aes(estimate)) +
  geom_histogram(fill = "firebrick2", 
                 color = "white", bins = 60) +
  xlab("Uninsured adults ages 18-34 by county (%)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~year, ncol = 1) +
  geom_vline(aes(xintercept = med,
                 group = year), lty = "dashed") +
  geom_text(aes(label = paste("Median = ", med), x = med, y = 55))




d10 <- top_n(dat, 10, diff) %>%
  mutate(type = "Insured population decreased",
         difftemp = diff)

i10 <- top_n(dat, -10, diff) %>%
  mutate(type = "Insured population increased",
         difftemp = abs(diff))

id10 <- bind_rows(list(i10, d10)) %>%
  arrange(desc(difftemp))
ggplot(id10) + 
  geom_col(aes(x = forcats::fct_reorder(NAME, difftemp), 
               y = difftemp, fill = type)) +
  coord_flip() +
  scale_fill_manual(values = c("firebrick2", "cyan4")) +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom",
        legend.title = element_blank()) +
  ggtitle("Counties with the greatest change (+/-) in
    insured population, ages 18-34, 2012-2016") +
  ylab("Difference in % insured (2016 - 2012)") +
  xlab("")

# dat16 is our original geographic object and dat is the tabular data
shp <- dat16 %>%
  filter(variable == "B27001_001") %>% # much faster than using distinct()
  select(GEOID, NAME) %>%
  left_join(dat, by = c("GEOID", "NAME")) %>%
  arrange(GEOID) %>%
  rename(uninsured_2012 = `2012`,
         uninsured_2016 = `2016`,
         uninsured_diff = diff)

head(dat)
# Remove the Aleutians West from shp for display purposes.
# NOTE: this isn't necessary since I'm using the shift_geo
# argument in the get_acs function. However if you're not
# using shift_geo or joining to a different spatial layer
# for the full US you may want to consider removing this 
# record for display purposes.
shp <- filter(shp, GEOID != "02016")







# Define the shape and the layer elements
tm_shape(shp) +
  tm_polygons()
tm_shape(shp) +
  tm_polygons("uninsured_2012")

tm_shape(shp) +
  tm_bubbles("uninsured_2012")


# Create a simple geographic object with one point
dat <- data.frame(c("Empire State Building"), 
                  lat = c(40.748595), 
                  long = c(-73.985718))
sites <- sf::st_as_sf(dat, coords = c("long", "lat"), 
                      crs = 4326, 
                      agr = "identity")

tm_shape(shp) +
  tm_polygons() +
  tm_shape(sites) +
  tm_dots(size = 2)


tm_shape(shp, projection = "wintri") +
  tm_polygons()


# Simplify shapes with tmap function
shpsimp <- simplify_shape(shp, fact = 0.05)



var <- "uninsured_2012"

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              style = "quantile",
              palette = "BuPu") +
  tm_legend(legend.position = c("left", "bottom"))



cuts <- c(0, 10, 20, 30, 40, 100)

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "seq", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "-BuPu", 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


mycols <- c("#f0f4c3", "#dce775", "#cddc39", "#afb42b", "#827717")

tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = mycols, 
              border.col = "white", 
              border.alpha = 0.5) +
  tm_legend(legend.position = c("left", "bottom"))


mymap <- tm_shape(shp, projection = 2163) +
  tm_polygons(var,
              breaks = cuts,
              palette = "BuPu", 
              border.col = "white", 
              border.alpha = 0.5,
              title = "Uninsured (%)") +
  tm_legend(legend.position = c("left", "bottom")) +
  tm_layout(title = "Uninsured adults ages 18-34 by county, 2012",
            title.size = 1.1,
            title.position = c("center", "top"))

mymap


mymap + tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08))



mymap + 
  tm_scale_bar() + 
  tm_compass()


# Add unit argument to tm_shape
tm_shape(shp, projection = 2163, unit = "mi")

# Customize scale bar and north arrow
mymap + tm_scale_bar(color.dark = "gray60",
                     position = c("right", "bottom")) + 
  tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
             color.dark = "gray60", text.color = "gray60",
             position = c("left", "top"))


# Create a state FIPS field 
shp <- mutate(shp, STFIPS = stringr::str_sub(GEOID, 1, 2))

# Aggregate the county layer by state 
states <- shp %>%
  aggregate_map(by = "STFIPS")
# Add the new state layer
mymap + tm_shape(states) +
  tm_borders(col = "black")


createMap <- function(.data, varname, statecol, maptitle){
  
  tm_shape(.data, projection = 2163, unit = "mi") +
    tm_polygons(varname,
                breaks = cuts,
                palette = "BuPu", 
                border.col = "white", 
                border.alpha = 0.5,
                title = "Uninsured (%)") +
    tm_legend(legend.position = c("left", "bottom")) +
    tm_layout(title = maptitle,
              title.size = 1.1,
              title.position = c("center", "top"),
              inner.margins = c(0.06, 0.10, 0.10, 0.08),
              frame = FALSE) +
    tm_scale_bar(color.dark = "gray60",
                 position = c("right", "bottom")) + 
    tm_compass(type = "4star", size = 2.5, fontsize = 0.5,
               color.dark = "gray60", text.color = "gray60",
               position = c("left", "top")) +
    tm_shape(states) +
    tm_borders(col = statecol)
  
}




m1 <- createMap(shp, 
                varname = "uninsured_2012", 
                statecol = "green", 
                maptitle = "Here is title 1")

m2 <- createMap(shp, 
                varname = "uninsured_2016", 
                statecol = "yellow",
                maptitle = "Here is title 2")

tmap_arrange(m1, m2, ncol = 1)





var2 <- c("uninsured_2012", "uninsured_2016")
title2 <- c("Uninsured adults ages 18-34 by county, 2012", 
            "Uninsured adults ages 18-34 by county, 2016")

createMap(shp, 
          varname = var2, 
          statecol = "black", 
          maptitle = title2)



shplong <- select(shp, GEOID, NAME, uninsured_2012, uninsured_2016) %>%
  tidyr::gather(year, estimate, c(uninsured_2012, uninsured_2016)) %>%
  arrange(GEOID)

glimpse(shplong)
## Observations: 6,282
## Variables: 5
## $ GEOID    <chr> "01001", "01001", "01003", "01003", "01005", "01005",...
## $ NAME     <chr> "Autauga County, Alabama", "Autauga County, Alabama",...
## $ year     <chr> "uninsured_2012", "uninsured_2016", "uninsured_2012",...
## $ estimate <dbl> 21.41164, 17.49956, 32.05542, 25.92662, 38.05556, 30....
## $ geometry <sf_geometry [m]> MULTIPOLYGON (((1269841 -13..., MULTIPOLY...
mymap <- tm_shape(shplong, projection = 2163) +
  tm_polygons("estimate",
              breaks = cuts,
              palette = "BuPu", border.col = "white", 
              border.alpha = 0.3,
              title = "Uninsured (%)") +
  tm_facets(by = "year", free.coords = TRUE, ncol = 1) +
  tm_shape(states) +
  tm_borders(col = "black")



# Filter to Texas and create some additional
# fields for mapping. Since we'll be using
tx <- filter(shp, STFIPS == "48") %>%
  mutate(NAME = stringr::str_remove(NAME, ", Texas"),
         `Insured Adults Ages 18-34, 2012-2016` = case_when(
           uninsured_diff < 0  ~ "Insured population increased",
           uninsured_diff > 0  ~ "Insured population decreased",
           uninsured_diff == 0 ~ "Insured population stayed the same"),
         diff2 = round(abs(uninsured_diff), 1),
         popup = ifelse(uninsured_diff != 0,
                        paste0(`Insured Adults Ages 18-34, 2012-2016`, " by ", diff2, "%"), 
                        `Insured Adults Ages 18-34, 2012-2016`),
         diffrad = as.numeric(cut(diff2, c(0, 5, 10, 20, 30, 45), 
                                  right = FALSE)))

# Remove some unnecessary fields
tx <- select(tx, -c(uninsured_2012, uninsured_2016, uninsured_diff, STFIPS))






# Basemap
carto <- "https://cartodb-basemaps-{s}.global.ssl.fastly.net/light_all/{z}/{x}/{y}{r}.png"

# Create a "normal" tmap except we'll add  
# the basemap and the `popup.vars` argument.
# The symbol size of the bubbles will be
# based on the data so use our calculated
# field `diffrad` which will apply sizes
# 1 through 5. Sizes can be further adjusted
# using the `scale` argument.

mymap <- tm_basemap(carto) +  
  tm_shape(tx) +
  tm_borders(col = "azure2") +
  tm_bubbles("diffrad", 
             col = "Insured Adults Ages 18-34, 2012-2016", 
             border.col = "white", 
             scale = 1.5,
             style = "fixed",
             palette = c("coral2", "aquamarine3", "gray"),
             popup.vars = c("County: " = "NAME", "Change: " = "popup"))

tmap_leaflet(mymap)




# Load the raster data
data(land)

# Remove AK and HI
shp_rmAKHI <- filter(shp, !STFIPS %in% c("02", "15"))

# Create a color palette for land cover
# This was taken directly from the tmap documentation
pal20 <- c("#003200", "#3C9600", "#006E00", "#556E19", "#00C800", 
           "#8CBE8C", "#467864", "#B4E664", "#9BC832", "#EBFF64", "#F06432", 
           "#9132E6", "#E664E6", "#9B82E6", "#B4FEF0", "#646464", "#C8C8C8", 
           "#FF0000", "#FFFFFF", "#5ADCDC")

# Map the raster data and assign the bounding box of
# the county layer. Add the county layer on top.
tm_shape(land, bbox = shp_rmAKHI) +
  tm_raster("cover", palette = pal20, alpha = 0.8) +
  tm_shape(shp_rmAKHI) + 
  tm_borders(alpha = 0.4, col = "black") +
  tm_layout(inner.margins = c(0.06, 0.10, 0.10, 0.08)) +
  tm_layout(legend.position = c("left","bottom"))





# INTERACTIVE TERNARY COLOR MAP OF REGIONAL EUROPEAN AGE COMPOSITIONS
# Jonas Schöley
# 2019-02-11

# Init --------------------------------------------------------------------

library(eurostat)      # eurostat data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS
library(tricolore)     # ternary color coding
library(leaflet)       # interactive maps

# Data --------------------------------------------------------------------

# download the data on European population counts
# 2015 by age-group at NUTS-3 level
euro_age <-
  get_eurostat(
    'demo_r_pjanaggr3',
    stringsAsFactors = FALSE
  ) %>%
  # filter NUTS-3, 2015, total population
  filter(
    sex == 'T',
    str_length(geo) == 5,
    year(time) == 2015,
    age %in% c('Y_LT15', 'Y15-64', 'Y_GE65', 'TOTAL')
  ) %>%
  # cleaning
  select(
    age,
    geo,
    values
  ) %>%
  spread(
    age,
    values
  ) %>%
  rename(
    total = TOTAL,
    age65plus = Y_GE65,
    age0to15 = Y_LT15,
    age15to65 = `Y15-64`
  ) %>%
  drop_na()

# download geospatial data for NUTS-3 regions,
# project to crs 3035 and crop to Europe
euro_nuts3_sf <-
  get_eurostat_geospatial(
    output_class = 'sf',
    resolution = '60',
    nuts_level = 3,
    # use NUTS-3 2013 coding
    # for compatability with Eurostat
    # population data
    year = '2013'
  ) %>%
  st_transform(
    crs = 3035
  ) %>%
  # add 0 buffer to avoid self-intersection errors
  st_buffer(0) %>%
  st_crop(
    xmin = 25e5,
    xmax = 75e5,
    ymin = 13.5e5,
    ymax = 54.5e5
  )

# average European age structure in 2015
euro_center <-
  with(
    euro_age,
    c(p_age0to15 = sum(age0to15)/sum(total),
      p_age15to65 = sum(age15to65)/sum(total),
      p_age65plus = sum(age65plus)/sum(total))
  )

# merge geodata and regional age structures and
# add population shares by age and
# differences from European average
euro_age_sf <-
  euro_nuts3_sf %>%
  left_join(
    euro_age,
    by = c('id' = 'geo')
  ) %>%
  mutate(
    p_age0to15 = age0to15/total,
    p_age15to65 = age15to65/total,
    p_age65plus = age65plus/total,
    d_age0to15 = p_age0to15-euro_center['p_age0to15'],
    d_age15to65 = p_age15to65-euro_center['p_age15to65'],
    d_age65plus = p_age65plus-euro_center['p_age65plus']
  )

tric_centered <-
  euro_age_sf %>%
  Tricolore(
    label_as = 'pct_diff',
    crop = TRUE,
    p1 = 'age65plus',
    p2 = 'age15to65',
    p3 = 'age0to15',
    center = rev(euro_center),
    spread = 2.9,
    breaks = Inf,
    contrast = .5,
    lightness = 1,
    chroma = 1,
    hue = 2/12
  )
euro_age_sf$rgb <- tric_centered$rgb

# Interactive map ---------------------------------------------------------

euro_age_sf %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addPolygons(smoothFactor = 0.1, weight = 0,
              fillColor = euro_age_sf$rgb,
              fillOpacity = 1,
              popup =
                paste0(
                  '<b>', euro_age_sf$NUTS_NAME, '</b></br>',
                  '0 to 15: ',
                  formatC(euro_age_sf$p_age0to15*100,
                          digits = 1, format = 'f'), '% (',
                  formatC(euro_age_sf$d_age0to15*100,
                          digits = 1, format = 'f', flag = '+'), '%pt.)</br>',
                  '15-65: ',
                  formatC(euro_age_sf$p_age15to65*100,
                          digits = 1, format = 'f'), '% (',
                  formatC(euro_age_sf$d_age15to65*100,
                          digits = 1, format = 'f', flag = '+'), '%pt.)</br>',
                  '65+: ',
                  formatC(euro_age_sf$p_age65plus*100,
                          digits = 1, format = 'f'), '% (',
                  formatC(euro_age_sf$d_age65plus*100,
                          digits = 1, format = 'f', flag = '+'), '%pt.)</br>'
                )
  )

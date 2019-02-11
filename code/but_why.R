# DESIGN ALTERNATIVES TO "REGIONAL POPULATIONS STRUCTURES AT A GLANCE"
# Jonas Schöley
# 2019-02-11

# Init --------------------------------------------------------------------

library(eurostat)      # eurostat data
library(rnaturalearth) # worldwide map data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS
library(tricolore)     # ternary color coding
library(ggtern)        # ternary ggplots
library(cartogram)     # cartograms

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

# download geospatial data for European,
# Asian and African countries,
# project to crs 3035 and crop to Europe
eura_sf <-
  # download geospatial data for
  # European, Asian and African countries
  ne_countries(
    continent = c('europe', 'asia', 'africa'),
    returnclass = 'sf',
    scale = 50
  ) %>%
  # project to crs 3035
  st_transform(
    crs = 3035
  ) %>%
  # merge into single polygon
  st_union(
    by_feature = FALSE
  ) %>%
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

# Small multiples divergent choropleth ------------------------------------

# divergent choropleth young population
plot_sm1 <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = d_age0to15),
    color = NA
  ) +
  scale_fill_distiller(
    type = 'div',
    palette = 5,
    direction = 1,
    oob = scales::squish,
    limits = c(-0.1, 0.1),
    breaks = c(-0.1, -0.05, 0, 0.05, 0.1),
    labels = c('10%pt. below avg.\nor more',
               '-5%pt.',
               'European avg. (17%)',
               '+5%pt.',
               '10%pt. above avg.\nor more')
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    legend.position = c(0.83, 0.7)
  ) +
  labs(
    subtitle = 'Share of population younger than 15 years\ncompared to European average',
    fill = NULL
  )

# divergent choropleth working age
plot_sm2 <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = d_age15to65),
    color = NA
  ) +
  scale_fill_distiller(
    type = 'div',
    palette = 4,
    direction = 1,
    oob = scales::squish,
    limits = c(-0.1, 0.1),
    breaks = c(-0.1, -0.05, 0, 0.05, 0.1),
    labels = c('10%pt. below avg.\nor more',
               '-5%pt.',
               'European avg. (66%)',
               '+5%pt.',
               '10%pt. above avg.\nor more')
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    legend.position = c(0.83, 0.7)
  ) +
  labs(
    subtitle = 'Share of population ages 15-64\ncompared to European average',
    fill = NULL
  )

# divergent choropleth 65+
plot_sm3 <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = d_age65plus),
    color = NA
  ) +
  scale_fill_distiller(
    type = 'div',
    palette = 1,
    direction = 1,
    oob = scales::squish,
    limits = c(-0.1, 0.1),
    breaks = c(-0.1, -0.05, 0, 0.05, 0.1),
    labels = c('10%pt. below avg.\nor more',
               '-5%pt.',
               'European avg. (17%)',
               '+5%pt.',
               '10%pt. above avg.\nor more')
  ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void() +
  theme(
    legend.position = c(0.83, 0.7)
  ) +
  labs(
    subtitle = 'Share of population older than 65 years\ncompared to European average',
    fill = NULL
  )

plot_div <- cowplot::plot_grid(plot_sm1, plot_sm2, plot_sm3, nrow = 1)

ggsave(
  plot = plot_div,
  filename = 'divergent.pdf',
  path = 'out/',
  width = 19,
  height = 6,
  units = 'cm',
  scale = 2
)

# Deviations from average versus raw shares -------------------------------

# generate centered ternary colors
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
euro_age_sf$centered_col <- tric_centered$rgb

# centered ternary key
plot_key_centered <-
  tric_centered$key +
  labs(caption = '',
       x = '65+',
       y = '15-65',
       z = '0-15'
  ) +
  theme(
    plot.background = element_rect(color = 'grey60')
  )

ggsave(
  plot = plot_key_centered,
  filename = 'key_centered.pdf',
  path = 'out/',
  width = 8,
  height = 8,
  units = 'cm',
  scale = 1.3
)

# centered ternary
plot_centered <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = centered_col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(plot_key_centered),
  #   xmin = 55e5,
  #   xmax = 75e5,
  #   ymin = 37e5,
  #   ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  plot = plot_centered,
  filename = 'centered.pdf',
  path = 'out/',
  width = 12,
  height = 11,
  units = 'cm',
  scale = 1.3
)

# generate non-centered ternary colors
tric_non_centered <-
  Tricolore(
    euro_age_sf,
    label_as = 'pct',
    crop = FALSE,
    p1 = 'age65plus',
    p2 = 'age15to65',
    p3 = 'age0to15',
    center = rep(1/3, 3),
    breaks = Inf,
    hue = 2/12
  )
euro_age_sf$non_centered_col <- tric_non_centered$rgb

plot_key_non_centered <-
  tric_non_centered$key +
  labs(
    x = '65+',
    y = '15-65',
    z = '0-15'
  ) +
  theme(
    plot.background = element_rect(color = 'grey60')
  )

ggsave(
  plot = plot_key_non_centered,
  filename = 'key_non_centered.pdf',
  path = 'out/',
  width = 8,
  height = 8,
  units = 'cm',
  scale = 1.3
)

plot_non_centered <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = non_centered_col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_non_centered),
  #   xmin = 55e5,
  #   xmax = 75e5,
  #   ymin = 37e5,
  #   ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  plot = plot_non_centered,
  filename = 'non_centered.pdf',
  path = 'out/',
  width = 12,
  height = 10,
  units = 'cm',
  scale = 1.3
)

# Discrete versus continuous colors ---------------------------------------

tric_discrete <-
  TricoloreSextant(
    euro_age_sf,
    values = c('#FFDE00', '#7CB474', '#00EBF0', '#AAB7FF', '#FF9FFF', '#CD9289'),
    p1 = 'age65plus',
    p2 = 'age15to65',
    p3 = 'age0to15',
    center = rev(euro_center),
    crop = TRUE
  )
euro_age_sf$discrete <- tric_discrete$rgb

plot_key_discrete <-
  tric_discrete$key +
  labs(
    x = '65+',
    y = '15-65',
    z = '0-15'
  ) +
  theme(
    plot.background = element_rect(color = 'grey60')
  )

ggsave(
  plot = plot_key_discrete,
  filename = 'key_discrete.pdf',
  path = 'out/',
  width = 8,
  height = 8,
  units = 'cm',
  scale = 1.3
)

plot_discrete <-
  euro_age_sf %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = discrete),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_discrete),
  #   xmin = 55e5,
  #   xmax = 75e5,
  #   ymin = 37e5,
  #   ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  plot = plot_discrete,
  filename = 'discrete.pdf',
  path = 'out/',
  width = 12,
  height = 10,
  units = 'cm',
  scale = 1.3
)

# Cartogram ---------------------------------------------------------------

# download high-res (for cartogram) geospatial data
# for NUTS-3 regions,
# project to crs 3035 and crop to Europe
euro_nuts3_sf_hd <-
  get_eurostat_geospatial(
    output_class = 'sf',
    resolution = '10',
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

# merge geodata and regional age structures and
# add population shares by age and
# differences from European average
euro_age_sf_hd <-
  euro_nuts3_sf_hd %>%
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

# generate cartogram based on total population size
# in NUTS-3 region
euro_age_sf_carto <-
  euro_age_sf_hd %>%
  st_cast("MULTIPOLYGON") %>% 
  cartogram_cont(weight = 'total')

# color mixing
tric_carto <-
  Tricolore(
    euro_age_sf_carto,
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
euro_age_sf_carto$centered_col <- tric_carto$rgb

save(euro_age_sf_carto, file = 'out/euro_age_sf_carto.RData')

# key
key_carto <-
  tric_carto$key +
  labs(caption = '',
       x = '65+', y = '15-65', z = '0-15') +
  theme(plot.background = element_rect(color = 'grey60'))

plot_carto <-
  euro_age_sf_carto %>%
  ggplot() +
  geom_sf(
    data = eura_sf,
    color = NA,
    fill = 'grey95'
  ) +
  geom_sf(
    aes(fill = centered_col),
    color = NA
  ) +
  scale_fill_identity() +
  # annotation_custom(
  #   ggplotGrob(key_centered),
  #   xmin = 55e5,
  #   xmax = 75e5,
  #   ymin = 37e5,
  #   ymax = 54e5
  # ) +
  coord_sf(
    expand = FALSE,
    datum = NA
  ) +
  theme_void()

ggsave(
  plot = plot_carto,
  filename = 'carto.pdf',
  path = 'out/',
  width = 12,
  height = 10,
  units = 'cm',
  scale = 1.3
)

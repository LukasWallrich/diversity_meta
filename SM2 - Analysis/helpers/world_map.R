# Code based on https://github.com/nilsreimer/ironic-effects-meta-analysis
# Thanks to Nils Reimer!

# Notes -------------------------------------------------------------------

#########################################################################
# Thanks to Claus Wilke for implementing the "Interrupted Goode         #
# homolosine" projection in R.                                          #
#########################################################################

create_world_map <- function(countries, counts, count_label, scale_start = 1) {

  # Library -----------------------------------------------------------------

  # Prepare map -------------------------------------------------------------

  # Load map
  world_sf <- st_as_sf(getMap(resolution = "low"))

  # Prepare map projection
  crs_goode <- "+proj=igh"
  goode_outline <- list(cbind(
    longs <- c(
      rep(180, 181),
      rep(c(80.01, 79.99), each = 91),
      rep(c(-19.99, -20.01), each = 91),
      rep(c(-99.99, -100.01), each = 91),
      rep(-180, 181),
      rep(c(-40.01, -39.99), each = 91),
      180
    ),
    lats <- c(
      90:-90,
      -90:0, 0:-90,
      -90:0, 0:-90,
      -90:0, 0:-90,
      -90:90,
      90:0, 0:90,
      90
    )
  )) %>%
    st_polygon() %>%
    st_sfc(
      crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    )
  goode_outline <- st_transform(goode_outline, crs = crs_goode)
  xlim <- st_bbox(goode_outline)[c("xmin", "xmax")]*1.1
  ylim <- st_bbox(goode_outline)[c("ymin", "ymax")]*1.1
  goode_encl_rect <-
    list(
      cbind(
        c(xlim[1], xlim[2], xlim[2], xlim[1], xlim[1]),
        c(ylim[1], ylim[1], ylim[2], ylim[2], ylim[1])
      )
    ) %>%
    st_polygon() %>%
    st_sfc(crs = crs_goode)
  goode_without <- st_difference(goode_encl_rect, goode_outline)


  # Prepare data ------------------------------------------------------------

  dm <- tibble(n = counts, country = countries) %>%
    mutate(id = row_number())

  if (length(setdiff(countries, c(as.character(world_sf$ADMIN), "Hong Kong", "Singapore")) > 0)) {
     message("Following countries cannot be matched: ", setdiff(countries, c(as.character(world_sf$ADMIN), "Hong Kong", "Singapore")), ". Rename them to match st_as_sf(getMap(resolution = 'low'))$ADMIN")
  }
  # Join with map data
  world_sf$N <- dm %>%
    filter(!(country %in% c("Hong Kong", "Singapore"))) %>%
    group_by(country) %>%
    summarize(I = n(), J = n_distinct(id), N = sum(n)) %>%
    ungroup() %>%
    mutate(
      country = factor(country, levels(world_sf$ADMIN))
    ) %>%
    left_join(
      tibble(country = world_sf$ADMIN),
      .,
      by = "country"
    ) %>%
    pull(N)

  # Prepare map data
  cities <- dm %>%
    filter(country %in% c("Hong Kong", "Singapore")) %>%
    group_by(country) %>%
    summarize(I = n(), J = n_distinct(id), N = sum(n)) %>%
    left_join(
      tribble(
        ~country,     ~lat,      ~long,
        "Hong Kong", 22.30271, 114.177216,
        "Singapore", 1.283333, 103.833333
      ),
      by = "country"
    ) %>%
    st_as_sf(coords = c("long", "lat"), crs = st_crs(world_sf))


  # Visualize ---------------------------------------------------------------

  p <- ggplot(world_sf) +
    geom_sf(
      aes(fill = N),
      color = "black",
      size = 0.5/.pt,
      alpha = 1
    ) +
    geom_sf(
      data = cities,
      colour = "black",
      size = 1.5
    ) +
    geom_sf(
      data = cities,
      aes(colour = N),
      size = 1
    ) +
    geom_sf(
      data = goode_without,
      color = NA,
      fill = "white"
    ) +
    geom_sf(
      data = goode_outline,
      color = "black",
      fill = NA,
      size = 0.5/.pt
    ) +
    scale_colour_viridis_c(
      option = "B",
      limits = c(scale_start, max(world_sf$N, na.rm = TRUE)),
      direction = -1,
      breaks = c(10^(0:ceiling(log10(max(world_sf$N, na.rm = TRUE))))),
      labels = f_comma(c(10^(0:ceiling(log10(max(world_sf$N, na.rm = TRUE)))))),
      trans = "log",
      na.value = "grey92",
      guide = NULL
    ) +
    scale_fill_viridis_c(
      option = "B",
      limits = c(scale_start, max(world_sf$N, na.rm = TRUE)),
      direction = -1,
      breaks = c(10^(0:ceiling(log10(max(world_sf$N, na.rm = TRUE))))),
      labels = f_comma(c(10^(0:ceiling(log10(max(world_sf$N, na.rm = TRUE)))))),
      trans = "log",
      na.value = "grey92"
    ) +
    guides(
      fill = guide_colourbar(
        title = paste("Number of", count_label),
        title.position = "top",
        label.position = "bottom",
        label.hjust = 0,
        ticks.linewidth = 1,
        draw.ulim = TRUE,
        barheight = unit(10, "pt"),
        barwidth = unit(2, "in")
      )
    ) +
    coord_sf(
      crs = crs_goode,
      xlim = 0.95 * xlim,
      ylim = 0.95 * ylim,
      expand = FALSE
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.justification = c(0, 0),
      panel.background = element_rect(fill = "#56B4E950", color = "white", linewidth = 1),
      panel.grid = element_blank()
    )

  return(p)

}

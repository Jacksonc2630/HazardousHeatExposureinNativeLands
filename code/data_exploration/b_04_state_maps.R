rm(list = ls())

# Load packages
library(sf)
library(ggplot2)
library(dplyr)
library(stringr)
library(here)

# Set up folder paths
project.folder <- paste0(here::here(), "/")
source(paste0(project.folder, "create_folder_structure.R"))
source(paste0(functions.folder, "script_initiate.R"))

# Output directory
output_dir <- paste0(figures.folder, "states_individual/")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load data
shapefile.data <- readRDS(paste0(project.folder, "data/processed/shapefile_data_all.rds"))
shapefile.data$GEOID <- as.character(shapefile.data$GEOID)

# Load territory-state intersections
territory_intersections <- readRDS(
  paste0(project.folder, "data/processed/territory_state_intersections.rds")
)
territory_intersections$GEOID <- as.character(territory_intersections$GEOID)

# Load states shapefile
states_sf <- st_read(
  paste0(project.folder, "data/shapefiles/states/states.shp"),
  quiet = TRUE
)

# Load native territories
native_sf <- st_read(
  paste0(project.folder, "data/shapefiles/native_boundaries/tl_2020_us_aitsn.shp"),
  quiet = TRUE
)

native_sf$GEOID <- str_pad(as.character(native_sf$GEOID), width = 7, side = "left", pad = "0")
proj_aea <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
states_sf <- st_transform(states_sf, proj_aea)
native_sf <- st_transform(native_sf, proj_aea)
states_sf <- states_sf %>% filter(STATE_FIPS %in% states_included)
native_sf <- native_sf %>%
  left_join(
    shapefile.data %>% select(GEOID, STATE_NAME, POP_2020),
    by = "GEOID"
  )

states_with_territories <- shapefile.data %>%
  filter(!is.na(STATE_NAME), !is.na(POP_2020), POP_2020 > 0) %>%
  distinct(STATE_NAME) %>%
  pull(STATE_NAME) %>%
  sort()

min_cross_border_proportion <- 0.05 

cross_border_by_state <- territory_intersections %>%
  left_join(
    shapefile.data %>% select(GEOID, STATE_NAME) %>% rename(PRIMARY_STATE = STATE_NAME),
    by = "GEOID"
  ) %>%
  filter(STATE_NAME != PRIMARY_STATE,
         !is.na(PRIMARY_STATE),
         area_proportion >= min_cross_border_proportion) %>%
  select(GEOID, STATE_NAME, area_proportion, PRIMARY_STATE)

if (nrow(cross_border_by_state) > 0) {
  message("\nCross-border territories found:")
  cross_border_print <- cross_border_by_state %>%
    arrange(STATE_NAME, desc(area_proportion)) %>%
    head(40) %>%
    mutate(area_proportion = as.numeric(area_proportion))
  print(as.data.frame(cross_border_print))
}

message("\nGenerating maps for ", length(states_with_territories), " states...")

# Generate one map per state

for (state_name in states_with_territories) {

  message("  Plotting: ", state_name)
  
  state_boundary <- states_sf %>% filter(STATE_NAME == state_name)

  if (nrow(state_boundary) == 0) {
    message("    WARNING: No state boundary found for ", state_name, " - skipping")
    next
  }

  primary_territories <- native_sf %>%
    filter(STATE_NAME == state_name, !is.na(POP_2020), POP_2020 > 0)

  cross_border_geoids <- cross_border_by_state %>%
    filter(STATE_NAME == state_name) %>%
    pull(GEOID)

  cross_border_territories <- native_sf %>%
    filter(GEOID %in% cross_border_geoids, !is.na(POP_2020), POP_2020 > 0)

  all_territories <- bind_rows(primary_territories, cross_border_territories) %>%
    distinct(GEOID, .keep_all = TRUE)

  if (nrow(all_territories) == 0) {
    message("    No territories with population for ", state_name, " - skipping")
    next
  }

  n_primary <- nrow(primary_territories)
  n_cross <- nrow(cross_border_territories)

  combined_bbox <- st_bbox(
    st_union(
      st_geometry(state_boundary),
      st_union(st_geometry(all_territories))
    )
  )

  x_range <- combined_bbox["xmax"] - combined_bbox["xmin"]
  y_range <- combined_bbox["ymax"] - combined_bbox["ymin"]
  pad <- max(x_range, y_range) * 0.05  # 5% padding

  view_box <- st_as_sfc(st_bbox(c(
    xmin = combined_bbox["xmin"] - pad,
    ymin = combined_bbox["ymin"] - pad,
    xmax = combined_bbox["xmax"] + pad,
    ymax = combined_bbox["ymax"] + pad
  ), crs = st_crs(states_sf)))

  neighbor_states <- states_sf %>%
    filter(STATE_NAME != state_name) %>%
    filter(lengths(st_intersects(., view_box)) > 0)

  # Build subtitle
  subtitle_text <- paste0(n_primary, " territories shown")
  if (n_cross > 0) {
    subtitle_text <- paste0(subtitle_text, " + ", n_cross, " cross-border")
  }

  # Create the plot
  p <- ggplot() +
    geom_sf(
      data = neighbor_states,
      fill = alpha("grey90", 0.5),
      color = alpha("grey50", 0.8),
      linewidth = 0.4,
      linetype = "dashed"
    ) +
    geom_sf(
      data = state_boundary,
      fill = "white",
      color = "black",
      linewidth = 0.8
    ) +
    geom_sf(
      data = all_territories,
      fill = alpha("red", 0.15),
      color = "red",
      linewidth = 0.5
    ) +
    coord_sf(
      xlim = c(combined_bbox["xmin"] - pad, combined_bbox["xmax"] + pad),
      ylim = c(combined_bbox["ymin"] - pad, combined_bbox["ymax"] + pad),
      expand = FALSE
    ) +
    labs(
      title = paste0(state_name, " - Native Territories"),
      subtitle = subtitle_text
    ) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5, size = 10, color = "grey40"),
      plot.margin = margin(10, 10, 10, 10)
    )

  # Save PNG
  filename <- paste0(
    gsub(" ", "_", state_name),
    "_native_territories_zoomed.png"
  )

  ggsave(
    filename = paste0(output_dir, filename),
    plot = p,
    width = 10,
    height = 8,
    dpi = 300,
    bg = "white"
  )

  if (n_cross > 0) {
    message("    ", n_primary, " primary + ", n_cross, " cross-border territories")
  }
}

message("Total maps generated: ", length(states_with_territories))

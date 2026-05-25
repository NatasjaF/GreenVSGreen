#----------------------------------------------------------------------------------------------
# Reconciling the green-versus-green dilemma in Greece’s renewable electricity transition 
#----------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(forcats)
library(scales)
library(cowplot)


# ============================================================
# Figure 1 — HERMES comparison among all six pathways
# Panels:
# (a) Annual installed capacity [GW]
# (b) Annual electricity generation [TWh]
# (c) Cumulative system cost increases over NECP baseline by 2050 [%]
# ============================================================

# ------------------------------------------------------------
# 1. File and sheets
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"

sheet_capacity   <- "capacity"
sheet_generation <- "generation"
sheet_costs      <- "costs"

# ------------------------------------------------------------
# 2. Scenario, year and technology order
# ------------------------------------------------------------

year_order <- c(2025, 2030, 2035, 2040, 2045, 2050)

pathway_order <- c(
  "NECP",
  "NECP_N2000",
  "NECP_BIOD",
  "NECP+",
  "NECP+_N2000",
  "NECP+_BIOD"
)

pathway_labels <- c(
  "NECP"        = "NECP",
  "NECP_N2000"  = "NECP-N2000",
  "NECP_BIOD"   = "NECP-BIOD",
  "NECP+"       = "NECP+",
  "NECP+_N2000" = "NECP+-N2000",
  "NECP+_BIOD"  = "NECP+-BIOD"
)

pathway_labels_short <- c(
  "NECP"        = "NECP",
  "NECP_N2000"  = "N2000",
  "NECP_BIOD"   = "BIOD",
  "NECP+"       = "NECP+",
  "NECP+_N2000" = "+N2000",
  "NECP+_BIOD"  = "+BIOD"
)

technology_order <- c(
  "Lignite",
  "Natural Gas",
  "Oil",
  "Biomass",
  "Solar Utility",
  "Solar Rooftop",
  "Onshore",
  "Offshore",
  "Hydro",
  "Batteries",
  "Pumped Hydro"
)

technology_order_gen <- c(
  "Lignite",
  "Natural Gas",
  "Oil",
  "Biomass",
  "Solar Utility",
  "Solar Rooftop",
  "Onshore",
  "Offshore",
  "Hydro",
  "Net Imports"
)

tech_cols <- c(
  "Lignite"       = "#4D4D4D",
  "Natural Gas"   = "#F0E442",
  "Oil"           = "#9E2D1E",
  "Biomass"       = "#009E73",
  "Solar Utility" = "#F4A582",
  "Solar Rooftop" = "#FDBF6F",
  "Onshore"       = "#56B4E9",
  "Offshore"      = "#0072B2",
  "Hydro"         = "#80CDC1",
  "Batteries"     = "#CC79A7",
  "Pumped Hydro"  = "#984EA3",
  "Net Imports"   = "#999999"
)

pathway_cols <- c(
  "NECP"        = "#4D4D4D",
  "NECP_N2000"  = "#F0E442",
  "NECP_BIOD"   = "#CC79A7",
  "NECP+"       = "#0099C7",
  "NECP+_N2000" = "#A6A6A6",
  "NECP+_BIOD"  = "#4DAF2A"
)

# ------------------------------------------------------------
# 3. Theme
# ------------------------------------------------------------

theme_nature <- function(base_size = 10, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size + 1, colour = "black"),
      axis.line = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      legend.title = element_blank(),
      plot.margin = margin(5, 5, 5, 5, "mm")
    )
}

# ------------------------------------------------------------
# 4. Data-reading functions
# ------------------------------------------------------------

read_energy_sheet <- function(file, sheet, value_name) {
  
  raw <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE
  )
  
  years <- raw[1, -1] |>
    unlist(use.names = FALSE) |>
    as.integer()
  
  pathways <- raw[2, -1] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  values <- raw[-c(1, 2), ]
  
  names(values) <- c(
    "Technology",
    paste(years, pathways, sep = "__")
  )
  
  values |>
    pivot_longer(
      cols = -Technology,
      names_to = "key",
      values_to = value_name
    ) |>
    separate(
      key,
      into = c("Year", "Pathway"),
      sep = "__",
      convert = TRUE
    ) |>
    mutate(
      Year = as.integer(Year),
      Pathway = str_squish(as.character(Pathway)),
      Technology = str_squish(as.character(Technology)),
      value = as.numeric(.data[[value_name]])
    ) |>
    select(Year, Pathway, Technology, value) |>
    filter(
      Year %in% year_order,
      Pathway %in% pathway_order,
      !is.na(value)
    )
}

read_cost_sheet <- function(file, sheet) {
  
  raw <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE
  )
  
  pathways <- raw[2, -1] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  values <- raw[3, -1] |>
    unlist(use.names = FALSE) |>
    as.numeric()
  
  tibble(
    Pathway = pathways,
    value = values
  ) |>
    mutate(
      value = if_else(Pathway == "NECP" & is.na(value), 0, value),
      Pathway = factor(Pathway, levels = pathway_order)
    ) |>
    filter(!is.na(Pathway), !is.na(value))
}

# ------------------------------------------------------------
# 5. X-axis structure for panels A and B
# ------------------------------------------------------------

x_axis_info <- tidyr::crossing(
  Year = year_order,
  Pathway = factor(pathway_order, levels = pathway_order)
) |>
  arrange(Year, match(as.character(Pathway), pathway_order)) |>
  mutate(
    Pathway = as.character(Pathway),
    x = row_number(),
    scenario_lab = pathway_labels_short[Pathway]
  )

scenario_breaks <- x_axis_info$x
scenario_labels <- x_axis_info$scenario_lab

year_label_df <- x_axis_info |>
  group_by(Year) |>
  summarise(
    x = mean(x),
    .groups = "drop"
  )

year_separators <- seq(
  from = length(pathway_order) + 0.5,
  by = length(pathway_order),
  length.out = length(year_order) - 1
)


# ------------------------------------------------------------
# 6. Manual legend function
# ------------------------------------------------------------

make_manual_legend <- function(
    items,
    colours,
    ncol = 3,
    text_size = 5.0,
    item_width = 1.05,
    key_width = 0.14,
    key_height = 0.13,
    text_gap = 0.05,
    row_spacing = 0.38,
    center_shift = 0
) {
  
  legend_df <- tibble(
    item = items,
    colour = as.character(unname(colours[items])),
    item_id = seq_along(items)
  ) |>
    mutate(
      col = ((item_id - 1) %% ncol),
      row = floor((item_id - 1) / ncol),
      row = max(row) - row,
      x_key_left  = col * item_width,
      x_key_right = x_key_left + key_width,
      x_text      = x_key_right + text_gap,
      y = row * row_spacing
    )
  
  content_left  <- min(legend_df$x_key_left)
  content_right <- max(legend_df$x_text) + 0.80
  content_width <- content_right - content_left
  
  plot_width <- content_width + 0.30
  x_offset <- (plot_width - content_width) / 2 - content_left + center_shift
  
  legend_df <- legend_df |>
    mutate(
      x_key_left  = x_key_left + x_offset,
      x_key_right = x_key_right + x_offset,
      x_text      = x_text + x_offset
    )
  
  ggplot(legend_df) +
    geom_rect(
      aes(
        xmin = x_key_left,
        xmax = x_key_right,
        ymin = y - key_height / 2,
        ymax = y + key_height / 2
      ),
      fill = legend_df$colour,
      colour = NA
    ) +
    geom_text(
      aes(x = x_text, y = y, label = item),
      hjust = 0,
      vjust = 0.5,
      size = text_size,
      colour = "black"
    ) +
    coord_cartesian(
      xlim = c(0, plot_width),
      ylim = c(-0.30, max(legend_df$y) + 0.30),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
}

# ------------------------------------------------------------
# 7. Year-strip function
# ------------------------------------------------------------

make_year_strip <- function(
    text_size = 4.6
) {
  ggplot(year_label_df, aes(x = x, y = 1, label = Year)) +
    geom_text(
      size = text_size,
      fontface = "bold",
      colour = "black"
    ) +
    scale_x_continuous(
      limits = c(0.5, max(scenario_breaks) + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0.8, 1.2),
      expand = c(0, 0)
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 5, 0, 5, "mm")
    )
}

# ------------------------------------------------------------
# 8. Read data
# ------------------------------------------------------------

capacity <- read_energy_sheet(
  file = xlsx_file,
  sheet = sheet_capacity,
  value_name = "Capacity_GW"
)

generation <- read_energy_sheet(
  file = xlsx_file,
  sheet = sheet_generation,
  value_name = "Generation_TWh"
)

costs <- read_cost_sheet(
  file = xlsx_file,
  sheet = sheet_costs
)

# ============================================================
# Panel A — annual installed capacity
# ============================================================

capacity_fig <- capacity |>
  filter(Technology != "Net Imports") |>
  mutate(
    Pathway = as.character(Pathway),
    Technology = factor(Technology, levels = technology_order)
  ) |>
  left_join(
    x_axis_info |> select(Year, Pathway, x),
    by = c("Year", "Pathway")
  )

p_a_core <- ggplot(capacity_fig, aes(x = x, y = value, fill = Technology)) +
  geom_vline(
    xintercept = year_separators,
    linewidth = 0.25,
    linetype = "dotted",
    colour = "grey70"
  ) +
  geom_col(
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order],
    breaks = technology_order,
    drop = TRUE
  ) +
  scale_x_continuous(
    breaks = scenario_breaks,
    labels = scenario_labels,
    limits = c(0.5, max(scenario_breaks) + 0.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, by = 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Installed capacity [GW]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 15),
    axis.text.x  = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 0, 5, "mm")
  )

p_a_years <- make_year_strip(
  text_size = 4.8
)

p_a_legend <- make_manual_legend(
  items = technology_order,
  colours = tech_cols,
  ncol = 4,
  text_size = 5.0,
  item_width = 1.05,
  key_width = 0.14,
  key_height = 0.13,
  text_gap = 0.05,
  row_spacing = 0.38,
  center_shift = 0
)

p_a_legend_centered <- cowplot::plot_grid(
  NULL,
  p_a_legend,
  NULL,
  ncol = 3,
  rel_widths = c(0.08, 0.84, 0.08)
)

p_a <- cowplot::plot_grid(
  p_a_core,
  p_a_years,
  p_a_legend_centered,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 0.07, 0.30)
)

p_a

# ------------------------------------------------------------
# Export panel A
# ------------------------------------------------------------

ggsave(
  filename = "Figure1a_installed_capacity_panel.pdf",
  plot = p_a,
  width = 100,
  height = 125,
  units = "mm",
  device = cairo_pdf,
  limitsize = FALSE,
  bg = "white"
)

ggsave(
  filename = "Figure1a_installed_capacity_panel.png",
  plot = p_a,
  width = 100,
  height = 125,
  units = "mm",
  dpi = 600,
  limitsize = FALSE,
  bg = "white"
)


# ============================================================
# Panel B — annual electricity generation
# Same style and parameters as Panel A
# ============================================================

generation_fig <- generation |>
  filter(Technology %in% technology_order_gen) |>
  mutate(
    Pathway = as.character(Pathway),
    Technology = factor(Technology, levels = technology_order_gen)
  ) |>
  left_join(
    x_axis_info |> select(Year, Pathway, x),
    by = c("Year", "Pathway")
  ) |>
  arrange(Year, Pathway, Technology)

generation_pos <- generation_fig |>
  filter(value >= 0)

generation_neg <- generation_fig |>
  filter(value < 0)

p_b_core <- ggplot() +
  geom_vline(
    xintercept = year_separators,
    linewidth = 0.25,
    linetype = "dotted",
    colour = "grey70"
  ) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_col(
    data = generation_pos,
    aes(x = x, y = value, fill = Technology),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_col(
    data = generation_neg,
    aes(x = x, y = value, fill = Technology),
    width = 0.78,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order_gen],
    breaks = technology_order_gen,
    drop = FALSE
  ) +
  scale_x_continuous(
    breaks = scenario_breaks,
    labels = scenario_labels,
    limits = c(0.5, max(scenario_breaks) + 0.5),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-20, 160),
    breaks = seq(-20, 160, by = 20),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 18, margin = margin (r=10)),
    axis.text.y  = element_text(size = 15),
    axis.text.x  = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 0, 5, "mm")
  )

p_b_years <- make_year_strip(
  text_size = 4.8
)

p_b_legend <- make_manual_legend(
  items = technology_order_gen,
  colours = tech_cols,
  ncol = 4,
  text_size = 5.0,
  item_width = 1.05,
  key_width = 0.14,
  key_height = 0.13,
  text_gap = 0.05,
  row_spacing = 0.38,
  center_shift = 0
)

p_b_legend_centered <- cowplot::plot_grid(
  NULL,
  p_b_legend,
  NULL,
  ncol = 3,
  rel_widths = c(0.08, 0.84, 0.08)
)

p_b <- cowplot::plot_grid(
  p_b_core,
  p_b_years,
  p_b_legend_centered,
  ncol = 1,
  align = "v",
  rel_heights = c(1, 0.07, 0.30)
)

p_b

# ------------------------------------------------------------
# Panel C — cumulative system cost increase by 2050
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_costs <- "costs"

read_cost_sheet <- function(file, sheet) {
  
  raw <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE
  )
  
  pathways <- raw[2, -1] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  values <- raw[3, -1] |>
    unlist(use.names = FALSE) |>
    as.numeric()
  
  tibble(
    Pathway = pathways,
    value = values
  ) |>
    mutate(
      # NECP is the baseline. If the Excel value is blank, set it to zero.
      value = if_else(Pathway == "NECP" & is.na(value), 0, value),
      Pathway = factor(Pathway, levels = pathway_order)
    ) |>
    filter(
      !is.na(Pathway),
      !is.na(value)
    )
}

costs_fig <- read_cost_sheet(
  file = xlsx_file,
  sheet = sheet_costs
)

costs_fig

# ------------------------------------------------------------
# Plot Figure 1c
# ------------------------------------------------------------

pathway_order_c <- pathway_order[pathway_order != "NECP"]

costs_fig_c <- costs_fig |>
  filter(Pathway != "NECP") |>
  mutate(
    Pathway = factor(as.character(Pathway), levels = pathway_order_c)
  )


p_c <- ggplot(costs_fig_c, aes(x = Pathway, y = value, fill = Pathway)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "black"
  ) +
  geom_col(
    width = 0.50,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = pathway_cols[pathway_order_c],
    breaks = pathway_order_c,
    drop = TRUE
  ) +
  scale_x_discrete(
    labels = pathway_labels[pathway_order_c]
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    expand = expansion(mult = c(0, 0.08)),
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    x = NULL,
    y = "Cumulative system cost increase by 2050 [%]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 15),
    axis.text.x  = element_text(
      size = 11.5,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    ),
    plot.margin = margin(5, 5, 5, 5, "mm")
  )

p_c


# ============================================================
# Figure 2 — Differences between the NECP and NECP+_BIOD pathways in 2050. 
# Panels:
# (a) Total installed capacity in 2050
# (b) electricity generation in 2050
# (c) cumulative costs per category by 2050
# ============================================================

# ------------------------------------------------------------
# Figure 2a — Difference in total installed capacity in 2050
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f2a <- "Total installed capacity"

# ------------------------------------------------------------
# Technology order and colours
# ------------------------------------------------------------

technology_order_f2a <- c(
  "Lignite",
  "Natural Gas",
  "Oil",
  "Biomass",
  "Solar Utility",
  "Solar Rooftop",
  "Onshore",
  "Offshore",
  "Hydro",
  "Batteries",
  "Pumped Hydro"
)

tech_cols <- c(
  "Lignite"       = "#4D4D4D",
  "Natural Gas"   = "#F0E442",
  "Oil"           = "#9E2D1E",
  "Biomass"       = "#009E73",
  "Solar Utility" = "#F4A582",
  "Solar Rooftop" = "#FDBF6F",
  "Onshore"       = "#56B4E9",
  "Offshore"      = "#0072B2",
  "Hydro"         = "#80CDC1",
  "Batteries"     = "#CC79A7",
  "Pumped Hydro"  = "#984EA3"
)

# ------------------------------------------------------------
# Theme
# ------------------------------------------------------------

theme_nature <- function(base_size = 10, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size + 1, colour = "black"),
      axis.line.x = element_line(linewidth = 0.35, colour = "grey70"),
      axis.line.y = element_line(linewidth = 0.35, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size + 1),
      legend.key.size = unit(4.5, "mm"),
      legend.spacing.x = unit(1.5, "mm"),
      plot.margin = margin(8, 10, 8, 10, "pt")
    )
}


# ------------------------------------------------------------
# Read and clean data
# ------------------------------------------------------------

f2a_raw <- read_excel(
  xlsx_file,
  sheet = sheet_f2a,
  col_names = FALSE
)

f2a <- f2a_raw |>
  slice(3:n()) |>
  transmute(
    Technology = str_squish(as.character(...1)),
    value = as.numeric(...2)
  ) |>
  filter(
    Technology %in% technology_order_f2a,
    !is.na(value)
  ) |>
  mutate(
    Technology = factor(Technology, levels = technology_order_f2a)
  )

# ------------------------------------------------------------
# Plot Figure 2a — narrow/tall version
# ------------------------------------------------------------

p_2a <- ggplot(f2a, aes(x = Technology, y = value, fill = Technology)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_col(
    width = 0.44,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order_f2a],
    breaks = technology_order_f2a,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(-0.20, 0.25, by = 0.05),
    limits = c(-0.20, 0.25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Difference in installed capacity in 2050 [%]"
  ) +
  theme_nature(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.title.y = element_text(size = 22, margin = margin(r = 8)),
    axis.text.y  = element_text(size = 19),
    
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.size = unit(3.2, "mm"),
    legend.spacing.x = unit(1.0, "mm"),
    legend.spacing.y = unit(0.6, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    
    plot.margin = margin(5, 4, 5, 4, "mm")
  ) +
  guides(
    fill = guide_legend(
      ncol = 4,
      byrow = TRUE,
      keyheight = unit(3.2, "mm"),
      keywidth  = unit(3.2, "mm")
    )
  )

p_2a

# ------------------------------------------------------------
# Figure 2b — Difference in electricity gen
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f2b <- "Electricity generation"

technology_order_f2b <- c(
  "Lignite",
  "Natural Gas",
  "Oil",
  "Biomass",
  "Solar Utility",
  "Solar Rooftop",
  "Onshore",
  "Offshore",
  "Hydro",
  "Net Imports"
)

tech_cols <- c(
  "Lignite"       = "#4D4D4D",
  "Natural Gas"   = "#F0E442",
  "Oil"           = "#9E2D1E",
  "Biomass"       = "#009E73",
  "Solar Utility" = "#F4A582",
  "Solar Rooftop" = "#FDBF6F",
  "Onshore"       = "#56B4E9",
  "Offshore"      = "#0072B2",
  "Hydro"         = "#80CDC1",
  "Net Imports"   = "#999999"
)

f2b_raw <- read_excel(
  xlsx_file,
  sheet = sheet_f2b,
  col_names = FALSE
)

f2b <- f2b_raw |>
  slice(3:n()) |>
  transmute(
    Technology = str_squish(as.character(...1)),
    value = as.numeric(...2)
  ) |>
  filter(
    Technology %in% technology_order_f2b,
    !is.na(value)
  ) |>
  mutate(
    Technology = factor(Technology, levels = technology_order_f2b)
  )

f2b

#Plot
y_min_2b <- floor(min(f2b$value, na.rm = TRUE) / 0.05) * 0.05
y_max_2b <- ceiling(max(f2b$value, na.rm = TRUE) / 0.05) * 0.05


p_2b <- ggplot(f2b, aes(x = Technology, y = value, fill = Technology)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_col(
    width = 0.44,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order_f2b],
    breaks = technology_order_f2b,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(-0.20, 0.25, by = 0.05),
    limits = c(-0.20, 0.25),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Difference in electricity generation in 2050 [%]"
  ) +
  theme_nature(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.title.y = element_text(size = 22, margin = margin(r = 8)),
    axis.text.y  = element_text(size = 19),
    
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.size = unit(3.2, "mm"),
    legend.spacing.x = unit(1.0, "mm"),
    legend.spacing.y = unit(0.6, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    
    plot.margin = margin(5, 4, 5, 4, "mm")
  ) +
  guides(
    fill = guide_legend(
      ncol = 4,
      byrow = TRUE,
      keyheight = unit(3.2, "mm"),
      keywidth  = unit(3.2, "mm")
    )
  )

p_2b

# ------------------------------------------------------------
#Figure 2c - Difference in cumulative costs
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f2c <- "Cumulative costs"

cost_order_f2c <- c(
  "Annualised Capital Costs",
  "Fixed Operating Costs",
  "Non-fuel Variable Operating Costs",
  "Fuels Costs",
  "Emissions Costs",
  "Curtailment Costs",
  "Cross-Border Electricity Exchange Costs"
)

cost_cols <- c(
  "Annualised Capital Costs"                 = "#4E79A7",
  "Fixed Operating Costs"                    = "#76B7B2",
  "Non-fuel Variable Operating Costs"        = "#F28E2B",
  "Fuels Costs"                              = "#E15759",
  "Emissions Costs"                          = "#4D4D4D",
  "Curtailment Costs"                        = "#B07AA1",
  "Cross-Border Electricity Exchange Costs"  = "#59A14F"
)

# ------------------------------------------------------------
# Read and clean data
# ------------------------------------------------------------

f2c_raw <- read_excel(
  xlsx_file,
  sheet = sheet_f2c,
  col_names = FALSE
)

f2c <- f2c_raw |>
  slice(3:n()) |>
  transmute(
    Category = str_squish(as.character(...1)),
    value = as.numeric(...2)
  ) |>
  filter(
    Category %in% cost_order_f2c,
    !is.na(value)
  ) |>
  mutate(
    Category = factor(Category, levels = cost_order_f2c)
  )

f2c

# ------------------------------------------------------------
# Plot Figure 2c
# ------------------------------------------------------------

y_min_2c <- floor(min(f2c$value, na.rm = TRUE) / 0.005) * 0.005
y_max_2c <- ceiling(max(f2c$value, na.rm = TRUE) / 0.005) * 0.005


p_2c <- ggplot(f2c, aes(x = Category, y = value, fill = Category)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.45,
    colour = "grey55"
  ) +
  geom_col(
    width = 0.4,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = cost_cols[cost_order_f2c],
    breaks = cost_order_f2c,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 0.1),
    breaks = seq(y_min_2c, y_max_2c, by = 0.005),
    limits = c(y_min_2c, y_max_2c),
    expand = expansion(mult = c(0, 0.02))
  ) +
  labs(
    x = NULL,
    y = "Difference in cumulative costs by 2050 [%]"
  ) +
  theme_nature(base_size = 11) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    
    axis.title.y = element_text(size = 22, margin = margin(r = 8)),
    axis.text.y  = element_text(size = 19),
    
    legend.position = "bottom",
    legend.text = element_text(size = 14),
    legend.key.size = unit(3.2, "mm"),
    legend.spacing.x = unit(1.0, "mm"),
    legend.spacing.y = unit(0.6, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 0, 0),
    
    plot.margin = margin(5, 4, 5, 4, "mm")
  ) +
  guides(
    fill = guide_legend(
      ncol = 2,
      byrow = TRUE,
      keyheight = unit(3.2, "mm"),
      keywidth  = unit(3.2, "mm")
    )
  )

p_2c

# ============================================================
# Figure 4 — Selected regional electricity generation results across scenarios 
# Panels:
# (a) Thira
# (b) Kos-Kalymnos
# (c) Peloponnese
# (d) Western Macedonia
# ============================================================

# ------------------------------------------------------------
# 1. File and sheets
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"

sheet_f4a <- "Thira"
sheet_f4b <- "Kos-Kalymnos"
sheet_f4c <- "Peloponnese"
sheet_f4d <- "Western Macedonia"

# ------------------------------------------------------------
# 2. Scenario and year order
# ------------------------------------------------------------

year_order <- c(2025, 2030, 2035, 2040, 2045, 2050)

pathway_order <- c(
  "NECP",
  "NECP_N2000",
  "NECP_BIOD",
  "NECP+",
  "NECP+_N2000",
  "NECP+_BIOD"
)

pathway_labels_short <- c(
  "NECP"        = "NECP",
  "NECP_N2000"  = "N2000",
  "NECP_BIOD"   = "BIOD",
  "NECP+"       = "NECP+",
  "NECP+_N2000" = "+N2000",
  "NECP+_BIOD"  = "+BIOD"
)

# ------------------------------------------------------------
# 3. Numeric x-axis structure
# ------------------------------------------------------------

x_axis_info <- tidyr::crossing(
  Year = year_order,
  Pathway = factor(pathway_order, levels = pathway_order)
) |>
  arrange(Year, match(as.character(Pathway), pathway_order)) |>
  mutate(
    Pathway = as.character(Pathway),
    x = row_number(),
    scenario_lab = pathway_labels_short[Pathway]
  )

scenario_breaks <- x_axis_info$x
scenario_labels <- x_axis_info$scenario_lab

year_label_df <- x_axis_info |>
  group_by(Year) |>
  summarise(
    x = mean(x),
    .groups = "drop"
  )

year_separators <- seq(
  from = length(pathway_order) + 0.5,
  by = length(pathway_order),
  length.out = length(year_order) - 1
)

make_year_strip <- function(text_size = 4.8) {
  ggplot(year_label_df, aes(x = x, y = 1, label = Year)) +
    geom_text(
      size = text_size,
      fontface = "bold",
      colour = "black"
    ) +
    scale_x_continuous(
      limits = c(0.5, max(scenario_breaks) + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(0.8, 1.2),
      expand = c(0, 0)
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 5, 0, 5, "mm")
    )
}

# ------------------------------------------------------------
# 4. Variable order and colours
# ------------------------------------------------------------

generation_order_f4 <- c(
  "Biomass",
  "Natural Gas",
  "Oil",
  "Hydro",
  "Onshore",
  "Offshore",
  "PV Utility",
  "PV Rooftop",
  "Net Trade",
  "Curtailment"
)

region_cols <- c(
  "Biomass"     = "#009E73",
  "Natural Gas" = "#F0E442",
  "Oil"         = "#9E2D1E",
  "Hydro"       = "#80CDC1",
  "Onshore"     = "#56B4E9",
  "Offshore"    = "#0072B2",
  "PV Utility"  = "#F4A582",
  "PV Rooftop"  = "#FDBF6F",
  "Net Trade"   = "#6A3D9A",
  "Curtailment" = "#5F5F5F"
)

# ------------------------------------------------------------
# 5. Theme
# ------------------------------------------------------------

theme_nature <- function(base_size = 10, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(size = base_size + 2),
      axis.text = element_text(size = base_size + 1, colour = "black"),
      axis.line = element_line(linewidth = 0.3, colour = "black"),
      axis.ticks = element_line(linewidth = 0.3, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size + 1),
      legend.key.size = unit(4.8, "mm"),
      legend.spacing.x = unit(1.5, "mm"),
      plot.margin = margin(8, 10, 10, 10, "pt")
    )
}

# ------------------------------------------------------------
# 6. Read regional sheet
# ------------------------------------------------------------

read_region_sheet <- function(file, sheet) {
  
  raw <- read_excel(
    file,
    sheet = sheet,
    col_names = FALSE
  )
  
  years <- raw[1, -1] |>
    unlist(use.names = FALSE) |>
    as.integer()
  
  pathways <- raw[2, -1] |>
    unlist(use.names = FALSE) |>
    as.character() |>
    str_squish()
  
  values <- raw[-c(1, 2), ]
  
  names(values) <- c(
    "Variable",
    paste(years, pathways, sep = "__")
  )
  
  values |>
    pivot_longer(
      cols = -Variable,
      names_to = "key",
      values_to = "value"
    ) |>
    separate(
      key,
      into = c("Year", "Pathway"),
      sep = "__",
      convert = TRUE
    ) |>
    mutate(
      Year = as.integer(Year),
      Pathway = str_squish(as.character(Pathway)),
      Variable = str_squish(as.character(Variable)),
      value = as.numeric(value)
    ) |>
    filter(
      Year %in% year_order,
      Pathway %in% pathway_order,
      Variable %in% c(generation_order_f4, "Demand"),
      !is.na(value)
    )
}

# ------------------------------------------------------------
# 7. Prepare regional data
# ------------------------------------------------------------

prepare_region_data <- function(region_data) {
  
  region_fig <- region_data |>
    mutate(
      Pathway = as.character(Pathway)
    ) |>
    left_join(
      x_axis_info |> select(Year, Pathway, x),
      by = c("Year", "Pathway")
    )
  
  region_bars <- region_fig |>
    filter(Variable %in% generation_order_f4) |>
    mutate(
      Variable = factor(Variable, levels = generation_order_f4)
    )
  
  region_demand <- region_fig |>
    filter(Variable == "Demand")
  
  list(
    fig = region_fig,
    bars = region_bars,
    demand = region_demand
  )
}

# ------------------------------------------------------------
# 8. Plot core panel
# ------------------------------------------------------------

plot_region_core <- function(
    region_bars,
    region_demand,
    y_label = "Electricity generation [TWh]"
) {
  
  y_max <- max(
    region_bars |>
      filter(value > 0) |>
      group_by(x) |>
      summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
      pull(total),
    region_demand$value,
    na.rm = TRUE
  ) * 1.10
  
  y_min <- min(region_bars$value, 0, na.rm = TRUE) * 1.20
  
  ggplot() +
    geom_vline(
      xintercept = year_separators,
      linewidth = 0.25,
      linetype = "dotted",
      colour = "grey70"
    ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.35,
      colour = "black"
    ) +
    geom_col(
      data = region_bars,
      aes(x = x, y = value, fill = Variable),
      width = 0.78,
      linewidth = 0,
      position = position_stack(reverse = TRUE)
    ) +
    geom_point(
      data = region_demand,
      aes(x = x, y = value, shape = "Demand"),
      colour = "black",
      size = 3.4
    ) +
    scale_fill_manual(
      values = region_cols[generation_order_f4],
      breaks = generation_order_f4,
      drop = FALSE
    ) +
    scale_shape_manual(
      values = c("Demand" = 16),
      breaks = "Demand",
      name = NULL
    ) +
    scale_x_continuous(
      breaks = scenario_breaks,
      labels = scenario_labels,
      limits = c(0.5, max(scenario_breaks) + 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = pretty_breaks(n = 6),
      expand = expansion(mult = c(0, 0.03))
    ) +
    coord_cartesian(
      ylim = c(y_min, y_max),
      clip = "off"
    ) +
    labs(
      x = NULL,
      y = y_label
    ) +
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE,
        order = 1
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(size = 4.2, colour = "black")
      )
    ) +
    theme_nature(base_size = 10) +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.title.y = element_text(size = 18, margin = margin (r=10)),
      axis.text.y  = element_text(size = 16),
      axis.text.x  = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
      legend.text = element_text(size = 17),
      legend.key.size = unit(4.8, "mm"),
      plot.margin = margin(5, 5, 0, 5, "mm")
    )
}

# ------------------------------------------------------------
# Manual legend for Figure 4
# ------------------------------------------------------------

make_manual_legend_f4 <- function(
    items,
    colours,
    include_demand = TRUE,
    ncol = 4,
    text_size = 5.0,
    item_width = 1.05,
    key_width = 0.14,
    key_height = 0.13,
    text_gap = 0.05,
    row_spacing = 0.38,
    center_shift = 0
) {
  
  legend_items <- tibble(
    item = items,
    colour = as.character(unname(colours[items])),
    type = "fill"
  )
  
  if (include_demand) {
    legend_items <- bind_rows(
      legend_items,
      tibble(
        item = "Demand",
        colour = "black",
        type = "point"
      )
    )
  }
  
  legend_df <- legend_items |>
    mutate(
      item_id = row_number(),
      
      # Put Demand alone in column 6
      col = if_else(
        item == "Demand",
        5L,                             # zero-based column index: 5 = 6th column
        as.integer((item_id - 1) %% 5)  # first five columns for all other items
      ),
      
      row = if_else(
        item == "Demand",
        0L,                             # same row, alone in column 6
        as.integer(floor((item_id - 1) / 5))
      ),
      
      row = max(row) - row,
      
      x_key_left  = col * item_width,
      x_key_right = x_key_left + key_width,
      x_key_mid   = (x_key_left + x_key_right) / 2,
      x_text      = x_key_right + text_gap,
      y = row * row_spacing
    )
  
  content_left  <- min(legend_df$x_key_left)
  content_right <- max(legend_df$x_text) + 0.80
  content_width <- content_right - content_left
  
  plot_width <- content_width + 0.30
  x_offset <- (plot_width - content_width) / 2 - content_left + center_shift
  
  legend_df <- legend_df |>
    mutate(
      x_key_left  = x_key_left + x_offset,
      x_key_right = x_key_right + x_offset,
      x_key_mid   = x_key_mid + x_offset,
      x_text      = x_text + x_offset
    )
  
  ggplot() +
    geom_rect(
      data = legend_df |> filter(type == "fill"),
      aes(
        xmin = x_key_left,
        xmax = x_key_right,
        ymin = y - key_height / 2,
        ymax = y + key_height / 2
      ),
      fill = (legend_df |> filter(type == "fill"))$colour,
      colour = NA
    ) +
    geom_point(
      data = legend_df |> filter(type == "point"),
      aes(x = x_key_mid, y = y),
      colour = "black",
      size = 3.6
    ) +
    geom_text(
      data = legend_df,
      aes(x = x_text, y = y, label = item),
      hjust = 0,
      vjust = 0.5,
      size = text_size,
      colour = "black"
    ) +
    coord_cartesian(
      xlim = c(0, plot_width),
      ylim = c(-0.30, max(legend_df$y) + 0.30),
      clip = "off"
    ) +
    theme_void() +
    theme(
      plot.margin = margin(0, 0, 0, 0, "mm")
    )
}

# ------------------------------------------------------------
# Build final panel: core plot + year strip + manual legend
# ------------------------------------------------------------

build_region_panel <- function(region_bars, region_demand) {
  
  p_core <- plot_region_core(
    region_bars = region_bars,
    region_demand = region_demand
  ) +
    theme(
      legend.position = "none",
      plot.margin = margin(5, 5, 0, 5, "mm")
    )
  
  p_years <- make_year_strip(text_size = 4.8)
  
  p_legend <- make_manual_legend_f4(
    items = generation_order_f4,
    colours = region_cols,
    include_demand = TRUE,
    ncol = 6,
    text_size = 5.0,
    item_width = 1.05,
    key_width = 0.14,
    key_height = 0.13,
    text_gap = 0.05,
    row_spacing = 0.38,
    center_shift = 0
  )
  
  p_legend_centered <- cowplot::plot_grid(
    NULL,
    p_legend,
    NULL,
    ncol = 3,
    rel_widths = c(0.08, 0.84, 0.08)
  )
  
  cowplot::plot_grid(
    p_core,
    p_years,
    p_legend_centered,
    ncol = 1,
    align = "v",
    rel_heights = c(1, 0.07, 0.30)
  )
}


# ============================================================
# 10. Read all regional data
# ============================================================

thira <- read_region_sheet(xlsx_file, sheet_f4a)
kos <- read_region_sheet(xlsx_file, sheet_f4b)
peloponnese <- read_region_sheet(xlsx_file, sheet_f4c)
wm <- read_region_sheet(xlsx_file, sheet_f4d)

# ============================================================
# 11. Prepare all regional data
# ============================================================

thira_data <- prepare_region_data(thira)
kos_data <- prepare_region_data(kos)
peloponnese_data <- prepare_region_data(peloponnese)
wm_data <- prepare_region_data(wm)

# ============================================================
# 12. Build panels
# ============================================================

p_4a <- build_region_panel(
  region_bars = thira_data$bars,
  region_demand = thira_data$demand
)

p_4b <- build_region_panel(
  region_bars = kos_data$bars,
  region_demand = kos_data$demand
)

p_4c <- build_region_panel(
  region_bars = peloponnese_data$bars,
  region_demand = peloponnese_data$demand
)

p_4d <- build_region_panel(
  region_bars = wm_data$bars,
  region_demand = wm_data$demand
)

p_4a
p_4b
p_4c
p_4d

#----------------------------------------------------------------------------------------------
# Reconciling the green-versus-green dilemma in Greece’s renewable electricity transition 
#----------------------------------------------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)
library(stringr)
library(forcats)
library(scales)
library(cowplot)

# ============================================================
# Figure 1 — HERMES comparison among all six pathways
# Panels:
# (a) Annual installed capacity (GW)
# (b) Annual electricity generation (TWh)
# (c) Cumulative system cost increases over NECP baseline by 2050 (%)
# ============================================================


xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"

sheet_capacity   <- "capacity"
sheet_generation <- "generation"
sheet_costs      <- "costs"

# ------------------------------------------------------------
# Scenario, year and technology order
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

# Consistent colour palette across panels and future figures.
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
# Theme
# ------------------------------------------------------------

theme_nature <- function(base_size = 7, base_family = "Arial") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      text = element_text(colour = "black"),
      axis.title = element_text(size = base_size),
      axis.text = element_text(size = base_size - 1, colour = "black"),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.line = element_line(linewidth = 0.25, colour = "black"),
      axis.ticks = element_line(linewidth = 0.25, colour = "black"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25, colour = "grey85"),
      legend.title = element_blank(),
      legend.text = element_text(size = base_size - 1),
      legend.key.size = unit(3.2, "mm"),
      legend.spacing.x = unit(1.2, "mm"),
      plot.margin = margin(2, 2, 2, 2, "mm"),
      strip.text = element_text(size = base_size, face = "bold")
    )
}

# ------------------------------------------------------------
# Data-cleaning
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
    as.character()
  
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
      Pathway = factor(Pathway, levels = pathway_order),
      Technology = factor(Technology, levels = technology_order),
      value = as.numeric(.data[[value_name]])
    ) |>
    select(Year, Pathway, Technology, value) |>
    filter(
      !is.na(value),
      !is.na(Pathway),
      !is.na(Technology)
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
    as.character()
  
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

make_x_lab <- function(year, pathway) {
  paste0(pathway_labels[as.character(pathway)], "\n", year)
}

make_x_levels <- function() {
  tidyr::crossing(
    Year = year_order,
    Pathway = factor(pathway_order, levels = pathway_order)
  ) |>
    mutate(
      Pathway = as.character(Pathway),
      x_lab = make_x_lab(Year, Pathway)
    ) |>
    arrange(Year, match(Pathway, pathway_order)) |>
    pull(x_lab)
}

# ------------------------------------------------------------
# Panel A — annual installed capacity
# ------------------------------------------------------------

capacity_fig <- capacity |>
  filter(Technology != "Net Imports") |>
  mutate(
    Technology = factor(Technology, levels = technology_order),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  )

n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)

p_a <- ggplot(capacity_fig, aes(x = x_lab, y = value, fill = Technology)) +
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
  guides(
    fill = guide_legend(reverse = FALSE)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    breaks = pretty_breaks(n = 6)
  ) +
  labs(
    x = NULL,
    y = "Installed capacity [GW]"
  ) +
  theme_nature(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(4.2, "mm")
  )

p_a

# ------------------------------------------------------------
# Panel B — annual electricity generation
# ------------------------------------------------------------

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
  "Net Imports"   = "#999999"
)

make_x_lab <- function(year, pathway) {
  paste0(pathway_labels[as.character(pathway)], "\n", year)
}

make_x_levels <- function() {
  tidyr::crossing(
    Year = year_order,
    Pathway = factor(pathway_order, levels = pathway_order)
  ) |>
    mutate(
      Pathway = as.character(Pathway),
      x_lab = make_x_lab(Year, Pathway)
    ) |>
    arrange(Year, match(Pathway, pathway_order)) |>
    pull(x_lab)
}

read_generation_sheet <- function(file, sheet) {
  
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
    as.character()
  
  values <- raw[-c(1, 2), ]
  
  names(values) <- c(
    "Technology",
    paste(years, pathways, sep = "__")
  )
  
  values |>
    pivot_longer(
      cols = -Technology,
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
      Technology = str_squish(as.character(Technology)),
      value = as.numeric(value)
    ) |>
    filter(
      Year %in% year_order,
      Pathway %in% pathway_order,
      Technology %in% technology_order_gen,
      !is.na(value)
    )
}

# ------------------------------------------------------------
# Read and clean generation data
# ------------------------------------------------------------

generation <- read_generation_sheet(
  file = xlsx_file,
  sheet = sheet_generation
)

generation_fig <- generation |>
  mutate(
    Pathway = factor(Pathway, levels = pathway_order),
    Technology = factor(Technology, levels = technology_order_gen),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  ) |>
  arrange(Year, Pathway, Technology)

# ------------------------------------------------------------
# Split positive and negative values
# ------------------------------------------------------------

generation_pos <- generation_fig |>
  filter(value >= 0)

generation_neg <- generation_fig |>
  filter(value < 0)

generation_neg |>
  summarise(
    min_negative = min(value, na.rm = TRUE),
    max_negative = max(value, na.rm = TRUE)
  )

n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)

y_max_b <- generation_pos |>
  group_by(x_lab) |>
  summarise(total_positive = sum(value, na.rm = TRUE), .groups = "drop") |>
  summarise(max_total = max(total_positive, na.rm = TRUE)) |>
  pull(max_total) * 1.06

y_min_b <- -20

# ------------------------------------------------------------
# Plot Figure 1b
# ------------------------------------------------------------

p_b <- ggplot() +
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
    aes(x = x_lab, y = value, fill = Technology),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_col(
    data = generation_neg,
    aes(x = x_lab, y = value, fill = Technology),
    width = 0.78,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order_gen],
    breaks = technology_order_gen,
    drop = FALSE
  ) +
  guides(
    fill = guide_legend(reverse = FALSE)
  ) +
  scale_y_continuous(
    breaks = seq(-20, ceiling(y_max_b / 50) * 50, by = 20),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(
    ylim = c(y_min_b, y_max_b),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 9) +
  theme(
    legend.position = "bottom",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 12, angle = 90, vjust = 0.5, hjust = 1),
    legend.text  = element_text(size = 16),
    legend.key.size = unit(4.2, "mm")
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
    width = 0.58,
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
  theme_nature(base_size = 9) +
  theme(
    legend.position = "none",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(
      size = 16,
      angle = 0,
      hjust = 0.5,
      vjust = 0.5
    )
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

f2a


#Plot
p_2a <- ggplot(f2a, aes(x = Technology, y = value, fill = Technology)) +
  geom_hline(
    yintercept = 0,
    linewidth = 0.35,
    colour = "grey60"
  ) +
  geom_col(
    width = 0.58,
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
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Difference in installed capacity in 2050 [%]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(4.8, "mm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 3,
      byrow = TRUE
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
    linewidth = 0.35,
    colour = "grey60"
  ) +
  geom_col(
    width = 0.58,
    linewidth = 0
  ) +
  scale_fill_manual(
    values = tech_cols[technology_order_f2b],
    breaks = technology_order_f2b,
    drop = FALSE
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    breaks = seq(y_min_2b, y_max_2b, by = 0.05),
    limits = c(y_min_2b, y_max_2b),
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Difference in electricity generation in 2050 [%]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(4.8, "mm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 2,
      byrow = TRUE
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
    linewidth = 0.35,
    colour = "grey60"
  ) +
  geom_col(
    width = 0.58,
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
    expand = expansion(mult = c(0, 0.03))
  ) +
  labs(
    x = NULL,
    y = "Difference in cumulative costs by 2050"
  ) +
  theme_nature(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    legend.key.size = unit(4.8, "mm")
  ) +
  guides(
    fill = guide_legend(
      nrow = 3,
      byrow = TRUE
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
#Figure 4a - Thira
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f4a <- "Thira"

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

make_x_lab <- function(year, pathway) {
  paste0(pathway_labels[as.character(pathway)], "\n", year)
}

make_x_levels <- function() {
  tidyr::crossing(
    Year = year_order,
    Pathway = factor(pathway_order, levels = pathway_order)
  ) |>
    mutate(
      Pathway = as.character(Pathway),
      x_lab = make_x_lab(Year, Pathway)
    ) |>
    arrange(Year, match(Pathway, pathway_order)) |>
    pull(x_lab)
}

# ------------------------------------------------------------
# Variable order and colours
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
# Theme
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

thira <- read_region_sheet(
  file = xlsx_file,
  sheet = sheet_f4a
)

thira_fig <- thira |>
  mutate(
    Pathway = factor(Pathway, levels = pathway_order),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  )

thira_bars <- thira_fig |>
  filter(Variable %in% generation_order_f4) |>
  mutate(
    Variable = factor(Variable, levels = generation_order_f4)
  )

thira_demand <- thira_fig |>
  filter(Variable == "Demand")

# Year separators
n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)


y_max_4a <- max(
  thira_bars |>
    filter(value > 0) |>
    group_by(x_lab) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(total),
  thira_demand$value,
  na.rm = TRUE
) * 1.10

y_min_4a <- min(thira_bars$value, 0, na.rm = TRUE) * 1.20

# ------------------------------------------------------------
# Plot Figure 4a
# ------------------------------------------------------------

p_4a <- ggplot() +
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
    data = thira_bars,
    aes(x = x_lab, y = value, fill = Variable),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_point(
    data = thira_demand,
    aes(x = x_lab, y = value, shape = "Demand"),
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
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(
    ylim = c(y_min_4a, y_max_4a),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
    legend.text  = element_text(size = 14),
    legend.key.size = unit(4.8, "mm")
  )

p_4a


# ------------------------------------------------------------
#Figure 4b - Kos-Kalymnos
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f4b <- "Kos-Kalymnos"

kos <- read_region_sheet(
  file = xlsx_file,
  sheet = sheet_f4b
)

kos_fig <- kos |>
  mutate(
    Pathway = factor(Pathway, levels = pathway_order),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  )

kos_bars <- kos_fig |>
  filter(Variable %in% generation_order_f4) |>
  mutate(
    Variable = factor(Variable, levels = generation_order_f4)
  )

kos_demand <- kos_fig |>
  filter(Variable == "Demand")

n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)

y_max_4b <- max(
  kos_bars |>
    filter(value > 0) |>
    group_by(x_lab) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(total),
  kos_demand$value,
  na.rm = TRUE
) * 1.10

y_min_4b <- min(kos_bars$value, 0, na.rm = TRUE) * 1.20

# ------------------------------------------------------------
# Plot Figure 4b
# ------------------------------------------------------------

p_4b <- ggplot() +
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
    data = kos_bars,
    aes(x = x_lab, y = value, fill = Variable),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_point(
    data = kos_demand,
    aes(x = x_lab, y = value, shape = "Demand"),
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
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(
    ylim = c(y_min_4b, y_max_4b),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
    legend.text  = element_text(size = 14),
    legend.key.size = unit(4.8, "mm")
  )

p_4b


# ------------------------------------------------------------
#Figure 4c - Peloponnese
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f4c <- "Peloponnese"

peloponnese <- read_region_sheet(
  file = xlsx_file,
  sheet = sheet_f4c
)

peloponnese_fig <- peloponnese |>
  mutate(
    Pathway = factor(Pathway, levels = pathway_order),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  )

peloponnese_bars <- peloponnese_fig |>
  filter(Variable %in% generation_order_f4) |>
  mutate(
    Variable = factor(Variable, levels = generation_order_f4)
  )

peloponnese_demand <- peloponnese_fig |>
  filter(Variable == "Demand")

n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)

y_max_4c <- max(
  peloponnese_bars |>
    filter(value > 0) |>
    group_by(x_lab) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(total),
  peloponnese_demand$value,
  na.rm = TRUE
) * 1.10

y_min_4c <- min(peloponnese_bars$value, 0, na.rm = TRUE) * 1.20

# ------------------------------------------------------------
# Plot Figure 4c
# ------------------------------------------------------------

p_4c <- ggplot() +
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
    data = peloponnese_bars,
    aes(x = x_lab, y = value, fill = Variable),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_point(
    data = peloponnese_demand,
    aes(x = x_lab, y = value, shape = "Demand"),
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
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(
    ylim = c(y_min_4c, y_max_4c),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
    legend.text = element_text(size = 14),
    legend.key.size = unit(4.8, "mm")
  )

p_4c

# ------------------------------------------------------------
#Figure 4d - Western Macedonia
# ------------------------------------------------------------

xlsx_file <- "C:/Users/nfrilingou/Documents/HERMES/HERMES_data.xlsx"
sheet_f4d <- "Western Macedonia"

wm <- read_region_sheet(
  file = xlsx_file,
  sheet = sheet_f4d
)

wm_fig <- wm |>
  mutate(
    Pathway = factor(Pathway, levels = pathway_order),
    x_lab = factor(
      make_x_lab(Year, Pathway),
      levels = make_x_levels()
    )
  )

wm_bars <- wm_fig |>
  filter(Variable %in% generation_order_f4) |>
  mutate(
    Variable = factor(Variable, levels = generation_order_f4)
  )

wm_demand <- wm_fig |>
  filter(Variable == "Demand")


n_pathways <- length(pathway_order)

year_separators <- seq(
  from = n_pathways + 0.5,
  by = n_pathways,
  length.out = length(year_order) - 1
)

y_max_4d <- max(
  wm_bars |>
    filter(value > 0) |>
    group_by(x_lab) |>
    summarise(total = sum(value, na.rm = TRUE), .groups = "drop") |>
    pull(total),
  wm_demand$value,
  na.rm = TRUE
) * 1.10

y_min_4d <- min(wm_bars$value, 0, na.rm = TRUE) * 1.20

# ------------------------------------------------------------
# Plot Figure 4d
# ------------------------------------------------------------

p_4d <- ggplot() +
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
    data = wm_bars,
    aes(x = x_lab, y = value, fill = Variable),
    width = 0.78,
    linewidth = 0,
    position = position_stack(reverse = TRUE)
  ) +
  geom_point(
    data = wm_demand,
    aes(x = x_lab, y = value, shape = "Demand"),
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
  scale_y_continuous(
    breaks = pretty_breaks(n = 6),
    expand = expansion(mult = c(0, 0.03))
  ) +
  coord_cartesian(
    ylim = c(y_min_4d, y_max_4d),
    clip = "off"
  ) +
  labs(
    x = NULL,
    y = "Electricity generation [TWh]"
  ) +
  theme_nature(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal",
    axis.title.y = element_text(size = 18),
    axis.text.y  = element_text(size = 16),
    axis.text.x  = element_text(size = 11, angle = 90, vjust = 0.5, hjust = 1),
    legend.text = element_text(size = 14),
    legend.key.size = unit(4.8, "mm")
  )

p_4d

#THE_END

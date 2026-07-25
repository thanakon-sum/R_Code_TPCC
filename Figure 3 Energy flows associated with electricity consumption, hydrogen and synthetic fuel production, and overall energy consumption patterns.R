# ============================================================================
# ENERGY FLOWS AND SECTOR ANALYSIS FIGURE
# ============================================================================
# 
# DESCRIPTION:
# This script creates a comprehensive 6-panel figure analyzing energy flows
# and sectoral energy consumption patterns across multiple climate scenarios.
# The figure combines Sankey diagrams showing energy transformation pathways
# with bar charts analyzing final energy consumption by sector.
#
# FIGURE LAYOUT:
# - Panels a-d: Energy flow Sankey diagrams (alluvial plots)
#   - Shows energy transformation from primary sources through electricity/
#     hydrogen production to final demand
#   - Includes losses at each conversion step
#   - Panel a: TPCC 2080
#   - Panel b: TPCC 2100
#   - Panel c: LCC 2100
#   - Panel d: HCC 2100
#
# - Panels e-f: Sectoral energy consumption analysis at 2100
#   - Panel e: Absolute energy consumption (EJ/yr) by sector
#   - Panel f: Percentage share by energy type and sector
#   - Sectors analyzed: Total, Industry, Transport, Buildings
#   - Energy types: Fossil, Biomass, Electricity, Hydrogen, Synfuel
#
# ============================================================================
# REQUIRED DATA FILES:
# ============================================================================
# 
# Place the following file in: userdirectory/data/
#
# 1. IAMCTemplate_Iteon_global.gdx

# ============================================================================
# REQUIRED R PACKAGES:
# ============================================================================
# 
# Install required packages if not already installed:
# install.packages(c("tidyverse", "ggalluvial", "gdxrrw", "cowplot", "ggpubr"))

# ============================================================================

# Load required libraries
library(tidyverse)
library(ggalluvial)
library(gdxrrw)
library(cowplot)
library(ggpubr)

# --------------------------------
# Setup GAMS
# --------------------------------
# Update this path to match your GAMS installation
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")

# --------------------------------
# File paths
# --------------------------------
# Defaults to ./data relative to this script's working directory; override if your data/ lives elsewhere
data_dir <- "data"
gdx_file <- file.path(data_dir, "IAMCTemplate_Iteon_global.gdx")

# Verify file exists
if (!file.exists(gdx_file)) stop("GDX file not found: ", gdx_file)

cat("\n========================================\n")
cat("ENERGY FLOW FIGURE GENERATOR\n")
cat("========================================\n")

# ============================================================================
# PART 1: LOAD DATA FOR ENERGY FLOW DIAGRAMS (Panels a-d)
# ============================================================================

cat("\n[1/5] Loading energy flow data...\n")

merged_data_flow <- rgdx(gdx_file, list(name = "mergedIAMC4AIM"))
df_study <- data.frame(merged_data_flow$val)

for(i in 1:length(merged_data_flow$uels)) {
  df_study[,i] <- merged_data_flow$uels[[i]][df_study[,i]]
}

colnames(df_study) <- c("Model", "Scenario", "Region", "Variable", "Unit", "Year", "Value")

df_study <- df_study %>%
  mutate(Year = as.numeric(Year),
         Value = as.numeric(Value)) %>%
  filter(Region == "World")

# Define study scenarios
study_scenarios <- c("HCC", "LCC", "TPCC")

df_study <- df_study %>%
  filter(Scenario %in% study_scenarios)

cat("   - Loaded", length(unique(df_study$Scenario)), "scenarios\n")

# ============================================================================
# PART 2: LOAD DATA FOR SECTOR ANALYSIS (Panels e-f)
# ============================================================================

cat("[2/5] Loading sector analysis data...\n")

merged_data_sector <- rgdx(gdx_file, list(name = "mergedIAMC4AIM"))
df_load_data <- data.frame(merged_data_sector$val)

for(i in 1:length(merged_data_sector$uels)) {
  df_load_data[,i] <- merged_data_sector$uels[[i]][df_load_data[,i]]
}
colnames(df_load_data) <- c("Model", "Scenario", "Region", "variable", "Unit", "Year", "value")
df_load_data <- df_load_data %>% 
  mutate(Year = as.numeric(Year), value = as.numeric(value)) %>%
  filter(Region == "World")

cat("   - Sector data loaded successfully\n")

# ============================================================================
# CALCULATE DERIVED VARIABLES (LOSSES) FOR ENERGY FLOW
# ============================================================================

calculate_losses <- function(data, year_val) {
  df_wide <- data %>%
    filter(Year == year_val) %>%
    select(Model, Scenario, Region, Variable, Value) %>%
    pivot_wider(names_from = Variable, values_from = Value, values_fill = 0)
  
  # Ensure all required columns exist, add them as 0 if they don't
  required_cols <- c("Sec_Ene_Ele", "Sec_Ene_Hyd_Ele", "Fin_Ene_Ele", "Sec_Ene_Inp_Ele_DAC",
                     "Sec_Ene_Hyd", "Fin_Ene_Liq_Hyd_syn", "Fin_Ene_Gas_Hyd_syn", 
                     "Fin_Ene_Hyd", "Sec_Ene_Inp_SolidsBio_DAC")
  
  for (col in required_cols) {
    if (!col %in% names(df_wide)) {
      df_wide[[col]] <- 0
    }
  }
  
  df_losses <- df_wide %>%
    mutate(
      Los_Ele = Sec_Ene_Ele - Sec_Ene_Hyd_Ele - Fin_Ene_Ele - Sec_Ene_Inp_Ele_DAC,
      Los_Hyd = Sec_Ene_Hyd - Fin_Ene_Liq_Hyd_syn - Fin_Ene_Gas_Hyd_syn - Fin_Ene_Hyd - Sec_Ene_Inp_SolidsBio_DAC
    ) %>%
    select(Model, Scenario, Region, Los_Ele, Los_Hyd) %>%
    pivot_longer(cols = c(Los_Ele, Los_Hyd), names_to = "Variable", values_to = "Value")
  
  return(df_losses)
}

# ============================================================================
# DEFINE ENERGY FLOW STRUCTURE FOR SANKEY
# ============================================================================

df_var <- tribble(
  ~Variable,                    ~Source,        ~Hydrogen,      ~Synfuel,       ~Final,
  'Sec_Ene_Hyd_Fos',            'Fossil',       'Hydrogen',     'Loss2',        'Loss',
  'Sec_Ene_Hyd_Bio',            'Biomass',      'Hydrogen',     'Loss2',        'Loss',
  'Sec_Ene_Inp_SolidsBio_DAC',   'Biomass',     'DAC',          'Loss2',        'Loss',
  'Los_Ele',                    'Electricity',  'Loss',         'Loss',         'Loss',
  'Sec_Ene_Inp_Ele_DAC',        'Electricity',  'DAC',          'Loss2',        'Loss',
  'Fin_Ene_Ele',                'Electricity',  'Electricity',  'Electricity',  'Electricity',
  'Los_Hyd',                    'Electricity',  'Hydrogen',     'Loss2',        'Loss',
  'Fin_Ene_Liq_Hyd_syn',        'Electricity',  'Hydrogen',     'Synfuel',      'Synfuel',
  'Fin_Ene_Gas_Hyd_syn',        'Electricity',  'Hydrogen',     'Synfuel',      'Synfuel',
  'Fin_Ene_Hyd',                'Electricity',  'Hydrogen',     'Hydrogen',     'Hydrogen'
)

carlev <- c('Synfuel', 'Hydrogen', 'Electricity', 'Fossil', 'Biomass',
            'DAC', 'Loss2', 'Loss')

carrier_colors <- c('Synfuel' = 'orchid', 
                    'Hydrogen' = 'thistle2', 
                    'Electricity' = 'lightsteelblue', 
                    'Fossil' = 'sandybrown', 
                    'Biomass' = 'darkolivegreen2',
                    'DAC' = 'darkgoldenrod2',
                    'Loss2' = 'grey',
                    'Loss' = 'grey')

# ============================================================================
# FUNCTION TO CREATE ENERGY FLOW SANKEY DIAGRAM
# ============================================================================

create_sankey <- function(data, year_val, scenario_val, show_title = FALSE) {
  
  # Calculate losses for this year
  df_losses <- calculate_losses(data %>% filter(Scenario == scenario_val), year_val)
  
  # Add calculated losses back to main data
  df_expanded <- data %>%
    filter(Year == year_val, Scenario == scenario_val) %>%
    bind_rows(df_losses %>% mutate(Year = year_val, Unit = "EJ/yr"))
  
  # Create plot
  p <- df_expanded %>% 
    filter(Region == 'World') %>% 
    inner_join(df_var, by = 'Variable') %>% 
    mutate(scen_lab = scenario_val) %>%
    select(colnames(df_var), Value, scen_lab) %>% 
    pivot_longer(cols = !c(scen_lab, Variable, Value), 
                 names_to = 'x', 
                 values_to = 'Carrier') %>% 
    mutate(Label = Carrier, Alpha = 1, Positionh = 0.5, Positionv = 0.5) %>% 
    mutate(Label = ifelse(x == 'Hydrogen' & Carrier == 'Electricity', ' ', Label)) %>%
    mutate(Label = ifelse(x == 'Synfuel' & Carrier %in% c('Electricity', 'Hydrogen', 'Loss'), ' ', Label)) %>%
    mutate(Label = ifelse(x == 'Synfuel' & Carrier == 'Loss2', 'Loss', Label)) %>%
    mutate(Label = ifelse(x == 'Final' & Carrier == 'Synfuel', ' ', Label)) %>%
    mutate(Alpha = ifelse(x == 'Hydrogen' & Carrier == 'Electricity', 0.5, Alpha)) %>%
    mutate(Alpha = ifelse(x == 'Synfuel' & Carrier %in% c('Electricity', 'Hydrogen', 'Loss'), 0.5, Alpha)) %>%
    mutate(Alpha = ifelse(x == 'Final' & Carrier == 'Loss', 0.5, Alpha)) %>%
    mutate(Positionh = ifelse(x == 'Source', 0.2, ifelse(x == 'Final', 0.8, 0.5))) %>%
    mutate(Positionv = ifelse(x == 'Source' & Carrier == 'Fossil', -0.5, Positionv)) %>%
    mutate(Variable = factor(Variable, levels = rev(df_var$Variable)),
           Carrier = factor(Carrier, levels = rev(carlev))) %>% 
    ggplot(aes(x = x, y = Value, alluvium = Variable, stratum = Carrier, label = Carrier)) +
    geom_flow(aes(fill = Carrier), show.legend = FALSE) +
    geom_stratum(aes(fill = Carrier, alpha = Alpha), color = 'transparent', show.legend = FALSE) +
    geom_text(aes(label = Label, hjust = Positionh, vjust = Positionv), 
              stat = 'stratum', size = 3) +
    labs(title = ifelse(show_title, paste0(scenario_val, " - ", year_val), NULL),
         x = NULL, 
         y = expression(paste('Energy (EJ ', yr^{-1}, ')'))) +
    scale_x_discrete(limits = colnames(df_var)[-1], 
                     labels = c('Source', '', '', 'Demand'), 
                     expand = c(0.05, 0.05)) +
    scale_y_continuous(limits = c(0, 800)) +
    scale_fill_manual(values = carrier_colors, name = NULL) +
    scale_alpha_continuous(limits = c(0, 1), range = c(0, 1)) +
    theme_bw() +
    theme(legend.position = 'none',
          strip.background = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
          axis.text.y = element_text(size = 8),
          axis.title.y = element_text(size = 9),
          plot.title = element_text(size = 9, face = "bold"))
  
  return(p)
}

# ============================================================================
# CREATE PANELS a-d: ENERGY FLOW DIAGRAMS
# ============================================================================

cat("[3/5] Creating energy flow Sankey diagrams...\n")

# Panel a: TPCC 2080
p_a <- create_sankey(df_study, 2080, "TPCC", show_title = TRUE)

# Panel b: TPCC 2100
p_b <- create_sankey(df_study, 2100, "TPCC", show_title = TRUE)

# Panel c: LCC 2100
p_c <- create_sankey(df_study, 2100, "LCC", show_title = TRUE)

# Panel d: HCC 2100
p_d <- create_sankey(df_study, 2100, "HCC", show_title = TRUE)

cat("   - Created 4 Sankey diagrams\n")

# ============================================================================
# CREATE PANELS e-f: SECTOR ANALYSIS
# ============================================================================

cat("[4/5] Creating sector analysis plots...\n")

# Define variable mappings for sectors
var_total <- tribble(
  ~Variable, ~Sector, ~Carrier,
  'Fin_Ene_SolidsCoa', 'Total', 'Coal',
  'Fin_Ene_Liq_Oil', 'Total', 'Oil',
  'Fin_Ene_Gas_Fos', 'Total', 'Gas',
  'Fin_Ene_Liq_Bio', 'Total', 'Biomass',
  'Fin_Ene_SolidsBio', 'Total', 'Biomass',
  'Fin_Ene_Ele', 'Total', 'Electricity',
  'Fin_Ene_Hyd', 'Total', 'Hydrogen',
  'Fin_Ene_Liq_Hyd_syn', 'Total', 'Synfuel',
  'Fin_Ene_Gas_Hyd_syn', 'Total', 'Synfuel'
)

var_industry <- tribble(
  ~Variable, ~Sector, ~Carrier,
  'Fin_Ene_Ind_SolidsCoa', 'Industry', 'Coal',
  'Fin_Ene_Ind_Liq_Oil', 'Industry', 'Oil',
  'Fin_Ene_Ind_Gas_Fos', 'Industry', 'Gas',
  'Fin_Ene_Ind_Liq_and_Sol_Bio', 'Industry', 'Biomass',
  'Fin_Ene_Ind_Ele', 'Industry', 'Electricity',
  'Fin_Ene_Ind_Hyd', 'Industry', 'Hydrogen',
  'Fin_Ene_Ind_Liq_Hyd_syn', 'Industry', 'Synfuel',
  'Fin_Ene_Ind_Gas_Hyd_syn', 'Industry', 'Synfuel'
)

var_transport <- tribble(
  ~Variable, ~Sector, ~Carrier,
  'Fin_Ene_Tra_Liq_Coa', 'Transport', 'Coal',
  'Fin_Ene_Tra_Liq_Oil', 'Transport', 'Oil',
  'Fin_Ene_Tra_Gas', 'Transport', 'Gas',
  'Fin_Ene_Tra_Liq_Bio', 'Transport', 'Biomass',
  'Fin_Ene_Tra_Ele', 'Transport', 'Electricity',
  'Fin_Ene_Tra_Hyd', 'Transport', 'Hydrogen',
  'Fin_Ene_Tra_Liq_Hyd_syn', 'Transport', 'Synfuel'
)

var_buildings <- tribble(
  ~Variable, ~Sector, ~Carrier,
  'Fin_Ene_Res_and_Com_SolidsCoa', 'Buildings', 'Coal',
  'Fin_Ene_Res_and_Com_Liq_Oil', 'Buildings', 'Oil',
  'Fin_Ene_Res_and_Com_Gas_Fos', 'Buildings', 'Gas',
  'Fin_Ene_Res_and_Com_SolidsBio', 'Buildings', 'Biomass',
  'Fin_Ene_Res_and_Com_Ele', 'Buildings', 'Electricity',
  'Fin_Ene_Res_and_Com_Hyd', 'Buildings', 'Hydrogen',
  'Fin_Ene_Res_and_Com_Liq_Hyd_syn', 'Buildings', 'Synfuel',
  'Fin_Ene_Res_and_Com_Gas_Hyd_syn', 'Buildings', 'Synfuel'
)

var_mapping <- bind_rows(var_total, var_industry, var_transport, var_buildings)

energy_type_map <- tribble(
  ~Carrier, ~EnergyType,
  'Coal', 'Fossil',
  'Oil', 'Fossil',
  'Gas', 'Fossil',
  'Biomass', 'Biomass',
  'Electricity', 'Electricity',
  'Hydrogen', 'Hydrogen',
  'Synfuel', 'Synfuel'
)

# Define scenarios for sector plots
cmain_plot <- c("HCC", "LCC", "TPCC")

# Prepare data - absolute values
plot_data_absolute <- df_load_data %>%
  filter(Year == 2100,
         Scenario %in% cmain_plot,
         variable %in% var_mapping$Variable) %>%
  left_join(var_mapping, by = c("variable" = "Variable")) %>%
  left_join(energy_type_map, by = "Carrier") %>%
  group_by(Scenario, Sector, EnergyType) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  mutate(
    Scenario = factor(Scenario, levels = cmain_plot),
    Sector = factor(Sector, levels = c("Total", "Industry", "Transport", "Buildings")),
    EnergyType = factor(EnergyType, levels = c("Fossil", "Biomass", "Electricity", "Hydrogen", "Synfuel"))
  )

# Prepare data - percentage
plot_data_percentage <- df_load_data %>%
  filter(Year == 2100,
         Scenario %in% cmain_plot,
         variable %in% var_mapping$Variable) %>%
  left_join(var_mapping, by = c("variable" = "Variable")) %>%
  left_join(energy_type_map, by = "Carrier") %>%
  group_by(Scenario, Sector, EnergyType) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = 'drop') %>%
  group_by(Scenario, Sector) %>%
  mutate(
    total = sum(value),
    percentage = (value / total) * 100
  ) %>%
  ungroup() %>%
  mutate(
    Scenario = factor(Scenario, levels = cmain_plot),
    Sector = factor(Sector, levels = c("Total", "Industry", "Transport", "Buildings")),
    EnergyType = factor(EnergyType, levels = c("Fossil", "Biomass", "Electricity", "Hydrogen", "Synfuel"))
  )

energy_colors <- c(
  "Fossil" = "sandybrown",
  "Electricity" = "lightsteelblue",
  "Hydrogen" = "thistle2",
  "Biomass" = "darkolivegreen2",
  "Synfuel" = "orchid"
)

# Panel e: Absolute values
p_e <- ggplot(plot_data_absolute, aes(x = Scenario, y = value, fill = EnergyType)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Sector, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = energy_colors) +
  scale_y_continuous(limits = c(0, 800)) +
  labs(
    x = "Scenario",
    y = expression(paste('Final Energy (EJ ', yr^{-1}, ')')),
    fill = "Energy Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_blank(),
    legend.position = "none",
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  )

# Panel f: Percentage share
p_f <- ggplot(plot_data_percentage, aes(x = Scenario, y = percentage, fill = EnergyType)) +
  geom_bar(stat = "identity", position = "stack") +
  facet_wrap(~Sector, nrow = 1, scales = "fixed") +
  scale_fill_manual(values = energy_colors) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(
    x = "Scenario",
    y = "Share (%)",
    fill = "Energy Type"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 9),
    strip.background = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.key.size = unit(1, "cm"),
    legend.key.width = unit(1.5, "cm"),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    panel.spacing = unit(0.5, "lines"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  guides(fill = guide_legend(nrow = 1, title.position = "top", title.hjust = 0.5))

cat("   - Created absolute and percentage sector plots\n")

# ============================================================================
# COMBINE ALL PANELS INTO FINAL FIGURE
# ============================================================================

cat("[5/5] Combining panels and creating final figure...\n")

# Remove legend from panel f first
p_f_no_legend <- p_f + theme(legend.position = "none")

# Top row: Arrange panels a-d (2x2 grid)
p_top <- plot_grid(
  p_a, p_b,
  p_c, p_d,
  labels = c("a", "b", "c", "d"),
  ncol = 2,
  nrow = 2
)

# Bottom row: Arrange panels e-f (side by side, both without legends)
p_bottom <- plot_grid(
  p_e,
  p_f_no_legend,
  labels = c("e", "f"),
  ncol = 2,
  nrow = 1,
  rel_widths = c(1, 1)
)

# Combine top and bottom rows
p_main <- plot_grid(
  p_top,
  p_bottom,
  ncol = 1,
  nrow = 2,
  rel_heights = c(1.2, 0.5)
)

# Create shared legend
basic_plot <- ggplot(data.frame(
  x = 1:5, 
  y = 1:5,
  type = factor(c("Fossil", "Biomass", "Electricity", "Hydrogen", "Synfuel"),
                levels = c("Fossil", "Biomass", "Electricity", "Hydrogen", "Synfuel"))
), aes(x = x, y = y, fill = type)) +
  geom_tile() +
  scale_fill_manual(
    values = energy_colors,
    name = "Energy Type"
  ) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_text(size = 8, face = "bold"),
    legend.text = element_text(size = 8),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm")
  ) +
  guides(fill = guide_legend(nrow = 1))

# Extract legend
legend <- get_legend(basic_plot)

# Add legend at the very bottom of everything
final_plot <- plot_grid(
  p_main,
  legend,
  ncol = 1,
  rel_heights = c(1, 0.05)
)

print(final_plot)

# ============================================================================
# SAVE OUTPUT
# ============================================================================

dir.create("output", showWarnings = FALSE)

ggsave(filename = "output/combined_energy_figure.png",
       plot = final_plot,
       width = 219.4, height = 306.9, units = "mm", dpi = 300, bg = "white")

ggsave(filename = "output/combined_energy_figure.pdf",
       plot = final_plot,
       width = 219.4, height = 306.9, units = "mm", dpi = 300, bg = "white")

cat("\n========================================\n")
cat("FIGURE GENERATION COMPLETE\n")
cat("========================================\n")
cat("Output files saved in 'output/' directory:\n")
cat("  - combined_energy_figure.png (350x280 mm, 300 dpi)\n")
cat("  - combined_energy_figure.pdf\n\n")
cat("Figure panels:\n")
cat("  a-d: Energy flow Sankey diagrams\n")
cat("  e:   Absolute sectoral energy consumption\n")
cat("  f:   Percentage share by energy type\n")
cat("========================================\n")
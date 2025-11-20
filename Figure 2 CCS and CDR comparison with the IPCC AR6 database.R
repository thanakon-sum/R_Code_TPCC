# ================================
# Combined Figure: CCS and CDR Analysis (6 panels)
# Left panels (a,c,e): CCS
# Right panels (b,d,f): CDR
# Top row (a,b): Cumulative emissions vs Cumulative deployment
# Middle row (c,d): Cumulative emissions vs Peak annual deployment
# Bottom row (e,f): Peak annual deployment vs 2100 values
# ================================

library(tidyverse)
library(gdxrrw)
library(cowplot)
library(readxl)
library(readr)
library(zoo)
library(RColorBrewer)

# --------------------------------
# Setup GAMS
# --------------------------------
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")

# --------------------------------
# File paths
# --------------------------------
gdx_file <- "/Users/suku/Downloads/IAMCTemplate_Iteon_global (5).gdx"
AR6_meta_file <- "/Users/suku/Downloads/1668008312256-AR6_Scenarios_Database_World_v1.1.csv/AR6_Scenarios_Database_metadata_indicators_v1.1.xlsx"
AR6_world_file <- "/Users/suku/Downloads/1668008312256-AR6_Scenarios_Database_World_v1.1.csv/AR6_Scenarios_Database_World_v1.1.csv"

# ============================================================================
# LOAD YOUR STUDY DATA
# ============================================================================

merged_data <- rgdx(gdx_file, list(name = "mergedIAMC4AIM"))
df_study <- data.frame(merged_data$val)
for (i in 1:length(merged_data$uels)) {
  df_study[, i] <- merged_data$uels[[i]][df_study[, i]]
}
colnames(df_study) <- c("Model", "Scenario", "Region", "Variable", "Unit", "Year", "Value")

df_study <- df_study %>%
  mutate(
    Year  = suppressWarnings(as.numeric(Year)),
    Value = suppressWarnings(as.numeric(Value))
  ) %>%
  filter(Region == "World")

# Filter scenarios (removed High Emissions)
study_scenarios <- c(
  "TPCC",
  "LCC",
  "HCC"
)

df_study <- df_study %>% filter(Scenario %in% study_scenarios)

# ============================================================================
# CALCULATE YOUR STUDY VALUES (ALL METRICS)
# ============================================================================

cat("\n=== Calculating Study Metrics ===\n")

# Cumulative emissions at 2100
df_study_emissions <- df_study %>%
  filter(Variable == "Emi_CO2_Cum", Year == 2100) %>%
  select(Model, Scenario, Value) %>%
  mutate(Cumulative_Emissions = Value / 1000)  # Mt -> Gt

# CCS metrics
df_study_ccs_cum <- df_study %>%
  filter(Variable == "Cum_Car_Seq_CCS", Year == 2100) %>%
  select(Model, Scenario, Value) %>%
  mutate(Cumulative_CCS = as.numeric(Value) / 1000)

df_study_ccs_peak <- df_study %>%
  filter(Variable == "Car_Seq_CCS", Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario) %>%
  summarise(Peak_CCS = max(Value, na.rm = TRUE) / 1000, .groups = "drop")

df_study_ccs_2100 <- df_study %>%
  filter(Variable == "Car_Seq_CCS", Year == 2100) %>%
  select(Model, Scenario, Value) %>%
  mutate(CCS_2100 = Value / 1000)

# CDR metrics
df_study_cdr_cum <- df_study %>%
  filter(Variable == "Cum_Car_Seq_CDR", Year == 2100) %>%
  select(Model, Scenario, Value) %>%
  mutate(Cumulative_CDR = as.numeric(Value) / 1000)

df_study_cdr_peak <- df_study %>%
  filter(Variable == "Car_Rem", Year >= 2020, Year <= 2100) %>%
  group_by(Model, Scenario) %>%
  summarise(Peak_CDR = max(Value, na.rm = TRUE) / 1000, .groups = "drop")

df_study_cdr_2100 <- df_study %>%
  filter(Variable == "Car_Rem", Year == 2100) %>%
  select(Model, Scenario, Value) %>%
  mutate(CDR_2100 = Value / 1000)

# Combine all metrics
df_study_all <- df_study_emissions %>%
  left_join(df_study_ccs_cum, by = c("Model", "Scenario")) %>%
  left_join(df_study_ccs_peak, by = c("Model", "Scenario")) %>%
  left_join(df_study_ccs_2100, by = c("Model", "Scenario")) %>%
  left_join(df_study_cdr_cum, by = c("Model", "Scenario")) %>%
  left_join(df_study_cdr_peak, by = c("Model", "Scenario")) %>%
  left_join(df_study_cdr_2100, by = c("Model", "Scenario"))

# ============================================================================
# LOAD AR6 DATA
# ============================================================================

cat("\n=== Loading AR6 data ===\n")

# Labels + cumulative emissions
df_AR6_labels <- read_xlsx(AR6_meta_file, sheet = "meta_Ch3vetted_withclimate") %>%
  select(
    Model,
    Scenario,
    Category,
    IMP_marker,
    Cumulative_Emissions = `Cumulative net CO2 (2020-2100, Gt CO2) (Harm-Infilled)`
  ) %>%
  mutate(
    Model = as.character(Model),
    Scenario = as.character(Scenario),
    Cumulative_Emissions = suppressWarnings(as.numeric(Cumulative_Emissions))
  ) %>%
  filter(Category %in% c("C1", "C2", "C3"))

# Read AR6 World CSV
df_AR6_world_wide <- read_csv(AR6_world_file, show_col_types = FALSE) %>%
  mutate(
    Model = as.character(Model),
    Scenario = as.character(Scenario),
    Region = as.character(Region),
    Variable = as.character(Variable),
    Unit = as.character(Unit)
  )

year_cols <- names(df_AR6_world_wide)[stringr::str_detect(names(df_AR6_world_wide), "^\\d{4}$")]
df_AR6_world_long <- df_AR6_world_wide %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year  = as.integer(Year),
    Value = suppressWarnings(as.numeric(Value))
  )

# ============================================================================
# INTERPOLATION FUNCTION
# ============================================================================

interpolate_annual <- function(df) {
  df_complete <- df %>%
    group_by(Model, Scenario, Variable) %>%
    arrange(Year) %>%
    complete(Year = seq(2020, 2100, by = 1)) %>%
    arrange(Year) %>%
    mutate(Value = zoo::na.approx(Value, na.rm = FALSE)) %>%
    ungroup()
  return(df_complete)
}

# ============================================================================
# CALCULATE AR6 CCS METRICS
# ============================================================================

cat("\n=== Calculating AR6 CCS metrics ===\n")

# Raw CCS data
df_AR6_ccs_raw <- df_AR6_world_long %>%
  filter(
    Region %in% c("World", "WORLD", "Global"),
    Variable == "Carbon Sequestration|CCS",
    Year >= 2020, Year <= 2100
  )

# Interpolate and calculate cumulative CCS
df_AR6_ccs_interpolated <- interpolate_annual(df_AR6_ccs_raw)

df_AR6_ccs_cum_calc <- df_AR6_ccs_interpolated %>%
  mutate(Value = Value / 1000) %>%
  group_by(Model, Scenario, Variable) %>%
  arrange(Year) %>%
  mutate(
    Value_lag = lag(Value, default = 0),
    trap_increment = (Value_lag + Value) / 2,
    trap_increment = if_else(Year == 2020, Value, trap_increment),
    cumulative = cumsum(trap_increment)
  ) %>%
  ungroup()

df_AR6_ccs_cum <- df_AR6_ccs_cum_calc %>%
  filter(Year == 2100) %>%
  group_by(Model, Scenario) %>%
  summarise(Cumulative_CCS = sum(cumulative, na.rm = TRUE), .groups = "drop")

# Peak CCS
df_AR6_ccs_peak <- df_AR6_world_long %>%
  filter(
    Region %in% c("World", "WORLD", "Global"),
    Variable == "Carbon Sequestration|CCS",
    Year >= 2020, Year <= 2100
  ) %>%
  mutate(Value = Value / 1000) %>%
  group_by(Model, Scenario) %>%
  summarise(Peak_CCS = max(Value, na.rm = TRUE), .groups = "drop")

# CCS at 2100
df_AR6_ccs_2100 <- df_AR6_world_long %>%
  filter(
    Region %in% c("World", "WORLD", "Global"),
    Variable == "Carbon Sequestration|CCS",
    Year == 2100
  ) %>%
  mutate(Value = Value / 1000) %>%
  group_by(Model, Scenario) %>%
  summarise(CCS_2100 = sum(Value, na.rm = TRUE), .groups = "drop")

# ============================================================================
# CALCULATE AR6 CDR METRICS
# ============================================================================

cat("\n=== Calculating AR6 CDR metrics ===\n")

cdr_vars <- c(
  "Carbon Sequestration|CCS|Biomass",
  "Carbon Sequestration|Land Use|Afforestation",
  "Carbon Sequestration|Direct Air Capture",
  "Carbon Sequestration|Enhanced Weathering",
  "Carbon Sequestration|Land Use|Biochar",
  "Carbon Sequestration|Land Use|Soil Carbon Management"
)

# Raw CDR data
df_AR6_cdr_raw <- df_AR6_world_long %>%
  filter(
    Region %in% c("World", "WORLD", "Global"),
    Variable %in% cdr_vars,
    Year >= 2020, Year <= 2100
  )

# Interpolate and calculate cumulative CDR
df_AR6_cdr_interpolated <- interpolate_annual(df_AR6_cdr_raw)

df_AR6_cdr_cum_calc <- df_AR6_cdr_interpolated %>%
  mutate(Value = Value / 1000) %>%
  group_by(Model, Scenario, Variable) %>%
  arrange(Year) %>%
  mutate(
    Value_lag = lag(Value, default = 0),
    trap_increment = (Value_lag + Value) / 2,
    trap_increment = if_else(Year == 2020, Value, trap_increment),
    cumulative = cumsum(trap_increment)
  ) %>%
  ungroup()

df_AR6_cdr_cum <- df_AR6_cdr_cum_calc %>%
  filter(Year == 2100) %>%
  group_by(Model, Scenario) %>%
  summarise(Cumulative_CDR = sum(cumulative, na.rm = TRUE), .groups = "drop")

# Peak CDR and CDR at 2100 (sum across technologies first)
df_AR6_cdr_total <- df_AR6_world_long %>%
  filter(
    Region %in% c("World", "WORLD", "Global"),
    Variable %in% cdr_vars,
    Year >= 2020, Year <= 2100
  ) %>%
  mutate(Value = Value / 1000) %>%
  group_by(Model, Scenario, Year) %>%
  summarise(Total_CDR = sum(Value, na.rm = TRUE), .groups = "drop")

df_AR6_cdr_peak <- df_AR6_cdr_total %>%
  group_by(Model, Scenario) %>%
  summarise(Peak_CDR = max(Total_CDR, na.rm = TRUE), .groups = "drop")

df_AR6_cdr_2100 <- df_AR6_cdr_total %>%
  filter(Year == 2100) %>%
  select(Model, Scenario, CDR_2100 = Total_CDR)

# ============================================================================
# COMBINE ALL AR6 METRICS
# ============================================================================

df_AR6_all <- df_AR6_labels %>%
  left_join(df_AR6_ccs_cum, by = c("Model", "Scenario")) %>%
  left_join(df_AR6_ccs_peak, by = c("Model", "Scenario")) %>%
  left_join(df_AR6_ccs_2100, by = c("Model", "Scenario")) %>%
  left_join(df_AR6_cdr_cum, by = c("Model", "Scenario")) %>%
  left_join(df_AR6_cdr_peak, by = c("Model", "Scenario")) %>%
  left_join(df_AR6_cdr_2100, by = c("Model", "Scenario")) %>%
  filter(!is.na(Cumulative_Emissions)) %>%
  mutate(across(c(Cumulative_CCS, Peak_CCS, CCS_2100, Cumulative_CDR, Peak_CDR, CDR_2100), 
                ~tidyr::replace_na(., 0)))

df_AR6_all_imps <- df_AR6_all %>%
  filter(!is.na(IMP_marker))

# ============================================================================
# SCENARIO LABELS AND COLORS
# ============================================================================

# Get colors from Set1 palette: red, green, blue
set1_colors <- brewer.pal(9, "Set1")

scenario_labels <- tribble(
  ~Scenario, ~Label, ~Color,
  "TPCC",  "TPCC",  set1_colors[1],  # Red
  "LCC",   "LCC",   set1_colors[3],  # Green
  "HCC",   "HCC",   set1_colors[2]   # Blue
)

df_study_all <- df_study_all %>% left_join(scenario_labels, by = "Scenario")
study_colors <- setNames(scenario_labels$Color, scenario_labels$Label)

IMP_shapes <- c(
  'GS' = 8, 'Neg' = 9, 'Ren' = 3, 'LD' = 11,
  'SP' = 4, 'Neg-2.0' = 10, 'Ren-2.0' = 12
)
category_shapes <- c('C1' = 1, 'C2' = 2, 'C3' = 0)

# ============================================================================
# COMMON THEME
# ============================================================================

theme_common <- theme_bw() +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.line = element_line(color = "black", linewidth = 0.3),
    axis.ticks = element_line(color = "black", linewidth = 0.3),
    axis.text = element_text(size = 8),
    axis.title = element_text(size = 9),
    legend.position = "none"
  )

# ============================================================================
# CREATE INDIVIDUAL PLOTS
# ============================================================================

# Panel (a): Cumulative emissions vs Cumulative CCS
p_a <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = Cumulative_Emissions, y = Cumulative_CCS, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = Cumulative_Emissions, y = Cumulative_CCS, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = Cumulative_Emissions, y = Cumulative_CCS, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors) +
  scale_shape_manual(values = c(category_shapes, IMP_shapes)) +
  labs(x = expression(paste("Cumulative net ", CO[2], " emissions 2020-2100 (Gt-", CO[2], ")")),
       y = expression(paste("Cumulative CCS 2020-2100 (Gt-", CO[2], ")"))) +
  theme_common

# Panel (b): Cumulative emissions vs Cumulative CDR
p_b <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = Cumulative_Emissions, y = Cumulative_CDR, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = Cumulative_Emissions, y = Cumulative_CDR, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = Cumulative_Emissions, y = Cumulative_CDR, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors) +
  scale_shape_manual(values = c(category_shapes, IMP_shapes)) +
  labs(x = expression(paste("Cumulative net ", CO[2], " emissions 2020-2100 (Gt-", CO[2], ")")),
       y = expression(paste("Cumulative CDR 2020-2100 (Gt-", CO[2], ")"))) +
  theme_common

# Panel (c): Cumulative emissions vs Peak CCS
p_c <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = Cumulative_Emissions, y = Peak_CCS, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = Cumulative_Emissions, y = Peak_CCS, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = Cumulative_Emissions, y = Peak_CCS, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors) +
  scale_shape_manual(values = c(category_shapes, IMP_shapes)) +
  labs(x = expression(paste("Cumulative net ", CO[2], " emissions 2020-2100 (Gt-", CO[2], ")")),
       y = expression(paste("Peak CCS (Gt-", CO[2], " ", yr^{-1}, ")"))) +
  theme_common

# Panel (d): Cumulative emissions vs Peak CDR
p_d <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = Cumulative_Emissions, y = Peak_CDR, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = Cumulative_Emissions, y = Peak_CDR, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = Cumulative_Emissions, y = Peak_CDR, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors) +
  scale_shape_manual(values = c(category_shapes, IMP_shapes)) +
  labs(x = expression(paste("Cumulative net ", CO[2], " emissions 2020-2100 (Gt-", CO[2], ")")),
       y = expression(paste("Peak CDR (Gt-", CO[2], " ", yr^{-1}, ")"))) +
  theme_common

# Panel (e): CCS at 2100 vs Peak CCS
p_e <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = CCS_2100, y = Peak_CCS, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = CCS_2100, y = Peak_CCS, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = CCS_2100, y = Peak_CCS, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors, name = "This study's\nscenarios") +
  scale_shape_manual(values = c(category_shapes, IMP_shapes)) +
  labs(x = expression(paste("CCS at 2100 (Gt-", CO[2], " ", yr^{-1}, ")")),
       y = expression(paste("Peak CCS (Gt-", CO[2], " ", yr^{-1}, ")"))) +
  theme_common

# Panel (f): CDR at 2100 vs Peak CDR  
p_f <- ggplot() +
  geom_point(data = df_AR6_all, aes(x = CDR_2100, y = Peak_CDR, shape = Category),
             color = "grey", size = 0.8, alpha = 0.5) +
  geom_point(data = df_AR6_all_imps, aes(x = CDR_2100, y = Peak_CDR, shape = IMP_marker),
             color = "black", size = 1.2) +
  geom_point(data = df_study_all, aes(x = CDR_2100, y = Peak_CDR, color = Label),
             size = 3) +
  scale_color_manual(values = study_colors, name = "This study's\nscenarios") +
  scale_shape_manual(values = c(category_shapes, IMP_shapes),
                     name = "AR6 category / IMP") +
  labs(x = expression(paste("CDR at 2100 (Gt-", CO[2], " ", yr^{-1}, ")")),
       y = expression(paste("Peak CDR (Gt-", CO[2], " ", yr^{-1}, ")"))) +
  theme_common

# ============================================================================
# EXTRACT LEGENDS
# ============================================================================

# Extract scenario legend from panel e
p_e_with_legend <- p_e + 
  guides(shape = "none") +
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(3, "mm"),
        legend.spacing.y = unit(1, "mm"),
        legend.margin = margin(t = 0, r = 0, b = -60, l = 0, unit = "mm"))

legend_scenarios <- get_legend(p_e_with_legend)

# Extract shape legend from panel f with all categories and IMPs
p_f_with_legend <- p_f + 
  guides(
    color = "none",
    shape = guide_legend(
      name = "AR6 category / IMP",
      override.aes = list(color = "black")
    )
  ) +
  theme(legend.position = "right",
        legend.title = element_text(size = 9),
        legend.text = element_text(size = 8),
        legend.key.size = unit(3, "mm"),
        legend.spacing.y = unit(1, "mm"),
        legend.margin = margin(t = 0, r = 0, b = 60, l = 0, unit = "mm"))

legend_shapes <- get_legend(p_f_with_legend)

# ============================================================================
# COMBINE PLOTS
# ============================================================================

# Create main plot grid (3 rows x 2 columns)
plot_grid_main <- plot_grid(
  p_a, p_b,
  p_c, p_d,
  p_e, p_f,
  ncol = 2,
  labels = c("a", "b", "c", "d", "e", "f"),
  label_size = 10,
  label_fontface = "bold"
)

# Combine legends vertically with minimal spacing
legends_combined <- plot_grid(
  legend_scenarios,
  legend_shapes,
  ncol = 1,
  rel_heights = c(0.8, 1.2),
  align = "v",
  axis = "lr"
)

# Final combined plot with legends on the right
final_plot <- plot_grid(
  plot_grid_main,
  legends_combined,
  ncol = 2,
  rel_widths = c(0.85, 0.15)
)

print(final_plot)

# ============================================================================
# SAVE
# ============================================================================

dir.create("output", showWarnings = FALSE)

ggsave(
  filename = "output/combined_6panel_ccs_cdr.png",
  plot = final_plot,
  width = 300, height = 360, units = "mm", dpi = 300
)

ggsave(
  filename = "output/combined_6panel_ccs_cdr.pdf",
  plot = final_plot,
  width = 300, height = 360, units = "mm", dpi = 300
)

cat("\n=== Combined 6-panel plot saved successfully ===\n")
cat("\nLayout:\n")
cat("Left panels (CCS):  a. Cum. emissions vs Cum. CCS\n")
cat("                    c. Cum. emissions vs Peak CCS\n")
cat("                    e. CCS at 2100 vs Peak CCS\n")
cat("Right panels (CDR): b. Cum. emissions vs Cum. CDR\n")
cat("                    d. Cum. emissions vs Peak CDR\n")
cat("                    f. CDR at 2100 vs Peak CDR\n")
cat("\nScenarios: TPCC, LCC, HCC\n")


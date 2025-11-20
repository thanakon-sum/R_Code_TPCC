library(tidyverse)
library(gdxrrw)
library(cowplot)
library(RColorBrewer)

# Setup GAMS
igdx("/Library/Frameworks/GAMS.framework/Versions/44/Resources")

# ============================================================================
# PART 1: CALCULATE REGIONAL THRESHOLDS FOR GEOLOGICAL FEASIBILITY
# ============================================================================

# ---------------------------------------------------------------------------
# 1A. WIND AND SOLAR THRESHOLDS
# ---------------------------------------------------------------------------
gdx_file_winsol <- "/Users/suku/Downloads/winsolpotential2.gdx"

winsol_cap <- rgdx.param(gdx_file_winsol, "winsol_cap") %>%
  as_tibble() %>%
  rename(Scen = 1, Sr33 = 2, Sre = 3, GR = 4, Y = 5, Value = 6) %>%
  filter(Scen == "MERRA2")

winsol_cf <- rgdx.param(gdx_file_winsol, "winsol_cf") %>%
  as_tibble() %>%
  rename(Scen = 1, Sr33 = 2, Sre = 3, GR = 4, Value = 5) %>%
  filter(Scen == "MERRA2")

# Region mapping to AIM17
region_mapping <- tribble(
  ~Sr33_original, ~Sr33_AIM17,
  "JPN", "JPN",
  "CHN", "CHN",
  "IND", "IND",
  "IDN", "XSE",
  "KOR", "XSE",
  "THA", "XSE",
  "MYS", "XSE",
  "VNM", "XSE",
  "XSE", "XSE",
  "XSA", "XSA",
  "XEA", "XSE",    # CORRECTED: was XSA
  "XCS", "CIS",
  "XME", "XME",
  "AUS", "XOC",
  "XOC", "XOC",
  "CAN", "CAN",
  "USA", "USA",
  "XE15", "XE25",
  "XE10", "XE25",
  "XE3", "XER",    # CORRECTED: was XE25
  "TUR", "TUR",
  "XEWI", "XER",
  "XEEI", "CIS",   # CORRECTED: was XER
  "XENI", "XER",
  "RUS", "CIS",
  "MEX", "XLM",
  "BRA", "BRA",
  "XLM", "XLM",
  "ZAF", "XAF",
  "XAF", "XAF",
  "XNF", "XNF"
)

winsol_cap_mapped <- winsol_cap %>%
  left_join(region_mapping, by = c("Sr33" = "Sr33_original")) %>%
  select(-Sr33) %>% rename(Sr33 = Sr33_AIM17)

winsol_cf_mapped <- winsol_cf %>%
  left_join(region_mapping, by = c("Sr33" = "Sr33_original")) %>%
  select(-Sr33) %>% rename(Sr33 = Sr33_AIM17)

potential_winsol_2050 <- winsol_cap_mapped %>%
  filter(Y == "2050") %>%
  group_by(Scen, Sr33, Sre, GR, Y) %>%
  summarise(Value_cap = sum(Value), .groups = "drop") %>%
  left_join(winsol_cf_mapped %>% 
              group_by(Scen, Sr33, Sre, GR) %>%
              summarise(Value_cf = mean(Value), .groups = "drop"),
            by = c("Scen", "Sr33", "Sre", "GR")) %>%
  mutate(potential_GWh = Value_cap * 8760 * Value_cf) %>%
  group_by(Sr33, Sre) %>%
  summarise(potential_GWh = sum(potential_GWh), .groups = "drop")

potential_winsol_aim17 <- potential_winsol_2050 %>%
  mutate(technology = case_when(
    Sre %in% c("PVCEN", "PVDEC") ~ "Solar",
    Sre %in% c("WNONS", "WNOFB", "WNOFF") ~ "Wind"
  )) %>%
  filter(!is.na(technology)) %>%
  group_by(Sr33, technology) %>%
  summarise(potential_EJ = sum(potential_GWh) * 0.00000036, .groups = "drop")

# ---------------------------------------------------------------------------
# 1B. BIOENERGY THRESHOLDS
# ---------------------------------------------------------------------------
gdx_file_bio <- "/Users/suku/Downloads/biopotential.gdx"

bio_potential_raw <- rgdx.param(gdx_file_bio, "PBION_out") %>%
  as_tibble() %>%
  rename(R = 1, Sgrade = 2, Sind = 3, Y = 4, Value = 5) %>%
  filter(Sind == "quantity", Y == "2050")

potential_bio_aim17 <- bio_potential_raw %>%
  left_join(region_mapping, by = c("R" = "Sr33_original")) %>%
  group_by(Sr33_AIM17) %>%
  summarise(potential_EJ = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Sr33_AIM17) %>%
  mutate(technology = "Bioenergy")

# ---------------------------------------------------------------------------
# 1C. CCS THRESHOLDS
# ---------------------------------------------------------------------------
ccs_file <- "/Users/suku/Downloads/gidden_et_al_geologic_carbon_storage_2.csv"
ccs_data_raw <- read_csv(ccs_file, skip = 3)

ccs_potential_country <- ccs_data_raw %>%
  select(NODE = 1, potential_Gt = 9) %>%
  mutate(potential_Gt = as.numeric(potential_Gt)) %>%
  filter(!is.na(potential_Gt), !is.na(NODE), NODE != "Total")

# NODE to AIM17 mapping for CCS (extensive mapping)
node_mapping_ccs <- tribble(
  ~NODE, ~Sr33_AIM17,
  "JPN", "JPN", "CHN", "CHN", "HKG", "CHN", "MAC", "CHN", "IND", "IND",
  "IDN", "XSE", "KOR", "XSE", "PRK", "XSE", "THA", "XSE", "MYS", "XSE",
  "VNM", "XSE", "PHL", "XSE", "SGP", "XSE", "MMR", "XSE", "KHM", "XSE",
  "LAO", "XSE", "BRN", "XSE", "TLS", "XSE", "TWN", "XSE",
  "PAK", "XSA", "BGD", "XSA", "LKA", "XSA", "NPL", "XSA", "AFG", "XSA",
  "BTN", "XSA", "MDV", "XSA", "MNG", "XSA",
  "SAU", "XME", "IRN", "XME", "IRQ", "XME", "ARE", "XME", "KWT", "XME",
  "QAT", "XME", "OMN", "XME", "YEM", "XME", "ISR", "XME", "JOR", "XME",
  "LBN", "XME", "SYR", "XME", "BHR", "XME", "PSE", "XME",
  "AUS", "XOC", "NZL", "XOC", "PNG", "XOC", "FJI", "XOC", "FSM", "XOC",
  "NCL", "XOC", "PYF", "XOC", "SLB", "XOC", "VUT", "XOC", "WSM", "XOC", "GUM", "XOC",
  "CAN", "CAN", "USA", "USA", "PRI", "USA", "VIR", "USA",
  "DEU", "XE25", "FRA", "XE25", "GBR", "XE25", "ITA", "XE25", "ESP", "XE25",
  "POL", "XE25", "NLD", "XE25", "BEL", "XE25", "GRC", "XE25", "PRT", "XE25",
  "CZE", "XE25", "ROU", "XE25", "HUN", "XE25", "AUT", "XE25", "SWE", "XE25",
  "BGR", "XE25", "DNK", "XE25", "FIN", "XE25", "SVK", "XE25", "IRL", "XE25",
  "HRV", "XE25", "LTU", "XE25", "SVN", "XE25", "LVA", "XE25", "EST", "XE25",
  "CYP", "XE25", "LUX", "XE25", "MLT", "XE25",
  "NOR", "XER", "CHE", "XER", "ISL", "XER", "ALB", "XER", "BIH", "XER",
  "MKD", "XER", "MNE", "XER", "SRB", "XER", "TUR", "TUR",
  "RUS", "CIS", "UKR", "CIS", "KAZ", "CIS", "UZB", "CIS", "BLR", "CIS",
  "AZE", "CIS", "GEO", "CIS", "TJK", "CIS", "TKM", "CIS", "KGZ", "CIS",
  "ARM", "CIS", "MDA", "CIS",
  "MEX", "XLM", "ARG", "XLM", "COL", "XLM", "CHL", "XLM", "PER", "XLM",
  "VEN", "XLM", "ECU", "XLM", "BOL", "XLM", "PRY", "XLM", "URY", "XLM",
  "CRI", "XLM", "PAN", "XLM", "GTM", "XLM", "HND", "XLM", "NIC", "XLM",
  "SLV", "XLM", "CUB", "XLM", "DOM", "XLM", "HTI", "XLM", "JAM", "XLM",
  "TTO", "XLM", "BHS", "XLM", "BRB", "XLM", "BLZ", "XLM", "GUY", "XLM",
  "SUR", "XLM", "GLP", "XLM", "MTQ", "XLM", "GUF", "XLM", "GRD", "XLM", "ABW", "XLM",
  "BRA", "BRA",
  "ZAF", "XAF", "NGA", "XAF", "KEN", "XAF", "ETH", "XAF", "TZA", "XAF",
  "GHA", "XAF", "UGA", "XAF", "MOZ", "XAF", "MDG", "XAF", "CMR", "XAF",
  "ZWE", "XAF", "AGO", "XAF", "ZMB", "XAF", "MWI", "XAF", "SEN", "XAF",
  "MLI", "XAF", "BFA", "XAF", "COG", "XAF", "COD", "XAF", "TGO", "XAF",
  "BEN", "XAF", "SLE", "XAF", "LBR", "XAF", "CAF", "XAF", "GAB", "XAF",
  "GNQ", "XAF", "BWA", "XAF", "NAM", "XAF", "LSO", "XAF", "SWZ", "XAF",
  "RWA", "XAF", "BDI", "XAF", "SOM", "XAF", "DJI", "XAF", "ERI", "XAF",
  "GIN", "XAF", "GNB", "XAF", "GMB", "XAF", "CIV", "XAF", "NER", "XAF",
  "TCD", "XAF", "MRT", "XAF", "STP", "XAF", "CPV", "XAF", "MUS", "XAF",
  "COM", "XAF", "MYT", "XAF", "REU", "XAF", "SSD", "XAF",
  "EGY", "XNF", "DZA", "XNF", "MAR", "XNF", "TUN", "XNF", "LBY", "XNF",
  "SDN", "XNF", "ESH", "XNF"
)

potential_ccs_aim17 <- ccs_potential_country %>%
  left_join(node_mapping_ccs, by = "NODE") %>%
  filter(!is.na(Sr33_AIM17)) %>%
  group_by(Sr33_AIM17) %>%
  summarise(potential_Gt = sum(potential_Gt, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Sr33_AIM17) %>%
  mutate(technology = "CCS")

# ---------------------------------------------------------------------------
# 1D. CREATE R5 AND WORLD AGGREGATIONS
# ---------------------------------------------------------------------------

# Mapping from AIM17 to R5 regions
r5_mapping <- tribble(
  ~Sr33_AIM17, ~Sr33_R5,
  "XE25", "R5OECD90+EU", "XER", "R5OECD90+EU", "TUR", "R5OECD90+EU",
  "XOC", "R5OECD90+EU", "JPN", "R5OECD90+EU", "CAN", "R5OECD90+EU", "USA", "R5OECD90+EU",
  "CHN", "R5ASIA", "IND", "R5ASIA", "XSE", "R5ASIA", "XSA", "R5ASIA",
  "BRA", "R5LAM", "XLM", "R5LAM",
  "CIS", "R5REF",
  "XME", "R5MAF", "XNF", "R5MAF", "XAF", "R5MAF"
)

# Aggregate Wind/Solar to R5
potential_winsol_r5 <- potential_winsol_aim17 %>%
  left_join(r5_mapping, by = c("Sr33" = "Sr33_AIM17")) %>%
  group_by(Sr33_R5, technology) %>%
  summarise(potential_EJ = sum(potential_EJ, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Sr33_R5)

potential_winsol_world <- potential_winsol_aim17 %>%
  group_by(technology) %>%
  summarise(potential_EJ = sum(potential_EJ, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sr33 = "World")

# Aggregate Bioenergy to R5
potential_bio_r5 <- potential_bio_aim17 %>%
  left_join(r5_mapping, by = c("Sr33" = "Sr33_AIM17")) %>%
  group_by(Sr33_R5, technology) %>%
  summarise(potential_EJ = sum(potential_EJ, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Sr33_R5)

potential_bio_world <- potential_bio_aim17 %>%
  summarise(potential_EJ = sum(potential_EJ, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sr33 = "World", technology = "Bioenergy")

# Aggregate CCS to R5
potential_ccs_r5 <- potential_ccs_aim17 %>%
  left_join(r5_mapping, by = c("Sr33" = "Sr33_AIM17")) %>%
  group_by(Sr33_R5, technology) %>%
  summarise(potential_Gt = sum(potential_Gt, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Sr33_R5)

potential_ccs_world <- potential_ccs_aim17 %>%
  summarise(potential_Gt = sum(potential_Gt, na.rm = TRUE), .groups = "drop") %>%
  mutate(Sr33 = "World", technology = "CCS")

# Combine all potentials
potential_winsol_all <- bind_rows(potential_winsol_aim17, potential_winsol_r5, potential_winsol_world)
potential_bio_all <- bind_rows(potential_bio_aim17, potential_bio_r5, potential_bio_world)
potential_ccs_all <- bind_rows(potential_ccs_aim17, potential_ccs_r5, potential_ccs_world)

# ---------------------------------------------------------------------------
# 1E. CALCULATE REGIONAL THRESHOLDS
# ---------------------------------------------------------------------------

# IPCC global thresholds
ipcc_thresholds <- tribble(
  ~technology, ~med_global, ~high_global, ~unit,
  "Wind",      830,         2000,          "EJ",
  "Solar",     1600,        50000,         "EJ",
  "Bioenergy", 100,         245,           "EJ",
  "CCS",       730,         1095,          "Gt"
)

# Calculate thresholds for Wind/Solar
global_winsol <- potential_winsol_world %>%
  select(technology, global_potential = potential_EJ)

thresholds_winsol <- potential_winsol_all %>%
  left_join(ipcc_thresholds %>% filter(technology %in% c("Wind", "Solar")), by = "technology") %>%
  left_join(global_winsol, by = "technology") %>%
  mutate(
    med_threshold = med_global * potential_EJ / global_potential,
    high_threshold = high_global * potential_EJ / global_potential
  )

# Calculate thresholds for Bioenergy
global_bio <- potential_bio_world %>%
  select(technology, global_potential = potential_EJ)

thresholds_bio <- potential_bio_all %>%
  left_join(ipcc_thresholds %>% filter(technology == "Bioenergy"), by = "technology") %>%
  left_join(global_bio, by = "technology") %>%
  mutate(
    med_threshold = med_global * potential_EJ / global_potential,
    high_threshold = high_global * potential_EJ / global_potential
  ) %>%
  rename(potential = potential_EJ)

# Calculate thresholds for CCS
global_ccs <- potential_ccs_world %>%
  select(technology, global_potential = potential_Gt)

thresholds_ccs <- potential_ccs_all %>%
  left_join(ipcc_thresholds %>% filter(technology == "CCS"), by = "technology") %>%
  left_join(global_ccs, by = "technology") %>%
  mutate(
    med_threshold = med_global * potential_Gt / global_potential,
    high_threshold = high_global * potential_Gt / global_potential
  ) %>%
  rename(potential = potential_Gt)

# Combine Wind/Solar thresholds and rename
thresholds_winsol <- thresholds_winsol %>%
  rename(potential = potential_EJ)

# Combine all thresholds
all_regional_thresholds <- bind_rows(
  thresholds_winsol,
  thresholds_bio,
  thresholds_ccs
)

# ============================================================================
# PART 2: LOAD SCENARIO DATA AND CREATE PLOTS
# ============================================================================

gdx_file <- "/Users/suku/Downloads/IAMCTemplate_Iteon_global (5).gdx"

# Read data
merged_data <- rgdx(gdx_file, list(name = "mergedIAMC4AIM"))
df_load_data <- data.frame(merged_data$val)

for(i in 1:length(merged_data$uels)) {
  df_load_data[,i] <- merged_data$uels[[i]][df_load_data[,i]]
}

colnames(df_load_data) <- c("Model", "Scenario", "Region", "variable", "Unit", "Year", "value")
df_load_data <- df_load_data %>% 
  mutate(Year = as.numeric(Year), value = as.numeric(value))

# Define scenarios
cmain <- c("CurPol", "TPCC", "TPCC_a", "TPCC_DAChigh", "TPCC_hydhigh", "TPCC_noBECCS", "TPCC_noEW")
cmain_plot <- c("TPCC", "TPCC_a", "TPCC_DAChigh", "TPCC_hydhigh", "TPCC_noBECCS", "TPCC_noEW")

# Define scenario aesthetics with Set1 colors
set1_colors <- brewer.pal(9, "Set1")
scenario_labels <- tribble(
  ~Scenario, ~Label, ~Color, ~Linetype, ~Shape,
  "TPCC", "TPCC", set1_colors[1], "solid", 16,
  "TPCC_a", "TPCC_a", set1_colors[2], "dashed", 17,
  "TPCC_DAChigh", "TPCC_DAChigh", set1_colors[3], "dotted", 15,
  "TPCC_hydhigh", "TPCC_hydhigh", set1_colors[4], "dotdash", 18,
  "TPCC_noBECCS", "TPCC_noBECCS", set1_colors[5], "longdash", 8,
  "TPCC_noEW", "TPCC_noEW", set1_colors[6], "twodash", 3
)

# Create manual scales
scenario_colors <- setNames(scenario_labels$Color, scenario_labels$Scenario)
scenario_linetypes <- setNames(scenario_labels$Linetype, scenario_labels$Scenario)
scenario_shapes <- setNames(scenario_labels$Shape, scenario_labels$Scenario)

# Plot theme
plot_theme_white <- theme_bw() +
  theme(
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.line = element_line(colour = 'black', linewidth = 0.25),
    panel.background = element_rect(fill = NA),
    strip.background = element_rect(fill = NA, colour = NA),
    strip.text = element_text(size = 6, colour = 'black', face = 'plain'),
    axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 6),
    axis.title = element_text(size = 6),
    legend.text = element_text(size = 8),
    legend.title = element_blank(),
    legend.key.height = unit(8.5, 'pt'),
    legend.key.width = unit(7, 'pt'),
    axis.ticks = element_line(colour = 'black', linewidth = 0.25),
    plot.title = element_text(size = 8),
    plot.tag = element_text(size = 7, face = 'bold'),
    plot.margin = margin(1, 1, 1, 1)
  )

# ============================================================================
# PART 2A: CALCULATE BECCS AND FOSSIL CCS REGIONAL THRESHOLDS
# ============================================================================
# (Must be done here after df_load_data is available)

# Extract 2050 values for BECCS - using average across all scenarios
potential_beccs_2050 <- df_load_data %>%
  filter(variable == "Car_Rem_Bio_wit_CCS", Year == 2050) %>%
  mutate(value_Gt = value / 1000) %>%  # Convert MtCO2 to GtCO2
  group_by(Region) %>%
  summarise(potential_Gt_yr = mean(value_Gt, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Region) %>%
  mutate(technology = "BECCS")

# Extract 2050 values for Fossil CCS - using average across all scenarios
potential_fossil_ccs_2050 <- df_load_data %>%
  filter(variable == "Car_Seq_CCS_Fos", Year == 2050) %>%
  mutate(value_Gt = value / 1000) %>%  # Convert MtCO2 to GtCO2
  group_by(Region) %>%
  summarise(potential_Gt_yr = mean(value_Gt, na.rm = TRUE), .groups = "drop") %>%
  rename(Sr33 = Region) %>%
  mutate(technology = "Fossil_CCS")

# Get World totals for scaling
world_beccs <- potential_beccs_2050 %>%
  filter(Sr33 == "World") %>%
  pull(potential_Gt_yr)

world_fossil_ccs <- potential_fossil_ccs_2050 %>%
  filter(Sr33 == "World") %>%
  pull(potential_Gt_yr)

# Calculate regional thresholds for BECCS (IPCC global: 3-7 GtCO2/yr)
thresholds_beccs <- potential_beccs_2050 %>%
  mutate(
    potential = potential_Gt_yr,
    med_threshold = 3 * potential_Gt_yr / world_beccs,
    high_threshold = 7 * potential_Gt_yr / world_beccs,
    med_global = 3,
    high_global = 7,
    global_potential = world_beccs,
    unit = "GtCO2/yr"
  ) %>%
  select(Sr33, technology, potential, med_threshold, high_threshold, med_global, high_global, global_potential, unit)

# Calculate regional thresholds for Fossil CCS (IPCC global: 3.8-8.8 GtCO2/yr)
thresholds_fossil_ccs <- potential_fossil_ccs_2050 %>%
  mutate(
    potential = potential_Gt_yr,
    med_threshold = 3.8 * potential_Gt_yr / world_fossil_ccs,
    high_threshold = 8.8 * potential_Gt_yr / world_fossil_ccs,
    med_global = 3.8,
    high_global = 8.8,
    global_potential = world_fossil_ccs,
    unit = "GtCO2/yr"
  ) %>%
  select(Sr33, technology, potential, med_threshold, high_threshold, med_global, high_global, global_potential, unit)

cat("\n==========================================\n")
cat("BECCS and Fossil CCS Regional Thresholds Calculated\n")
cat("==========================================\n")

# Combine with existing thresholds from PART 1
all_regional_thresholds <- bind_rows(
  all_regional_thresholds,  # Existing thresholds (Wind, Solar, Bio, CCS geol)
  thresholds_beccs,
  thresholds_fossil_ccs
)

# ============================================================================
# UPDATED FUNCTION DEFINITIONS
# ============================================================================


create_absolute_value_plot_regional <- function(var_name, title, unit_label, 
                                                technology_name, current_region,
                                                convert_to_gt = FALSE) {
  
  # DEBUG: Print what we're looking for
  cat(sprintf("DEBUG: Looking for technology='%s', region='%s'\n", technology_name, current_region))
  
  # Get thresholds for this region and technology
  region_thresholds <- all_regional_thresholds %>%
    filter(Sr33 == current_region, technology == technology_name)
  
  # DEBUG: Show what we found
  cat(sprintf("DEBUG: Found %d rows\n", nrow(region_thresholds)))
  if(nrow(region_thresholds) > 0) {
    cat("DEBUG: Threshold values:\n")
    print(region_thresholds %>% select(Sr33, technology, med_threshold, high_threshold))
  }
  
  if(nrow(region_thresholds) == 0) {
    cat("Warning: No thresholds found for", technology_name, "in", current_region, "\n")
    return(NULL)
  }
  
  y_medium <- region_thresholds$med_threshold[1]
  y_high <- region_thresholds$high_threshold[1]
  
  # DEBUG: Print the actual threshold values being used
  cat(sprintf("DEBUG: Using y_medium=%.2f, y_high=%.2f\n\n", y_medium, y_high))
  
  plot_data <- df_load_data_region %>% filter(variable == var_name)
  
  if (convert_to_gt) {
    plot_data <- plot_data %>% mutate(value = value / 1000)
  }
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    filter(Scenario %in% cmain_plot) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = Inf,
             fill = "mediumpurple1", alpha = 0.2) + 
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year, y = value, color = Scenario, shape = Scenario)) +
    geom_line(aes(x = Year, y = value, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = unit_label, title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_continuous(breaks = seq(2000, 2100, by = 10))
}


# Function for plots with fixed global thresholds (not regional)
create_absolute_value_plot <- function(var_name, title, y_medium, y_high, unit_label,
                                       convert_to_gt = FALSE) {
  
  plot_data <- df_load_data_region %>% filter(variable == var_name)
  
  if (convert_to_gt) {
    plot_data <- plot_data %>% mutate(value = value / 1000)
  }
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    filter(Scenario %in% cmain_plot) %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = Inf,
             fill = "mediumpurple1", alpha = 0.2) + 
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year, y = value, color = Scenario, shape = Scenario)) +
    geom_line(aes(x = Year, y = value, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = unit_label, title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_continuous(breaks = seq(2000, 2100, by = 10))
}


# Keep all other function definitions from your original script
create_decadal_change_plot <- function(var_name, title, y_medium, y_high, 
                                       y_label = "10-Year Change (%)",
                                       positive_threshold = FALSE) {
  
  plot_data <- df_load_data_region %>% 
    filter(variable == var_name) %>%
    group_by(Model, Scenario, Region) %>%
    arrange(Year) %>%
    mutate(decadal_pct_change = ((value / lag(value, n = 2)) - 1) * 100,
           Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))) %>%
    filter(Scenario %in% cmain_plot, !is.na(decadal_pct_change))
  
  if(nrow(plot_data) == 0) return(NULL)
  
  if(positive_threshold) {
    plot_data %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
               fill = "skyblue", alpha = 0.2) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = Inf,
               fill = "mediumpurple1", alpha = 0.2) + 
      geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
      geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
      geom_point(aes(x = Year_label, y = decadal_pct_change, color = Scenario, shape = Scenario), size = 1.5) +
      geom_line(aes(x = Year_label, y = decadal_pct_change, group = Scenario, 
                    color = Scenario, linetype = Scenario)) +
      labs(x = NULL, y = y_label, title = title) +
      plot_theme_white +
      scale_color_manual(values = scenario_colors) +
      scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
      scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
  } else {
    plot_data %>%
      ggplot() +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
               fill = "skyblue", alpha = 0.2) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = -Inf,
               fill = "mediumpurple1", alpha = 0.2) + 
      geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
      geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
      geom_point(aes(x = Year_label, y = decadal_pct_change, color = Scenario, shape = Scenario), size = 1.5) +
      geom_line(aes(x = Year_label, y = decadal_pct_change, group = Scenario, 
                    color = Scenario, linetype = Scenario)) +
      labs(x = NULL, y = y_label, title = title) +
      plot_theme_white +
      scale_color_manual(values = scenario_colors) +
      scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
      scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
  }
}

create_investment_ratio_plot <- function(mitigation_vars, baseline_vars, title, 
                                         y_medium, y_high) {
  
  mitigation_data <- df_load_data_region %>%
    filter(variable %in% mitigation_vars, Scenario %in% cmain) %>%
    group_by(Model, Scenario, Region, Year) %>%
    summarise(mitigation_inv = sum(value, na.rm = TRUE), .groups = 'drop')
  
  baseline_data <- df_load_data_region %>%
    filter(variable %in% baseline_vars, Scenario == "CurPol") %>%
    group_by(Model, Region, Year) %>%
    summarise(baseline_inv = sum(value, na.rm = TRUE), .groups = 'drop')
  
  if(nrow(baseline_data) == 0) {
    plot_data <- mitigation_data %>%
      mutate(inv_ratio = 1, Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))) %>%
      filter(Scenario != "CurPol")
  } else {
    plot_data <- mitigation_data %>%
      left_join(baseline_data, by = c("Model", "Region", "Year")) %>%
      mutate(
        baseline_inv = ifelse(is.na(baseline_inv), 0, baseline_inv),
        inv_ratio = ifelse(baseline_inv == 0, 1, mitigation_inv / baseline_inv),
        Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))
      ) %>%
      filter(Scenario != "CurPol")
  }
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = Inf,
             fill = "mediumpurple1", alpha = 0.2) +
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year_label, y = inv_ratio, color = Scenario, shape = Scenario), size = 1.5) +
    geom_line(aes(x = Year_label, y = inv_ratio, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = "Investment Ratio", title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
}

create_share_plot <- function(var_num, var_denom, title, y_medium, y_high) {
  
  plot_data <- df_load_data_region %>% 
    filter(variable %in% c(var_num, var_denom)) %>%
    distinct(Model, Scenario, Region, Year, variable, .keep_all = TRUE) %>%
    pivot_wider(names_from = variable, values_from = value) %>% 
    transmute(Model, Scenario, Region, Year,
              share = .data[[var_num]] / .data[[var_denom]] * 100) %>% 
    group_by(Model, Scenario, Region) %>%
    arrange(Year) %>%
    mutate(share_scaleup = share - lag(share, n = 2),
           Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))) %>%
    filter(Scenario %in% cmain_plot, !is.na(share_scaleup))
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = Inf,
             fill = "mediumpurple1", alpha = 0.2) + 
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year_label, y = share_scaleup, color = Scenario, shape = Scenario)) +
    geom_line(aes(x = Year_label, y = share_scaleup, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = "10-Year Share Change (pp)", title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
}

create_single_sector_energy_plot <- function(var_name, title, y_medium, y_high) {
  
  plot_data <- df_load_data_region %>% 
    filter(variable == var_name) %>%
    group_by(Model, Scenario, Region) %>%
    arrange(Year) %>%
    mutate(decadal_pct_change = ((value / lag(value, n = 2)) - 1) * 100,
           Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))) %>%
    filter(Scenario %in% cmain_plot, !is.na(decadal_pct_change))
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = -Inf,
             fill = "mediumpurple1", alpha = 0.2) + 
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year_label, y = decadal_pct_change, color = Scenario, shape = Scenario), 
               size = 1.5) +
    geom_line(aes(x = Year_label, y = decadal_pct_change, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = "10-Year Change (%)", title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
}

create_share_change_plot <- function(var_num, var_denom, title, y_medium, y_high) {
  
  plot_data <- df_load_data_region %>% 
    filter(variable %in% c(var_num, var_denom)) %>%
    distinct(Model, Scenario, Region, Year, variable, .keep_all = TRUE) %>%
    pivot_wider(names_from = variable, values_from = value) %>%
    transmute(Model, Scenario, Region, Year,
              share = .data[[var_num]] / .data[[var_denom]] * 100) %>%
    group_by(Model, Scenario, Region) %>%
    arrange(Year) %>%
    mutate(share_change = share - lag(share, n = 2),
           Year_label = paste0(Year - 10, "-", substr(Year, 3, 4))) %>%
    filter(Scenario %in% cmain_plot, !is.na(share_change))
  
  if(nrow(plot_data) == 0) return(NULL)
  
  plot_data %>%
    ggplot() +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_medium, ymax = y_high,
             fill = "skyblue", alpha = 0.2) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = y_high, ymax = -Inf,
             fill = "mediumpurple1", alpha = 0.2) + 
    geom_hline(yintercept = y_medium, linetype = "dashed", color = "lightblue", linewidth = 0.5) +
    geom_hline(yintercept = y_high, linetype = "dashed", color = "blue", linewidth = 0.5) +
    geom_point(aes(x = Year_label, y = share_change, color = Scenario, shape = Scenario), size = 1.5) +
    geom_line(aes(x = Year_label, y = share_change, group = Scenario, 
                  color = Scenario, linetype = Scenario)) +
    labs(x = NULL, y = "10-Year Change (pp)", title = title) +
    plot_theme_white +
    scale_color_manual(values = scenario_colors) +
    scale_linetype_manual(values = scenario_linetypes) +     scale_shape_manual(values = scenario_shapes) +
    scale_x_discrete(breaks = c("2010-20", "2030-40", "2050-60", "2070-80", "2090-00"))
}

# ============================================================================
# LOOP THROUGH REGIONS
# ============================================================================

regions_to_plot <- c("World","R5OECD90+EU", "R5ASIA", "R5LAM", "R5MAF", "R5REF")

cat("\n==========================================\n")
cat("Available regions in the dataset:\n")
cat("==========================================\n")
unique_regions <- unique(df_load_data$Region)
for(i in 1:length(unique_regions)) {
  cat(sprintf("%2d. %s\n", i, unique_regions[i]))
}
cat("==========================================\n\n")

cat("Regions to be plotted:\n")
for(i in 1:length(regions_to_plot)) {
  cat(sprintf("%2d. %s\n", i, regions_to_plot[i]))
}
cat("\n")

# Also add this check before the loop to verify the data structure
cat("\n==========================================\n")
cat("Verifying BECCS/Fossil CCS thresholds:\n")
cat("==========================================\n")
print(all_regional_thresholds %>%
        filter(technology %in% c("BECCS", "Fossil_CCS"), Sr33 == "R5ASIA") %>%
        select(Sr33, technology, potential, med_threshold, high_threshold))
cat("==========================================\n\n")

for(region_name in regions_to_plot) {
  
  cat("\n==========================================\n")
  cat("Processing region:", region_name, "\n")
  cat("==========================================\n")
  
  # Filter data for current region
  df_load_data_region <- df_load_data %>% 
    filter(Region == region_name) %>%
    mutate(Scenario = factor(Scenario, levels = c("CurPol", "TPCC", "TPCC_a", 
                                                  "TPCC_DAChigh", "TPCC_hydhigh", 
                                                  "TPCC_noBECCS", "TPCC_noEW")))
  
  if(nrow(df_load_data_region) == 0) {
    cat("âš  No data found for region:", region_name, "\n")
    next
  }
  
  cat("âœ“ Data rows found:", nrow(df_load_data_region), "\n")
  
  # CREATE ALL PLOTS - UPDATED ROW 1 WITH REGIONAL THRESHOLDS
  g_biomass <- create_absolute_value_plot_regional('Prm_Ene_Bio', "Biomass Potential", "EJ/yr", 
                                                   "Bioenergy", region_name)
  g_wind_pot <- create_absolute_value_plot_regional('Prm_Ene_Win', "Wind Potential", "EJ/yr", 
                                                    "Wind", region_name)
  g_solar_pot <- create_absolute_value_plot_regional('Prm_Ene_Solar', "Solar Potential", "EJ/yr", 
                                                     "Solar", region_name)
  g_ccs_geol <- create_absolute_value_plot_regional('Cum_Car_Seq_CCS', "CCS Geological Potential", "GtCO2", 
                                                    "CCS", region_name, convert_to_gt = TRUE)
  
  # Row 2: ECONOMIC FEASIBILITY (unchanged - still using global thresholds)
  g_gdp_loss <- create_absolute_value_plot('Pol_Cos_GDP_Los_rat', "GDP Loss", 5, 10, "GDP Loss (%)")
  g_carbon_price <- create_absolute_value_plot('Prc_Car_NPV_5pc', "Carbon Price", 60, 120, "USD/tCO2")
  g_energy_inv <- create_investment_ratio_plot(
    mitigation_vars = c('Inv_Ene_Sup', 'Inv_Ene_Dem_Eff_and_Dec'),
    baseline_vars = c('Inv_Ene_Sup', 'Inv_Ene_Dem_Eff_and_Dec'),
    "Energy Investments", 1.2, 1.5
  )
  
  # Row 3 & 4 & 5: TECHNOLOGICAL FEASIBILITY (unchanged)
  g_wind <- create_share_plot('Sec_Ene_Ele_Win', 'Sec_Ene_Ele', "Wind Scale-up", 10, 20)
  g_solar <- create_share_plot('Sec_Ene_Ele_Solar', 'Sec_Ene_Ele', "Solar Scale-up", 10, 20)
  g_nuclear <- create_share_plot('Sec_Ene_Ele_Nuc', 'Sec_Ene_Ele', "Nuclear Scale-up", 5, 10)
  g_beccs <- create_absolute_value_plot_regional('Car_Rem_Bio_wit_CCS', "BECCS Scale-up", "GtCO2/yr", 
                                                 "BECCS", region_name, convert_to_gt = TRUE)
  g_biofuel_tra <- create_share_plot('Fin_Ene_Tra_Liq_Bio', 'Fin_Ene_Tra', "Biofuels in Transport", 5, 10)
  g_elec_tra <- create_share_plot('Fin_Ene_Tra_Ele', 'Fin_Ene_Tra', "Electricity in Transport", 10, 15)
  g_hydrogen <- create_share_plot('Fin_Ene_Tra_Hyd', 'Fin_Ene_Tra', "Hydrogen Scale-up", 10, 20)
  g_dac <- create_share_plot('Sec_Ene_Inp_Ele_DAC', 'Sec_Ene_Ele', "DAC Scale-up", 10, 20)
  g_fossil_ccs <- create_absolute_value_plot_regional('Car_Seq_CCS_Fos', "Fossil CCS Scale-up", "GtCO2/yr", 
                                                      "Fossil_CCS", region_name, convert_to_gt = TRUE)
  
  # Row 5 & 6: SOCIO-CULTURAL FEASIBILITY (unchanged)
  g_total_energy <- create_single_sector_energy_plot('Fin_Ene', "Total Energy Demand", -10, -20)
  g_industry_energy <- create_single_sector_energy_plot('Fin_Ene_Ind', "Industry Energy Demand", -10, -20)
  g_transport_energy <- create_single_sector_energy_plot('Fin_Ene_Tra', "Transport Energy Demand", -10, -20)
  g_residential_energy <- create_single_sector_energy_plot('Fin_Ene_Res_and_Com', "Residential Energy Demand", -10, -20)
  g_livestock <- create_share_change_plot('Agr_Dem_Liv_Foo', 'Agr_Dem_Foo', "Livestock Share", -0.5, -1)
  g_forest <- create_decadal_change_plot('Lan_Cov_Frs', "Forest Cover", 2, 5, "10-Year Change (%)", positive_threshold = TRUE)
  g_pasture <- create_decadal_change_plot('Lan_Cov_Pst', "Pasture Cover", -5, -10, "10-Year Change (%)")
  
  # Extract legend
  legend <- NULL
  first_valid_plot <- if(!is.null(g_wind)) g_wind else if(!is.null(g_solar)) g_solar else NULL
  
  if(!is.null(first_valid_plot)) {
    first_valid_plot <- first_valid_plot +
      guides(linetype = guide_legend(ncol = 1, title = NULL, reverse = FALSE),
             color = guide_legend(ncol = 1, title = NULL),
             shape = guide_legend(ncol = 1, title = NULL))
    legend <- get_legend(first_valid_plot + theme(legend.position = 'right'))
  }
  
  # Calculate row heights and positions
  row_height <- 0.15
  row_spacing <- 0.01
  row6_y <- 0.01
  row5_y <- row6_y + row_height + row_spacing
  row4_y <- row5_y + row_height + row_spacing
  row3_y <- row4_y + row_height + row_spacing
  row2_y <- row3_y + row_height + row_spacing
  row1_y <- row2_y + row_height + row_spacing
  plot_width <- 0.23
  
  legend_x <- 0.73
  legend_width <- 0.13
  
  # CREATE COMBINED FIGURE
  g_combined <- ggdraw() +
    # Row 1: GEOLOGICAL FEASIBILITY (now with regional thresholds)
    draw_plot(g_biomass + theme(legend.position = "none"), 0.01, row1_y, plot_width, row_height) +
    draw_plot(g_wind_pot + theme(legend.position = "none"), 0.26, row1_y, plot_width, row_height) +
    draw_plot(g_solar_pot + theme(legend.position = "none"), 0.51, row1_y, plot_width, row_height) +
    draw_plot(g_ccs_geol + theme(legend.position = "none"), 0.76, row1_y, plot_width, row_height) +
    
    # Row 2: ECONOMIC FEASIBILITY
    draw_plot(g_gdp_loss + theme(legend.position = "none"), 0.01, row2_y, plot_width, row_height) +
    draw_plot(g_carbon_price + theme(legend.position = "none"), 0.26, row2_y, plot_width, row_height) +
    draw_plot(g_energy_inv + theme(legend.position = "none"), 0.51, row2_y, plot_width, row_height) +
    draw_plot(legend, x = legend_x, y = row2_y, width = legend_width, height = row_height) +
    draw_label(region_name, x = 0.88, y = row2_y + row_height/1.3, 
               size = 12, fontface = "bold", hjust = 0.5) +
    
    # Row 3: TECHNOLOGICAL FEASIBILITY (Part 1)
    draw_plot(g_wind + theme(legend.position = "none"), 0.01, row3_y, plot_width, row_height) +
    draw_plot(g_solar + theme(legend.position = "none"), 0.26, row3_y, plot_width, row_height) +
    draw_plot(g_nuclear + theme(legend.position = "none"), 0.51, row3_y, plot_width, row_height) +
    draw_plot(g_beccs + theme(legend.position = "none"), 0.76, row3_y, plot_width, row_height) +
    
    # Row 4: TECHNOLOGICAL FEASIBILITY (Part 2)
    draw_plot(g_biofuel_tra + theme(legend.position = "none"), 0.01, row4_y, plot_width, row_height) +
    draw_plot(g_elec_tra + theme(legend.position = "none"), 0.26, row4_y, plot_width, row_height) +
    draw_plot(g_hydrogen + theme(legend.position = "none"), 0.51, row4_y, plot_width, row_height) +
    draw_plot(g_dac + theme(legend.position = "none"), 0.76, row4_y, plot_width, row_height) +
    
    # Row 5: TECHNOLOGICAL (Fossil CCS) & SOCIO-CULTURAL (Part 1)
    draw_plot(g_fossil_ccs + theme(legend.position = "none"), 0.01, row5_y, plot_width, row_height) +
    draw_plot(g_total_energy + theme(legend.position = "none"), 0.26, row5_y, plot_width, row_height) +
    draw_plot(g_industry_energy + theme(legend.position = "none"), 0.51, row5_y, plot_width, row_height) +
    draw_plot(g_transport_energy + theme(legend.position = "none"), 0.76, row5_y, plot_width, row_height) +
    
    # Row 6: SOCIO-CULTURAL FEASIBILITY (Part 2)
    draw_plot(g_residential_energy + theme(legend.position = "none"), 0.01, row6_y, plot_width, row_height) +
    draw_plot(g_livestock + theme(legend.position = "none"), 0.26, row6_y, plot_width, row_height) +
    draw_plot(g_forest + theme(legend.position = "none"), 0.51, row6_y, plot_width, row_height) +
    draw_plot(g_pasture + theme(legend.position = "none"), 0.76, row6_y, plot_width, row_height) +
    
    # Add plot labels
    draw_plot_label(
      label = c('a', 'b', 'c', 'd',
                'e', 'f', 'g',
                'h', 'i', 'j', 'k',
                'l', 'm', 'n', 'o',
                'p','q', 'r', 's' ,
                't','u', 'v', 'w'),
      x = c(0, 0.25, 0.50, 0.75,
            0, 0.25, 0.50,
            0, 0.25, 0.50, 0.75,
            0, 0.25, 0.50, 0.75,
            0, 0.25, 0.50, 0.75,
            0, 0.25, 0.50, 0.75),
      y = c(rep(row1_y + row_height - 0.01, 4),
            rep(row2_y + row_height - 0.01, 3),
            rep(row3_y + row_height - 0.01, 4),
            rep(row4_y + row_height - 0.01, 4),
            rep(row5_y + row_height - 0.01, 4),
            rep(row6_y + row_height - 0.01, 4)),
      size = 10
    )
  
  # Display the plot
  plot(g_combined)
  
  # Save the figure
  filename <- paste0('g_feasibility_combined_', gsub("\\+", "_", region_name), '.png')
  ggsave(filename = filename, g_combined, 
         width = 350, height = 450, units = 'mm', dpi = 500)
  
  cat("âœ“ Saved:", filename, "\n")
}

cat("\n==========================================\n")
cat("All regions processed!\n")
cat("==========================================\n")

# Print summary of thresholds
cat("\n==========================================\n")
cat("Regional Thresholds Summary:\n")
cat("==========================================\n")
print(all_regional_thresholds %>%
        filter(Sr33 %in% regions_to_plot) %>%
        select(Sr33, technology, potential, med_threshold, high_threshold) %>%
        arrange(technology, Sr33))
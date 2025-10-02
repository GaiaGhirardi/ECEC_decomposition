#'############################################'
#' NEPS - GROUP DISPARITIES DECOMPOSITION          
#'############################################'

#'  RIGHT AFTER 2A_DECOMPOSITION_1

#### Custom Axis Labels ####
y_axis_effect     <- "Effect Estimate"
y_axis_selection  <- "Cov(D, \u03C4)"  # Unicode for tau
y_axis_prevalence <- "Prevalence Estimate"

# Outcome labels for the prevalence graph (childcare arrangements in Math)
# and labels for the other graphs as well.
outcome_labels <- c(ecec = "Center-based ECEC", 
                    fdc  = "Family Day Care", 
                    gran = "Grandparents/Relatives", 
                    par  = "Parental Care Only")
# Create labels for the 'childcare' variable (uppercase keys)
childcare_labels <- c("ECEC" = "Center-based ECEC",
                      "FDC"  = "Family Day Care",
                      "GRAN" = "Grandparents/Relatives",
                      "PAR"  = "Parental Care Only")

#### Functions for Data Extraction ####

# For ATE (effect) data: extract rows for ATE_G1, ATE_G0, and ATE_G1-ATE_G0,
# add a column for the childcare arrangement, and recode group names.
extract_effect_data <- function(result_obj, childcare) {
  result_obj$results_specific %>%
    tibble::rownames_to_column(var = "measure") %>%
    filter(measure %in% c("ATE_G1", "ATE_G0", "ATE_G1-ATE_G0")) %>%
    mutate(childcare = childcare,
           group_label = case_when(
             measure == "ATE_G1"       ~ "High SES",
             measure == "ATE_G0"       ~ "Low SES",
             measure == "ATE_G1-ATE_G0"  ~ "Difference"
           ))
}

# For Selection (Cov) data: extract rows for Cov_G1 and Cov_G0,
# add a column for the childcare arrangement, and recode group names.
extract_selection_data <- function(result_obj, childcare) {
  result_obj$results_specific %>%
    tibble::rownames_to_column(var = "measure") %>%
    filter(measure %in% c("Cov_G1", "Cov_G0")) %>%
    mutate(childcare = childcare,
           group_label = case_when(
             measure == "Cov_G1" ~ "High SES",
             measure == "Cov_G0" ~ "Low SES"
           ))
}

# For Prevalence data: extract all rows and add an outcome (childcare) identifier.
extract_specific_measures <- function(result_obj, outcome_name) {
  result_obj$results_specific %>%
    tibble::rownames_to_column(var = "measure") %>%
    mutate(outcome = outcome_name)
}

#### 1. Data Preparation for Math (ATE and Selection) ####

# -- ATE (Effect) Data for Math --
effect_math_ecec <- extract_effect_data(EDUM_math_ecec_pa, "ECEC")
effect_math_fdc  <- extract_effect_data(EDUM_math_fdc_pa, "FDC")
effect_math_gran <- extract_effect_data(EDUM_math_gran_pa, "GRAN")
effect_math_par  <- extract_effect_data(EDUM_math_par_pa, "PAR")

effect_math <- bind_rows(effect_math_ecec, effect_math_fdc, effect_math_gran, effect_math_par)
# Separate group-specific effects (bars) and difference (scatter)
effect_bars_math <- effect_math %>% filter(group_label %in% c("High SES", "Low SES"))
effect_diff_math <- effect_math %>% filter(group_label == "Difference")

# -- Selection Data for Math --
sel_math_ecec <- extract_selection_data(EDUM_math_ecec_pa, "ECEC")
sel_math_fdc  <- extract_selection_data(EDUM_math_fdc_pa, "FDC")
sel_math_gran <- extract_selection_data(EDUM_math_gran_pa, "GRAN")
sel_math_par  <- extract_selection_data(EDUM_math_par_pa, "PAR")

sel_math <- bind_rows(sel_math_ecec, sel_math_fdc, sel_math_gran, sel_math_par)

#### 2. Data Preparation for Voc (ATE and Selection) ####

# -- ATE (Effect) Data for Voc --
effect_voc_ecec <- extract_effect_data(EDUM_voc_ecec_pa, "ECEC")
effect_voc_fdc  <- extract_effect_data(EDUM_voc_fdc_pa, "FDC")
effect_voc_gran <- extract_effect_data(EDUM_voc_gran_pa, "GRAN")
effect_voc_par  <- extract_effect_data(EDUM_voc_par_pa, "PAR")

effect_voc <- bind_rows(effect_voc_ecec, effect_voc_fdc, effect_voc_gran, effect_voc_par)
# Separate group-specific effects and difference
effect_bars_voc <- effect_voc %>% filter(group_label %in% c("High SES", "Low SES"))
effect_diff_voc <- effect_voc %>% filter(group_label == "Difference")

# -- Selection Data for Voc --
sel_voc_ecec <- extract_selection_data(EDUM_voc_ecec_pa, "ECEC")
sel_voc_fdc  <- extract_selection_data(EDUM_voc_fdc_pa, "FDC")
sel_voc_gran <- extract_selection_data(EDUM_voc_gran_pa, "GRAN")
sel_voc_par  <- extract_selection_data(EDUM_voc_par_pa, "PAR")

sel_voc <- bind_rows(sel_voc_ecec, sel_voc_fdc, sel_voc_gran, sel_voc_par)

#### 3. Data Preparation for Prevalence (Math) ####
# Use Math results (either Math or Voc is fine for prevalence).
spec_ecec <- extract_specific_measures(EDUM_math_ecec_pa, "ecec")
spec_fdc  <- extract_specific_measures(EDUM_math_fdc_pa, "fdc")
spec_gran <- extract_specific_measures(EDUM_math_gran_pa, "gran")
spec_par  <- extract_specific_measures(EDUM_math_par_pa, "par")

all_spec <- bind_rows(spec_ecec, spec_fdc, spec_gran, spec_par)

d_data <- all_spec %>% 
  filter(measure %in% c("D_G1", "D_G0", "D_G1-D_G0")) %>%
  mutate(group_label = case_when(
    measure == "D_G1"      ~ "High SES",
    measure == "D_G0"      ~ "Low SES",
    measure == "D_G1-D_G0" ~ "Difference"
  ))

#### 4. Plotting ####

## ATE (Effect) Graph for Math ##
p_ate_math <- ggplot() +
  # Bars for group-specific effects (High SES and Low SES)
  geom_col(data = effect_bars_math, 
           aes(x = childcare, y = point, fill = group_label),
           position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(data = effect_bars_math,
            aes(x = childcare, y = point, label = sprintf("%.2f", point), group = group_label),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +    
  geom_errorbar(data = effect_bars_math, 
                aes(x = childcare, y = point, ymin = CI_lower, ymax = CI_upper, group = group_label),
                position = position_dodge(width = 0.7), width = 0.2, color = "black", alpha=.2) +  
  geom_point(data = effect_diff_math, 
             aes(x = childcare, y = point),
             size = 3, shape = 21, fill = "white", color = "black") +
  geom_errorbar(data = effect_diff_math, 
                aes(x = childcare, y = point, ymin = CI_lower, ymax = CI_upper),
                width = 0.2, color = "black") +
  scale_x_discrete(labels = childcare_labels) +
  labs(title = "Effect on Mathematics",
       x = "Childcare Arrangement",
       y = y_axis_effect,
       fill = "SES groups") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

## ATE (Effect) Graph for Voc ##
p_ate_voc <- ggplot() +
  geom_col(data = effect_bars_voc, 
           aes(x = childcare, y = point, fill = group_label),
           position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(data = effect_bars_voc,
            aes(x = childcare, y = point, label = sprintf("%.2f", point), group = group_label),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +  
  geom_errorbar(data = effect_bars_voc, 
                aes(x = childcare, y = point, ymin = CI_lower, ymax = CI_upper, group = group_label),
                position = position_dodge(width = 0.7), width = 0.2, color = "black", alpha=.2) +  
  geom_point(data = effect_diff_voc, 
             aes(x = childcare, y = point),
             size = 3, shape = 21, fill = "white", color = "black") +
  geom_errorbar(data = effect_diff_voc, 
                aes(x = childcare, y = point, ymin = CI_lower, ymax = CI_upper),
                width = 0.2, color = "black") +
  scale_x_discrete(labels = childcare_labels) +
  labs(title = "Effect on Vocabulary",
       x = "Childcare Arrangement",
       y = y_axis_effect,
       fill = "SES groups") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

## Selection Graph for Math ##
p_sel_math <- ggplot(sel_math, aes(x = childcare, y = point, fill = group_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(data = sel_math,
            aes(x = childcare, y = point, label = sprintf("%.2f", point), group = group_label),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +  
  scale_x_discrete(labels = childcare_labels) +
  labs(title = "Selection - Mathematics",
       x = "Childcare Arrangement",
       y = y_axis_selection,
       fill = "SES groups") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

## Selection Graph for Voc ##
p_sel_voc <- ggplot(sel_voc, aes(x = childcare, y = point, fill = group_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(data = sel_voc,
            aes(x = childcare, y = point, label = sprintf("%.2f", point), group = group_label),
            position = position_dodge(width = 0.7),
            vjust = -0.5, size = 3) +  
  scale_x_discrete(labels = childcare_labels) +
  labs(title = "Selection - Vocabulary",
       x = "Childcare Arrangement",
       y = y_axis_selection,
       fill = "SES groups") +
  theme_minimal() +
  theme(text = element_text(size = 12),
        plot.title = element_text(face = "bold", size = 14))

## Prevalence Graphs (using Math results) ##
# Graph including the Difference group
p_prevalence_all <- ggplot(d_data, aes(x = outcome, y = point, fill = group_label)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper),
                width = 0.2,
                position = position_dodge(width = 0.7)) +
  geom_text(aes(label = round(point, 2)),
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            hjust = -0.1,
            size = 3) +
  scale_x_discrete(labels = outcome_labels) +
  theme_minimal() +
  labs(title = "Prevalence",
       x = "Childcare Arrangement",
       y = y_axis_prevalence,
       fill = "SES groups") +
  theme(text = element_text(size = 12))

ggsave("Prevalence.png", plot = p_prevalence_all, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

#### 5. Arrange and Display the Plots ####

# Arrange ATE (Effect) graphs for Math and Voc vertically
hetate <- grid.arrange(p_ate_math, p_ate_voc, ncol = 1)
ggsave("Effect.png", plot = hetate, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")

# Arrange Selection graphs for Math and Voc vertically
hetsel <- grid.arrange(p_sel_math, p_sel_voc, ncol = 1)
ggsave("Selection.png", plot = hetsel, path=graphs, 
       dpi = 320, width = 12, height = 10, units = "in")
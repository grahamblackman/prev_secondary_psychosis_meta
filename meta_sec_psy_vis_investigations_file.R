########################################################################################
#                                                                                      #
# meta_sec_psy_vis_investigations_file.R from Blackman et al                                 #
#                                                                                      #
# "How common are secondary causes of psychosis?                                       # 
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
#                                                                                      #
# visualizations of proportion of secondary psychosis according to investigation       #
#                                                                                      #
#                                                                                      #
########################################################################################

#
# This script plots forest plot of the the estimated proportion for all causes of secondary psychosis
# restricted to studies that performed MRI, blood, CSF, EEG and urine analysis on all patients
#

# plot with forester ------------------------------------------------------

plot_with_forester_investigation <- function(meta_df) {
  # Plots meta-analytic data into a forest plot produced by forester
  #
  # Arguments:
  #  # meta_df: dataframe with data
  
  
  # Sort the dataframe by the largest estimate first - use arrange(desc(prop)) for reverse order
  meta_df <- meta_df %>%
    arrange(prop)
  
  # Extract column for the left side column needed for forester
  meta_df_lsd <- meta_df %>%
    dplyr::select(Type = type, "Total cases" = total_cases, "Total N" = total_n, "k studies" = k)
  
  # Create formatted string for estimate and 95% CI e.g. "50.0% (40.1 - 55.2)"
  meta_df$estimate_str <- sprintf("%.2f%% (%.2f - %.2f)", meta_df$prop*100, meta_df$prop_lower*100, meta_df$prop_higher*100)
  
  # Extract columns for the right side column needed for forester
  meta_df_rsd <- meta_df %>%
    dplyr::select(Prevalence = estimate_str)
  
  #...and plot
  forester(left_side_data = meta_df_lsd,
           right_side_data = meta_df_rsd,
           estimate = meta_df$prop,
           ci_low = meta_df$prop_lower,
           ci_high = meta_df$prop_higher,
           stripe_colour = "#ffffff",
           estimate_precision = 2,
           font_family = "sans",
           point_sizes = 2,
           xlim = c(0, .4),
           xbreaks = c(0,.10,.20,.30,.40),
           display = FALSE,
           ggplot_width = 20,
           dpi = 600,
           file_path = here::here("output/forester_sec_investigations.png"))
}

# Create empty dataframe to store results
investigation_summary_df <- data.frame(type=character(),
                                       total_cases = numeric(),
                                       total_n = numeric(),
                                       k = numeric(),
                                       prop = numeric(),
                                       prop_lower = numeric(),
                                       prop_higher = numeric(),
                                       stringsAsFactors =FALSE)

# Extract raw proportions from meta-analytic objects using the extract_proportions function
# and add each one to the bottom of the summary_df dataframe

extractpr_invest <- extract_proportions(pes_sec_psych_summary_by_urine_all, "Urine")
investigation_summary_df[nrow(investigation_summary_df)+1,] <- extractpr_invest 

extractpr_invest <- extract_proportions(pes_sec_psych_summary_by_blood_all, "Blood")
investigation_summary_df[nrow(investigation_summary_df)+1,] <- extractpr_invest 

extractpr_invest <- extract_proportions(pes_sec_psych_summary_by_mri_all, "MRI")
investigation_summary_df[nrow(investigation_summary_df)+1,] <- extractpr_invest 

extractpr_invest <- extract_proportions(pes_sec_psych_summary_by_csf_all, "CSF")
investigation_summary_df[nrow(investigation_summary_df)+1,] <- extractpr_invest 

extractpr_invest <- extract_proportions(pes_sec_psych_summary_by_eeg_all, "EEG")
investigation_summary_df[nrow(investigation_summary_df)+1,] <- extractpr_invest 

# Change relevant columns to numeric)
investigation_summary_df$prop <- as.numeric(investigation_summary_df$prop)
investigation_summary_df$prop_lower <- as.numeric(investigation_summary_df$prop_lower)
investigation_summary_df$prop_higher <- as.numeric(investigation_summary_df$prop_higher)

plot_with_forester_investigation(investigation_summary_df)




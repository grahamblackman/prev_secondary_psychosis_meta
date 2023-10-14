########################################################################################
#                                                                                      #
# meta_mri_visualization_to_file.R from Blackman et al                                 #
#                                                                                      #
# "How common are secondary causes of psychosis?                                       # 
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
# visualizations of proportion of each subtype within a single forest plot             #                                                            #
#                                                                                      #
########################################################################################


# extract proportions from meta analysis  ---------------------------------


extract_proportions <- function(meta_obj, name){
  # Extract proportions and other meta-analytic information from the metaprop object
  #
  # Arguments:
  #  meta_obj: metaprop meta-analysis object
  #  name: name of subtype for labelling
  #

  # Calculate total cases and total N for studies that report cases (i.e. don't have NA for cases)
  total_cases <- 0
  total_n <- 0
  for (i in 1:length(meta_obj$n)) {
    if (!is.na(meta_obj$event[i])) {
      total_cases <- total_cases + meta_obj$event[i]
      total_n <- total_n + meta_obj$n[i]
    }
  }
    
  # Extract estimate and 95% CIs
  TE_random_vals <- c(meta_obj$TE.random,
                      meta_obj$lower.random,
                      meta_obj$upper.random)
  
  # Backtransform these three values to proportions
  vals <- unlist(lapply(TE_random_vals, meta:::backtransf, sm = "PFT", n = 1/mean(1/meta_obj$n)))

  # Return, name of subtype proportion (e.g. 'metabolic', total cases, total N,
  #  k studies included, and backtransformed estimate and upper and lower CI
  append(name, c(total_cases, total_n, meta_obj$k, vals))
}


# plot with forester ------------------------------------------------------

plot_with_forester <- function(meta_df) {
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
           estimate_precision = 5,
           font_family = "sans",
           point_sizes = 2,
           xlim = c(0, .2),
           xbreaks = c(0,.10,.20),
           display = FALSE,
           file_path = here::here("output/forester_sec_subtypes.png"))
}


# Create empty dataframe to store results
summary_df <- data.frame(type=character(),
                         total_cases = numeric(),
                         total_n = numeric(),
                         k = numeric(),
                         prop = numeric(),
                         prop_lower = numeric(),
                         prop_higher = numeric(),
                         stringsAsFactors=FALSE)

# Extract raw proportions from meta-analytic objects using the extract_proportions function
# and add each one to the bottom of the summary_df dataframe

extractpr <- extract_proportions(pes_auto_sec_psych_summary, "Autoimmune")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_congen_sec_psych_summary, "Congenital")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_cerebrovas_sec_psych_summary, "Cerebrovascular")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_space_oc_sec_psych_summary, "Space Occupying")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_metabolic_sec_psych_summary, "Metabolic")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_dietary_sec_psych_summary, "Dietary")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_infec_sec_psych_summary, "Infection")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_degen_sec_psych_summary, "Degenerative")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_demye_sec_psych_summary, "Demyelination")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_seizure_sec_psych_summary, "Seizure")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_endocrine_sec_psych_summary, "Endocrine")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_toxic_sec_psych_summary, "Drug Induced/Toxic")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_iatrogenic_sec_psych_summary, "Iatrogenic")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_other_sub_sec_psych_summary, "Other")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_unknown_sub_sec_psych_summary, "Not Specified/Unknown")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_sleep_sec_psych_summary, "Sleep")
summary_df[nrow(summary_df)+1,] <- extractpr

extractpr <- extract_proportions(pes_head_sec_psych_summary, "Head Injury")
summary_df[nrow(summary_df)+1,] <- extractpr


# Change relevant columns to numeric)
summary_df$prop <- as.numeric(summary_df$prop)
summary_df$prop_lower <- as.numeric(summary_df$prop_lower)
summary_df$prop_higher <- as.numeric(summary_df$prop_higher)

plot_with_forester(summary_df)



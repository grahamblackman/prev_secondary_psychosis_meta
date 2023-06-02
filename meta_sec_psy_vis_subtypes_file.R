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

# Function to extract raw proportion and upper and lower CI from meta-analytic objects
# Outputs vector with name, propotion, upper and lower CI

extract_proportions <- function(meta_obj, name){
  (TE_random_vals <- c(meta_obj$TE.random,
                       meta_obj$lower.random,
                       meta_obj$upper.random))
  vals <- unlist(lapply(TE_random_vals, meta:::backtransf, sm="PFT", n=1/mean(1/meta_obj$n)))
  append(name, vals)
}

# Create empty dataframe to store results
summary_df <- data.frame(type=character(),
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

# Change relevant columns to numeric)
summary_df$prop <- as.numeric(summary_df$prop)
summary_df$prop_lower <- as.numeric(summary_df$prop_lower)
summary_df$prop_higher <- as.numeric(summary_df$prop_higher)

# Visualisation -----------------------------------------------------------

# Global options

options(scipen = 100, digits = 1)
options(max.print = 500000000)

DV <- c("","","","","","","","","","","","","","","")
IV <- summary_df$type
ES <- summary_df$prop
LCI <- summary_df$prop_lower
UCI <- summary_df$prop_higher

subtype_ab <- data.frame(DV, IV, ES, LCI, UCI)

if (write_to_file == 1) {
  png(file=paste('output/', 'forest_sec_psych_subtypes.png', sep=''), width = 10, height = 10, units = 'in', res = 200)
}

ggplot(data=subtype_ab, aes(x = reorder(IV,-ES), y = ES, ymin = LCI, ymax = UCI)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 1, size = 1) +  
  geom_errorbar(aes(ymin = LCI, ymax = UCI), width = 0.5, cex = 1) + 
  facet_wrap(~DV) +
  coord_flip() +  
  geom_point(shape = 15, size = 5) + 
  ggtitle("") + 
  xlab("") + 
  ylab("proportion all psychosis  ") + 
  scale_y_continuous(limits = c(0,.25), breaks = c(0,.05,.10,.15,.20,.25)) + 
  theme(line = element_line(colour = "white", size = 3),
        strip.background = element_rect(fill="white"),
        legend.position ="none",
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(colour = "Black", margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.title.y = element_text(colour = "Black", margin = margin(t = 0, r = 20, b = 0, l = 0)),
        plot.title = element_text(colour = "Black", margin = margin(t = 0, r = 0, b = 20, l = 0)),
        axis.text = element_text(size=24, color = "Black"),
        text = element_text(size = 24), plot.margin = margin(t = 2, r = 2, b = 2, l = 2, unit = "cm"))

if (write_to_file == 1) {
  dev.off()
}


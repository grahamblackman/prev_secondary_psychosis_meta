########################################################################################
#                                                                                      #
# meta_sec_psy_vis_file.R from Blackman et al                                          #
#                                                                                      #
# "How common are secondary causes of psychosis?                                       #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
#     meta analysis sub group analysis                                                 #
#                                                                                      #
########################################################################################

#
# This script calculates the meta analysis of proportions of secondary psychosis subtypes...
#
# Note:  metaprop and rma functions are used for running meta analysis (rma is needed for moderator and leave one out
#       sensitivity analyses) 
#       

# endogenous vs exogenous --------------------------------------------------------------------


# Endogenous 
data$endo <- as.numeric(data$endo)
(pes_endo_sec_psych_summary <- meta::metaprop(endo, tot_sample, author_year, data = data, sm = "PFT",
                                              method.tau = "DL", method.ci = "WS"))
# Exogenous
data$exo <- as.numeric(data$exo)
(pes_exo_sec_psych_summary <- meta::metaprop(exo, tot_sample, author_year, data = data, sm = "PFT",
                                             method.tau = "DL", method.ci = "WS"))

# Endogenous: subtypes --------------------------------------------------------------------

# Autoimmune
data$auto_freq <- as.numeric(data$auto_freq)
(pes_auto_sec_psych_summary <- meta::metaprop(auto_freq, tot_sample, author_year, data = data, sm = "PFT",
                                              method.tau = "DL", method.ci = "WS"))
# Congenital
data$congen_freq <- as.numeric(data$congen_freq)
(pes_congen_sec_psych_summary <- meta::metaprop(congen_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                method.tau = "DL", method.ci = "WS"))
# Cerebrovascular
data$cerebrovas_freq <- as.numeric(data$cerebrovas_freq	)
(pes_cerebrovas_sec_psych_summary <- meta::metaprop(cerebrovas_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                    method.tau = "DL", method.ci = "WS"))

# Space occupying
data$space_oc_freq <- as.numeric(data$space_oc_freq)
(pes_space_oc_sec_psych_summary <- meta::metaprop(space_oc_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                  method.tau = "DL", method.ci = "WS"))

# Metabolic
data$metabolic_freq <- as.numeric(data$metabolic_freq)
(pes_metabolic_sec_psych_summary <- meta::metaprop(metabolic_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                   method.tau = "DL", method.ci = "WS"))

# Dietary
data$dietary_freq <- as.numeric(data$dietary_freq)
(pes_dietary_sec_psych_summary <- meta::metaprop(dietary_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                 method.tau = "DL", method.ci = "WS"))

# Infection
data$infec_freq <- as.numeric(data$infec_freq)
(pes_infec_sec_psych_summary <- meta::metaprop(infec_freq, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS"))

# Degeneration
data$degen_freq <- as.numeric(data$degen_freq)
(pes_degen_sec_psych_summary <- meta::metaprop(degen_freq, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS"))

# Demyelinating
data$demye_freq <- as.numeric(data$demye_freq)
(pes_demye_sec_psych_summary <- meta::metaprop(demye_freq, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS"))

# Seizure
data$seizure_freq <- as.numeric(data$seizure_freq)
(pes_seizure_sec_psych_summary <- meta::metaprop(seizure_freq, tot_sample, author_year, data=data, sm = "PFT",
                                                 method.tau = "DL", method.ci = "WS"))

# Sleep
data$sleep_freq <- as.numeric(data$sleep_freq)
(pes_sleep_sec_psych_summary <- meta::metaprop(sleep_freq, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS"))

# Head injury
data$head_freq <- as.numeric(data$head_freq)
(pes_head_sec_psych_summary <- meta::metaprop(head_freq, tot_sample, author_year, data = data, sm = "PFT",
                                              method.tau = "DL", method.ci = "WS"))

# Endocrine
data$endocrine_freq <- as.numeric(data$endocrine_freq)
(pes_endocrine_sec_psych_summary <- meta::metaprop(endocrine_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                   method.tau = "DL", method.ci = "WS"))



# Exogenous: subtypes --------------------------------------------------------------------

# Toxic/illicit substance
data$toxic_freq <- as.numeric(data$toxic_freq)
(pes_toxic_sec_psych_summary <- meta::metaprop(toxic_freq, tot_sample, author_year, data = data, sm = "PFT",
                                               method.tau = "DL", method.ci = "WS"))

# Iatrogenic
data$iatrogenic_freq <- as.numeric(data$iatrogenic_freq)
(pes_iatrogenic_sec_psych_summary <- meta::metaprop(iatrogenic_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                    method.tau = "DL", method.ci = "WS"))

# Other/unknown: subtypes --------------------------------------------------------------------

# Other
data$other_sub_freq <- as.numeric(data$other_sub_freq)
(pes_other_sub_sec_psych_summary <- meta::metaprop(other_sub_freq, tot_sample, author_year, data = data, sm = "PFT",
                                                   method.tau = "DL", method.ci = "WS"))

# Unknown
data$unknown_sub_freq <- as.numeric(data$unknown_sub_freq)
(pes_unknown_sub_sec_psych_summary <- meta::metaprop(unknown_sub_freq, tot_sample, author_year, data = data, sm="PFT",
                                                     method.tau = "DL", method.ci = "WS"))


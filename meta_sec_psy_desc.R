########################################################################################
#                                                                                      #
# meta_sec_psy_desc.R from Blackman et al                                              #
#                                                                                      #
# Secondary causes of psychosis: A systematic review and meta-analysis                 #
#                                                                                      #                                                                                      
#                                                                                      #
#   descriptive analyses                                                               #
#                                                                                      #
#                                                                                      #
########################################################################################

# to do:
# cog_assess_stan_tool_freq  - uses 'yes' and 'no' rather than 'all' and 'some'


# This script calculates descriptive statistics
#
# Descriptive statistics relating to studies

# Calculate total number of studies 
tot_study <- sum(complete.cases(data$tot_sample))
sprintf("Total number of studies included: %s", tot_study)

# Calculate total number of number of studies reporting first-episode psychosis 
tot_study_fep <- count_if("yes",data$FEP) 
sprintf("Total number of number of studies reporting first-episode psychosis only cohort: %s", tot_study_fep)

# Calculate total number of number of studies reporting non-first-episode psychosis only samples
tot_study_non_fep_only <- tot_study-tot_study_fep 
sprintf("Total number of number of studies reporting on not exclusively first-episode psychosis only samples: %s", tot_study_non_fep_only)

# Calculate earliest and latest publication dates of included studies
study_year_min <- min(data$year, na.rm = TRUE)
sprintf("Earliest publication year of included study: %s", study_year_min)

study_year_max <- max(data$year, na.rm = TRUE)
sprintf("Latest publication year of included study: %s", study_year_max)

# Descriptive statistics relating to data reported in studies

# Pooled sample 

# Calculate total number of psychosis patients assessed for secondary psychosis
tot_sam <- sum(data$tot_sample, na.rm = TRUE)
sprintf("Pooled sample of psychosis patients assessed for secondary psychosis: %s", tot_sam)

# Calculate total number of psychosis patients assessed for secondary psychosis from FEP samples
tot_sam_fep <- sum(data_fep$tot_sample, na.rm = TRUE)
sprintf("Pooled sample (FEP studies only) assessed for secondary psychosis: %s", tot_sam_fep)

# Calculate total number of psychosis patients assessed for secondary psychosis from non-FEP only samples
tot_sam_non_fep_only <- tot_sam - tot_sam_fep
sprintf("Pooled sample (non-FEP only studies) assessed for secondary psychosis: %s", tot_sam_non_fep_only )

# Calculate minimum and maximum sample size reported across included studies
sam_size_min <- min(data$tot_sample, na.rm = TRUE)
sprintf("Minimum sample size reported in included studies: %s", sam_size_min)

sam_size_max <- max(data$tot_sample, na.rm = TRUE)
sprintf("Maximum sample size reported in included studies: %s", sam_size_max)

# clinical setting -----------------------------------------------------------

# number of studies by clinical setting 
gen_hos <- count_if("General Hospital", data$setting) 
sprintf("Studies in general hostpial setting: %.0f", gen_hos)

outpat_psych <- count_if("Psychiatric Clinic", data$setting) 
sprintf("Studies in a Psychiatric Clinic setting: %.0f", outpat_psych)

inpat_psych <- count_if("Psychiatric Hospital", data$setting) 
sprintf("Studies in a Psychiatric Hospital setting: %.0f", inpat_psych)

any_psych <- outpat_psych + inpat_psych
sprintf("Studies in any psychiatric setting: %.0f", any_psych)

mixture <- count_if("2 or more settings", data$setting) 
sprintf("Studies in a mixture of settings: %.0f", mixture)

# number of patients by clinical setting 
tot_sam_gen_hos<- sum(data_gen_hos$tot_sample, na.rm = TRUE)
sprintf("Pooled sample from studies restricted to A&E and other general hostpial settings: %.0f", tot_sam_gen_hos)

tot_sam_outpat_psych <- sum(data_outpat_psych$tot_sample, na.rm = TRUE)
sprintf("Pooled sample from studies restricted out patient psychiatric setting: %.0f", tot_sam_outpat_psych)

tot_sam_inpat_psych <- sum(data_inpat_psych$tot_sample, na.rm = TRUE)
sprintf("Pooled sample from studies restricted to patient psychiatric setting: %.0f", tot_sam_inpat_psych)

tot_sam_any_psych <- tot_sam_outpat_psych  + tot_sam_inpat_psych
sprintf("Pooled sample in any psych setting: %.0f", tot_sam_any_psych)

tot_sam_mixture <- sum(data_mixture$tot_sample, na.rm = TRUE)
sprintf("Pooled sample from studies using mixture of clinical settings t: %.0f", tot_sam_inpat_psych)

# Calculate and show number of studies by continent 

n_am <- count_if("n_am", data$continent)
sprintf("North American studies: %s", n_am)

s_am <- count_if("s_am", data$continent)
sprintf("South American studies: %s", s_am)

euro <- count_if("euro", data$continent)
sprintf("European studies: %s", euro)

afri <- count_if("afri", data$continent)
sprintf("African studies: %s", afri)

asia <- count_if("asia", data$continent)
sprintf("Asian studies: %s", asia)

aus <- count_if("aus", data$continent)
sprintf("Australsian studies: %s", aus)


# age characteristics --------------------------------------------------

# Number of studies with age reported
tot_study_age <- sum(complete.cases(data$mean_age))
sprintf("Number of studies with age reported: %s", tot_study_age)

# Minimum and maximum age across studies
sam_age_min <- min(data$mean_age, na.rm = TRUE)
sprintf("Minimum age of patient reported in included studies: %s", sam_age_min)

sam_age_max <- max(data$mean_age, na.rm = TRUE)
sprintf("Maximum age of patient reported in included studies: %s", sam_age_max)

# Minimum and maximum age across studies for primary psychosis
pri_sam_age_min <- min(data$mean_age_pri, na.rm = TRUE)
sprintf("Minimum age of patient reported in included studies for primary psychosis: %s", pri_sam_age_min)

pri_sam_age_max <- max(data$mean_age_pri, na.rm = TRUE)
sprintf("Maximum age of patient reported in included studies for primary psychosis: %s", pri_sam_age_max)

# Minimum and maximum age across studies for secondary psychosis
sec_sam_age_min <- min(data$mean_age_sec, na.rm = TRUE)
sprintf("Minimum age of patient reported in included studies for secondary psychosis: %s", sec_sam_age_min)

sec_sam_age_max <- max(data$mean_age_sec, na.rm = TRUE)
sprintf("Maximum age of patient reported in included studies for secondary psychosis: %s", sec_sam_age_max)

# sex characteristics -----------------------------------------------------

# Number of studies with sex reported
tot_study_sex <- sum(complete.cases(data$gender_female_freq))
sprintf("Number of studies with sex reported: %s", tot_study_sex)

# Proportion of female participants: min
prop_fem_min <- min(data$gender_female_freq/data$tot_sample, na.rm = TRUE)
sprintf("Minimum proportion of female participants across studies: %.2f", prop_fem_min)

# Proportion of female participants: max
prop_fem_max <- max(data$gender_female_freq/data$tot_sample, na.rm = TRUE)
sprintf("Maximum proportion of female participants across studies: %.2f", prop_fem_max)

# Proportion of female participants: total
data_gender_reported <- data %>% 
  na.omit(data$gender_female_freq)
tot_prop_fem <- sum(data_gender_reported$gender_female_freq, na.rm = TRUE) / sum(data_gender_reported$tot_sample, na.rm = TRUE)
sprintf("Proportion of female participants across studies: %.2f", tot_prop_fem)

# study characteristics -----------------------------------------------------

# Number of studies using prospective design
tot_study_prospect <- count_if("yes", data$design)
sprintf("Number of prospective studies: %s", tot_study_prospect)


# Psychosis duration 


DUP_min <- min(data$DUP,na.rm=TRUE)
sprintf("min DUP across studies: %s", DUP_min)

DUP_max <- max(data$DUP,na.rm=TRUE)
sprintf("max DUP across studies: %s", DUP_max)



# secondary psychosis considered ---------------------------------------------------

# Calculate number of studies which assessed multiple causes of psychosis 
tot_multiple_sec_psych_types <- count_if("multiple", data$multiple_sec_psych_types)
sprintf("Total number of studies that considered mutiple type of psychosis: %s", tot_multiple_sec_psych_types)

# Calculate number of studies which assessed single causes of psychosis 
tot_single_sec_psych_types <- count_if("single", data$multiple_sec_psych_types)
sprintf("Total number of studies that only considered a single type of psychosis (eg drug induced): %s", tot_single_sec_psych_types)

# Calculate number of studies unclear if assessed  single of multiple causes of psychosis 
tot_single_sec_psych_types <- count_if("NS", data$multiple_sec_psych_types)
sprintf("Total number of studies where it was unclear if assessed  single of multiple causes of psychosis : %s", tot_single_sec_psych_types)

#  psychosis disorder or symptom considered ---------------------------------------------------

# Calculate number of studies which assessed disorder 
tot_psych_disorder <- count_if("disorder", data$disorder)
sprintf("Total number of studies that classified psychotic disorder: %s", tot_psych_disorder)

tot_psych_symptoms <- count_if("symptoms", data$disorder)
sprintf("Total number of studies that classified psychotic symptoms: %s", tot_psych_symptoms)

# Calculate number of studies unclear if assessed  single of multiple causes of psychosis 
tot_single_sec_psych_types <- count_if("NS", data$multiple_sec_psych_types)
sprintf("Total number of studies where it was unclear if assessed  single of multiple causes of psychosis : %s", tot_single_sec_psych_types)


# Neuroimaging/neurophysiology: ---------------------------------------------------------

# MRI
tot_mri_all <- count_if("all", data$mri_freq)
sprintf("Total number of studies that routinely performed mri on all patients: %s", tot_mri_all)

tot_mri_subset <- count_if("some", data$mri_freq)
sprintf("Total number of studies that performed mri on subset of patients: %s", tot_mri_subset)

# CT
tot_ct_all <- count_if("all", data$ct_freq)
sprintf("Total number of studies that routinely performed CT on all patients: %s", tot_ct_all)

tot_ct_subset <- count_if("some", data$ct_freq)
sprintf("Total number of studies that performed ct on subset of patients: %s", tot_ct_subset)

# Nuclear
tot_nuclear_all <- count_if("all", data$nuclear_freq)
sprintf("Total number of studies that routinely performed nuclear on all patients: %s", tot_nuclear_all)

tot_nuclear_subset <- count_if("some", data$nuclear_freq)
sprintf("Total number of studies that performed nuclear on subset of patients: %s", tot_nuclear_subset)

# EEG
tot_eeg_all <- count_if("all", data$eeg_freq)
sprintf("Total number of studies that routinely performed eeg on all patients: %s", tot_eeg_all)

tot_eeg_subset <- count_if("some", data$eeg_freq)
sprintf("Total number of studies that performed eeg on subset of patients: %s", tot_eeg_subset)

# Biosamples --------------------------------------------------------------

# CSF
tot_csf_all <- count_if("all", data$csf_freq)
sprintf("Total number of studies that routinely performed CSF on all patients: %s", tot_csf_all)

tot_csf_subset <- count_if("some", data$csf_freq)
sprintf("Total number of studies that performed CSF on subset of patients: %s", tot_csf_subset)

# Blood
tot_blood_all <- count_if("all", data$blood_freq)
sprintf("Total number of studies that routinely performed blood on all patients: %s", tot_blood_all)

tot_blood_subset <- count_if("some", data$blood_freq)
sprintf("Total number of studies that performed blood on subset of patients: %s", tot_blood_subset)

# Urine
tot_urine_all <- count_if("all", data$urine_freq)
sprintf("Total number of studies that routinely performed urine on all patients: %s", tot_urine_all)

tot_urine_subset <- count_if("some", data$urine_freq)
sprintf("Total number of studies that performed urine on subset of patients: %s", tot_urine_subset)


# Psychiatric assessment --------------------------------------------------

# psych_assess_freq
tot_psych_assess_all <- count_if("all", data$psych_assess_freq)
sprintf("Total number of studies that routinely performed psych_assess on all patients: %s", tot_psych_assess_all)

tot_psych_assess_subset <- count_if("some", data$psych_assess_freq)
sprintf("Total number of studies that performed psych_assess on subset of patients: %s", tot_psych_assess_subset)

# psych_assess_stan_tool_freq
tot_psych_assess_stan_tool_all <- count_if("yes", data$psych_assess_stan_tool_freq)
sprintf("Total number of studies that routinely performed psych_assess_stan_tool on all patients: %s", tot_psych_assess_stan_tool_all)

tot_psych_assess_stan_tool_subset <- count_if("some", data$psych_assess_stan_tool_freq)
sprintf("Total number of studies that performed psych_assess_stan_tool on subset of patients: %s", tot_psych_assess_stan_tool_subset)

# Physical examination frequency
tot_physical_exam_all <- count_if("all", data$physical_exam_freq)
sprintf("Total number of studies that routinely performed physical_exam on all patients: %s", tot_physical_exam_all)

tot_physical_exam_subset <- count_if("some", data$physical_exam_freq)
sprintf("Total number of studies that performed physical_exam on subset of patients: %s", tot_physical_exam_subset)

# Cognitive assessment frequency
tot_cog_assess_all <- count_if("all", data$cog_assess_freq)
sprintf("Total number of studies that routinely performed cog_assess on all patients: %s", tot_cog_assess_all)

tot_cog_assess_subset <- count_if("some", data$cog_assess_freq)
sprintf("Total number of studies that performed cog_assess on subset of patients: %s", tot_cog_assess_subset)

# **relabel** Cognitive assessment cog_assess_stan_tool_freq
tot_cog_assess_stan_tool_all <- count_if("yes", data$cog_assess_stan_tool_freq)
sprintf("Total number of studies that routinely performed cog_assess_stan_tool on all patients: %s", tot_cog_assess_stan_tool_all)

tot_cog_assess_stan_tool_subset <- count_if("some", data$cog_assess_stan_tool_freq)
sprintf("Total number of studies that performed cog_assess_stan_tool on subset of patients: %s", tot_cog_assess_stan_tool_subset)


# Other assessment frequency
tot_other_assess_all <- count_if("all", data$other_assess_freq)
sprintf("Total number of studies that routinely performed other_assess on all patients: %s", tot_other_assess_all)

tot_other_assess_subset <- count_if("some", data$other_assess_freq)
sprintf("Total number of studies that performed other_assess on subset of patients: %s", tot_other_assess_subset)


# Quality assessment and risk of bias -------------------------------------

qual_min <- min(data$tot_rob_score)
sprintf("Lowest quality score scross studies: %s", qual_min)
qual_max <- max(data$tot_rob_score)
sprintf("Highest quality score scross studies: %s", qual_max)

bias_low <- count_if(7 %thru% 10, data$tot_rob_score)
sprintf("Number of studies scoring in the low range for bias: %s", bias_low)
bias_med <- count_if(4 %thru% 6, data$tot_rob_score)
sprintf("Number of studies scoring in the medium range for bias: %s", bias_med)
bias_high <- count_if(0 %thru% 3, data$tot_rob_score)
sprintf("Number of studies scoring in the high range for bias: %s", bias_high)

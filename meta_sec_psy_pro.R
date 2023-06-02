########################################################################################
#                                                                                      #
# meta_mri_processing.R from Blackman et al                                            #
#                                                                                      #
# "How common are secondary causes of psychosis?                                       #
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
########################################################################################

#
# This script ensures key variables are set to the correct data types and subsets the main
# dataframes into dataframes that contain specific study types for later analysis
#

####### preparing data for analyses ######


# # Set  multiple variables to character type
subtype_char_vars <- c('author_year','setting', 
                       'continent', 'design', 'FEP','multiple_sec_psych_types', 
                       'mri_freq', 'ct_freq', 'nuclear_freq', 'eeg_freq', 
                       'csf_freq', 'blood_freq', 'urine_freq', 'psych_assess_freq', 'psych_assess_stan_tool_freq', 'physical_exam_freq', 
                       'cog_assess_freq', 'cog_assess_stan_tool_freq', 
                       'other_assess_freq', 'not_spec_assess_freq',
                       'disorder','diagnostic_tool',	'class_system')

data <- data %>%
  mutate_at(subtype_char_vars, as.character)

# # Set all multiple variables to numeric type
subtype_num_vars <- c('year', 'gender_male_freq','gender_female_freq', 'mean_age', 'ethnicity_freq', 
                      'tot_sample', 'tot_sec_psych',  
                      'auto_freq', 'congen_freq', 'toxic_freq', 'iatrogenic_freq', 'cerebrovas_freq', 'space_oc_freq', 'metabolic_freq', 'dietary_freq', 'infec_freq', 'degen_freq', 'demye_freq', 'seizure_freq', 'endocrine_freq', 'other_sub_freq', 'unknown_sub_freq', 
                      'tot_rob_score')

data <- data %>%
   mutate_at(subtype_num_vars, as.numeric)

# First episode psychosis patients
data <- data %>%
  mutate("tot_pri_psych" = tot_sample - tot_sec_psych,na.rm = TRUE)


# subsets of studies ------------------------------------------------------

# Create dataframe that includes only studies reporting FEP...
data_fep <- data %>%
  subset(FEP == "yes")

# Create dataframe based on clinical setting...

# Inpatient
data_inpat_psych <- data %>%
  subset(setting == "inpat_psych")

# Outpatient
data_outpat_psych <- data %>% 
  subset(setting == "outpat_psych")

# Accident and Emergency
data_A_and_E <- data %>% 
  subset(setting == "A_and_E")


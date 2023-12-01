########################################################################################
#                                                                                      #
# meta_mri_visualization_to_file.R from Blackman et al                                 #
#                                                                                      #
# "How common are secondary causes of psychosis?                                       # 
#  A meta-analysis of prevalence"                                                      #
#                                                                                      #
#                                                                                      #
#   primary visualizations                                                             #
#                                                                                      #
#                                                                                      #
########################################################################################

#
# This script save the primary visualizations from the meta-analysis presented in the paper and
# related analyses to the output directory specified below
#


# Global options ----------------------------------------------------------

options(na.action = "na.omit")
# options(na.action = "na.pass")


############# . Forest plots of proportion of secondary psychosis ###############

# all eligible studies included

# order by precision
precision_sec <- sqrt(ies_da_sec_psych$vi)

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych.png', sep=''), width = 10, height = 10, units = 'in', res = 300)
}

forest_sec_psych <- forest(pes_sec_psych_summary,
                           xlim = c(0,100),
                           pscale = 100,
                           rightcols = FALSE,
                           leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                           fontsize = 10,
                           xlab = "Any secondary cause(%)", smlab="",
                           fixed = FALSE,
                           weight.study = "fixed", # same size if random used
                           col.square = "black", col.square.lines = "black",
                           col.diamond = "black", col.diamond.lines = "black",
                           plotwidth = "10cm", colgap = "5mm",fig.height = 0.1, fig.width = 0.2, fig.align = "center",
                           pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                           print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                           digits = 1, sortvar = precision_sec)
forest_sec_psych

if (write_to_file == 1) {
  dev.off() 
}

# by subgroup: secondary causes considered [not included in paper] --------------------------------------------------------

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych_by_cause.png', sep = ''), width = 7, height = 14, units = 'in', res = 300)
}

forest_sec_psych_by_cause <- forest(pes_sec_psych_summary_by_multiple,
                                    xlim = c(0,100),
                                    pscale = 100,
                                    rightcols = FALSE,
                                    leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                    fontsize = 10,
                                    xlab = "Any secondary cause(%)", smlab = "",
                                    fixed = FALSE,
                                    weight.study = "fixed", #same size if random used
                                    col.square = "black", col.square.lines = "black",
                                    col.diamond = "black", col.diamond.lines = "black",
                                    plotwidth = "5cm", colgap = "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                    pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                                    print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                                    digits = 1)

forest_sec_psych_by_cause

if (write_to_file == 1) {
  dev.off() 
}

# by subgroup: FEP [not included in paper]--------------------------------------------------------

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych_by_FEP.png', sep = ''), width = 7, height = 14, units = 'in', res = 300)
}

forest_sec_psych_by_FEP <- forest(pes_sec_psych_summary_by_FEP,
                                  xlim = c(0,100),
                                  pscale = 100,
                                  rightcols = FALSE,
                                  leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                  fontsize = 10,
                                  xlab = "Any secondary cause(%)", smlab = "",
                                  fixed = FALSE,
                                  weight.study = "fixed", #same size if random used
                                  col.square = "black", col.square.lines = "black",
                                  col.diamond = "black", col.diamond.lines = "black",
                                  plotwidth = "5cm", colgap = "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                  pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                                  print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                                  digits = 1)

forest_sec_psych_by_FEP

if (write_to_file == 1) {
  dev.off() 
}

# by subgroup: design [not included in paper]--------------------------------------------------------

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych_by_design.png', sep=''), width = 7, height = 14, units = 'in', res = 300)
}

forest_sec_psych_by_design <- forest(pes_sec_psych_summary_by_design,
                                     xlim = c(0,100),
                                     pscale = 100,
                                     rightcols = FALSE,
                                     leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                     fontsize = 10,
                                     xlab = "Any secondary cause(%)", smlab = "",
                                     fixed = FALSE,
                                     weight.study = "fixed", #same size if random used
                                     col.square = "black", col.square.lines = "black",
                                     col.diamond = "black", col.diamond.lines = "black",
                                     plotwidth = "5cm", colgap = "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                     pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                                     print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                                     digits = 1)

forest_sec_psych_by_design

if (write_to_file == 1) {
  dev.off() 
}

# by subgroup: setting [not included in paper]--------------------------------------------------------

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych_by_setting.png', sep=''), width = 7, height = 14, units = 'in', res = 300)
}

forest_sec_psych_by_setting <- forest(pes_sec_psych_summary_by_setting_bin,
                                      xlim = c(0,100),
                                      pscale = 100,
                                      rightcols = FALSE,
                                      leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                      fontsize = 10,
                                      xlab = "Any secondary cause(%)", smlab = "",
                                      fixed = FALSE,
                                      weight.study = "fixed", #same size if random used
                                      col.square = "black", col.square.lines = "black",
                                      col.diamond = "black", col.diamond.lines = "black",
                                      plotwidth = "5cm", colgap =  "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                      pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                                      print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                                      digits = 1)

forest_sec_psych_by_setting

if (write_to_file == 1) {
  dev.off() 
}

# by subgroup: symptoms or disorder [not included in paper] --------------------------------------------------------

if (write_to_file == 1) {
  png(file = paste('output/', 'forest_sec_psych_by_disorder.png', sep=''), width = 7, height = 14, units = 'in', res = 300)
}

forest_sec_psych_by_disorder <- forest(pes_sec_psych_summary_by_disorder,
                                       xlim = c(0,100),
                                       pscale = 100,
                                       rightcols = FALSE,
                                       leftcols = c("studlab", "event", "n", "effect", "ci"), leftlabs = c("Study", "Cases", "Total", "Prev(%)", "95% C.I."), 
                                       fontsize = 10,
                                       xlab = "Any secondary cause(%)", smlab = "",
                                       fixed = FALSE,
                                       weight.study = "fixed", #same size if random used
                                       col.square = "black", col.square.lines = "black",
                                       col.diamond = "black", col.diamond.lines = "black",
                                       plotwidth = "5cm", colgap =  "5mm",fig.height = 0.1, fig.width = 0.1, fig.align = "center",
                                       pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
                                       print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
                                       digits = 1)
forest_sec_psych_by_disorder

if (write_to_file == 1) {
  dev.off() 
}





# ############# Supplementary Figures Section  ###############
#
# ############# Funnel plot of studies

if (write_to_file == 1) {
  png(file=paste('output/', 'funnel_all.png', sep=''), width = 7, height = 5, units = 'in', res = 300)
}

funnel(pes_sec_psych_summary,
       xlim = c(0.2,1),
       steps = "10",
       pscale = 100,
       fixed = FALSE,
       rightcols = FALSE,
       leftcols = c("studlab", "event", "n", "effect", "ci"), ylab = "Standard Error", xlab = "Any secondary cause(%)", smlab = "",
       col = "black",
       pooled.totals = FALSE, comb.fixed = FALSE, fs.hetstat = 10,
       print.tau2 = TRUE, print.Q = TRUE, print.pval.Q = TRUE, print.I2 = TRUE,
       digits = 2, sortvar = precision_sec)

if (write_to_file == 1) {
  dev.off()
}



############## Forest plot of leave one out sensitivity analysis ###############
 
if (write_to_file == 1) {
  png(file = paste('output/', 'forest_L1O_sen_anal.png', sep=''), width = 8, height = 8, units = 'in', res = 300)
}

forest(leave1out_sec_psych,
       fontsize = 10, spacing = .8)

if (write_to_file == 1) {
  dev.off()
}





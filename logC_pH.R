# function to plot logC diagrams
# pka_list: list of pka values, ideally in order of most acidic to 
#           least acidic, but sorted later by alpha_pH; default values 
#           are for citric acid
# pH_range: min to max; defaults to 0 to 14
# pH_step: increments along pH axis for calculating alpha values; 
#          defaults to 0.01
# plot_logc: logical value; if TRUE (the default), plots logC diagram
# conc_total: analytical concentration of weak acid; defaults to 0.1 M
# species: option to enter name of weak acid for main title of plot;
#          defaults to NULL, which supresses main title
# labels: option to enter vector of labels for legend; defaults to
#         NULL, which uses a default legend

logC_pH = function(pka_list = c(3.128, 4.761, 6.396), 
                   pH_range = c(0,14), pH_step = 0.01, conc_total = 0.1,
                   plot_logc = TRUE, species = NULL, labels = NULL) {
  
  # calculate pC values
  # call function alpha_pH to calculate alpha values; convert alpha 
  # values into pC values for each species; and add column of pOH values
  # NOTE: for now, code sources alpha_pH, which requires setting working
  # directory to the folder where the function is stored
  
  source(file = "alpha_pH.R", local = TRUE)
  logc = alpha_pH(pka_list, pH_range, pH_step, plot_alpha = FALSE)
  n = length(pka_list)
  logc[ , 2:(n + 2)] = -log10(logc[ , 2:(n + 2)] * conc_total)
  logc$pOH = 14 - logc$pH
  
  # create logC plot
  # if requested, returns a plot; adds a main title if name of species 
  # is provided; adds a legend using default values or provided labels; 
  # and add labels for H+ and for OH-
  
  if (plot_logc == TRUE) {
    matplot(logc$pH, as.matrix(logc[ , 2:(n + 2)]), 
            type = "l", lty = c(1:6), lwd = 2, 
            col = c(rep("blue", 6), rep("red", 6)),
            xlim = c(pH_range[1], pH_range[2]), ylim = c(14,-0.4),
            xlab = "pH", ylab = "pC")
    lines(logc$pH, logc$pH, lwd = 2, lty = 2, col = "green4")
    lines(logc$pH, logc$pOH, lwd = 2, lty = 2, col = "firebrick4")
    text(0, 13.5, pos = 3, offset = 0.1, expression("OH"^"-"), col = "firebrick4")
    text(14, 13.5, pos = 3, offset = 0.1, expression("H"^"+"), col = "green4")
    abline(h = -log10(conc_total), col = "black", lwd = 1)
  }
  if (is.null(species) == FALSE) {
    title(main = paste("logC plot for ", species, sep = ""))
  }
  if (is.null(labels) == TRUE) {
    labels = rep("", n + 1)
    for (i in 1:(n + 1)) {
      num.protons = n - i + 1
      labels[i] = eval(substitute(expression(alpha[I]), 
                                  list(I = num.protons)))
    }
    legend(x = "top", 
           legend = labels, lty = c(1:6), 
           col = c(rep("blue", 6), rep("red", 6)), lwd = 2,
           bty = "n", horiz = TRUE)
  } else {
    legend(x = "top", 
           legend = labels, lty = c(1:6), 
           col = c(rep("blue", 6), rep("red", 6)), lwd = 2,
           bty = "n", horiz = TRUE)
  }

# return data: data frame containing pH, pOH, and alpha values  
  
  invisible(logc)
  
}

# sample code to show output

# source('~/Box Sync/Sabbatical 2015-16/R-Project/EquilibriumDiagrams/alpha_pH.R')
# x = logC_pH(species = "citric acid")
# head(x)

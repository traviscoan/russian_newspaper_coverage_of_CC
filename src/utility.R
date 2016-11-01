#----------------------------------------------------------------
# A set of utility functions to execute the replication.R script.
#----------------------------------------------------------------

# Estimate "hurdle" portion via logistic regression
est.logit.model <- function(dv, df){
  b <- "kremlin + putin + center + right + energy + business + government + politics + inflation + cc_disasters + (1|groups) + (1|year)"
  model_formula <- as.formula(paste(dv, '~', b))
  fit <- brm(formula = model_formula,
             data = df, family = bernoulli(link = "logit"),
             prior = set_prior("cauchy(0, 2.5)", class = "b"),
             warmup = 2000, iter = 5000, chains = 1,
             control = list(adapt_delta = 0.95))
  
  # Extract estimates
  post <- posterior_samples(fit)
  post_medians <- as.data.frame(apply(post, 2, median))
  post_coda <- as.mcmc(fit)
  hpd <- HPDinterval(post_coda, prob = .9)
  hpd_df <- as.data.frame(hpd)
  estimates <- as.data.frame(cbind(post_medians, hpd_df))
  names(estimates) <- c("est", "lower", "upper")
  return(list("estimates" = estimates[1:11,], "posterior" = post[,1:11]))
}

# Estimate hierarchical beta model using Stan via brms
est.beta.model <- function(dv, df) {
  b <- paste("0 + main + spec + main:(kremlin + putin + center + right + energy + business + government + politics + inflation + cc_disasters + t + t_2)", 
             "+ spec:(kremlin + putin + center + right + energy + business + government + politics + inflation + cc_disasters + t + t_2) + (1|groups) + (1|yq)",
             sep = "")
  model_formula <- as.formula(paste(dv, '~', b))
  fit <- brm(formula = model_formula,
             data = df, family = zero_inflated_beta(link = "logit"),
             prior = set_prior("normal(0,5)", class = "b"),
             warmup = 2000, iter = 5000, chains = 1,
             control = list(adapt_delta = 0.95))
  
  # Print results to track progress
  print(fit)
  
  # Extract estimates
  post <- posterior_samples(fit)
  post_medians <- as.data.frame(apply(post, 2, median))
  post_coda <- as.mcmc(fit)
  hpd <- HPDinterval(post_coda, prob = .9)
  hpd_df <- as.data.frame(hpd)
  estimates <- as.data.frame(cbind(post_medians, hpd_df))
  names(estimates) <- c("est", "lower", "upper")
  
  return(list("estimates" = estimates, "posterior" = post))
}

# Parsse estimates to combine for latter plots
parse.beta.estimates <- function(results){
  estimation_results <-data.frame()
  for (i in seq(length(dvs))){
    ests <- results[[i]][[1]]
    main_indices <- grep("*main*", row.names(ests))
    ests <- ests[main_indices,][1:11,]
    print(ests)
    ests$dv_label <- dvs[i]
    estimation_results <- as.data.frame(rbind(estimation_results, ests))
  }
  return(estimation_results)
}

# Prepare data for plots

prep.plot.data <- function(estimation_results){
  # Define and map new labels
  label_dvs <- c("irenv", "science_impacts", "energy_combined", "ir", "hurdle")
  label_names <- c("Int'l Agreements", "Climate Science and Impacts", "Energy Issues", "Geopolitics", "Coverage Decision")
  estimation_results$label_new <- mapvalues(estimation_results$dv_label, from = label_dvs, to = label_names)
  # Change order of panel label
  estimation_results$label_new <- factor(estimation_results$label_new, 
                                         levels=c("Coverage Decision", "Climate Science and Impacts", 
                                                  "Geopolitics", "Energy Issues", "Int'l Agreements"))
  # Add variable levels
  var_names <- c("Intercept", "Kremlin", "Putin", "Center", "Right", "Energy",
                 "Business (owner)", "State (owner)", "Opp. Party (owner)",
                 "Inflation", "Disasters")
  var_labels <- rep(var_names, 5)
  estimation_results <- as.data.frame(cbind(estimation_results, var_labels))
  # Order variables
  estimation_results$vname <- factor(estimation_results$var_labels, 
                                     levels(estimation_results$var_labels)[c(6,9,7,10,2,11,8,1,4,5,3,12,13)])
  estimation_results$vname_reverse <- factor(estimation_results$vname, 
                                             levels=rev(levels(estimation_results$vname)))
  
  return(estimation_results)
}










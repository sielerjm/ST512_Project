# Functions


# Calculate Alpha Scores --------------------------------------------------
#   Description: Generates alpha diversity scores from a list of alpha methods
#   Input: phyloseq object, list of alpha div. methods, 
#   Output: dataframe of alpha-diversity scores

alpha_base <- function(
  physeq,  # Phyloseq object
  methods,  # List of alpha methods (e.g., c(Shannon, Simpson, Observed) )
  smpl.col.name = "Sample",  # Default is "Sample" but you can change it to whatever when you call the function 
  phylo.div = F  # Only set to true if you have phylogenetic information attached to your phyloseq object
){
  
  end.method <- ifelse(isTRUE(phylo.div),(length(methods)-1), length(methods)) 
  
  # Calculates alpha scores
  print("Calculating non-phylogenetic alpha scores...")  # TEST
  
  tmp.dt <- phyloseq::estimate_richness(
    physeq = physeq,  # Physeq object
    measures = methods[1:end.method]
  ) %>% as.data.table(keep.rownames = smpl.col.name) %>% setkeyv(smpl.col.name)  # Sets sample column name to whatever you've called it (e.g., "Sample" or "Sample.ID")
  tmp.dt[, se.chao1 := NULL] # No idea what this does, but it's from Keatons code and I think it's important if you calculate se.chao1 scores
  
  # If you have phylogenetic information in your phyloseq object you'll want to set phylo.div to true
  if(isTRUE(phylo.div)){
    
    print("Calculating phylogenetic alpha scores...")  # TEST
    
    # Calculate the sum of the total phylogenetic branch length for one or multiple samples. See ?picante::pd() for more info
    #   - Returns a dataframe of the PD and species richness (SR) values for all samples
    phy.dt <- picante::pd(samp = otu.matrix(physeq), tree = phyloseq::phy_tree(physeq), include.root = F) %>%
      as.data.table(keep.rownames = smpl.col.name) %>% setkeyv(smpl.col.name)  # set col name
    
    # Names the columns by alpha method
    # names(phy.dt)[(length(phy.dt)-1):length(phy.dt)] <- methods[length(methods)]  #methods[(length(methods)-1):length(methods)]
    names(phy.dt)[2:3] <- methods[(length(methods)-1):length(methods)]
    tmp.dt[phy.dt] # adds the columns to the other dataframe with the previously calculated alpha scores
    
    # return to sender
    return (tmp.dt[phy.dt])
  }
  
  # Returns alpha scores datatable
  return (tmp.dt)
}


# Melt Data Table ---------------------------------------------------------
#   Description: Melts sample data table for easy plotting
#   Input: normalized alpha scores, alpha methods, variable names
#   Output: a melted datatable


melt_to_datatable_2 <- function(datatable1, datatable2, vars, var.name, samp.name = "Sample", val.name = "Score"){
  
  comb.data <- datatable1[datatable2, on = (samp.name)] %>% setkeyv(samp.name)
  
  return( melt(
    comb.data,  # combined datatables from above
    measure.vars = vars, # Measure variables for melting. Can be missing, vector, list, or pattern-based.
    variable.name = var.name, # Name for the measured variable names column.
    value.name = val.name  # Name for the molten data values column(s).
  )
  )
}





# Linear regression plots --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 

check_assump_plots <- function(data, 
                               alpha,
                               num.row = 2,
                               num.col = 2
                               ){
  
  tmp.plot.list <- list()
    
    # Residuals vs Fitted
    tmp.plot.list$resVsFit <- res_vs_fitted(data[[alpha]], alpha)
    
    # Q-Q Plot
    tmp.plot.list$qqPlot <- qq_plot(data[[alpha]], alpha)
    
    # Scale Location
    tmp.plot.list$scaleLocPlot <- scale_loc_plot(data[[alpha]], alpha)
    
    # Residuals vs Leverage
    tmp.plot.list$resVsLev <- res_vs_lev_plot(data[[alpha]])

  # return(tmp.plot.list)
    
  return( marrangeGrob(tmp.plot.list, nrow = 2, ncol = 2, top=paste0("Check Assumptions (LM): ", alpha)) )
}

res_vs_fitted <- function(data, alpha){
  
  tmp.plot <- {
    ggplot(data, aes(x=fitted(data), y=residuals(data))) + 
      geom_point(size = .75, alpha = .5) + 
      geom_smooth(method = "lm", color = "red") + 
      labs(title = "Residuals vs Fitted",
           # caption = alpha,
           x = "Fitted values",
           y = "Residuals"
      )
  }
  
  return (tmp.plot)
}

qq_plot <- function(data, alpha){
  
  tmp.plot <- {
    ggplot(data, aes(sample = stdres(data))) +
      stat_qq(size = .75, alpha = .5) +
      stat_qq_line(color = "red") +
      labs(title = "Normal Q-Q",
           # caption = alpha,
           x = "Theoretical Quantiles",
           y = "Standardized Residuals"
      )
  }
  
  return (tmp.plot)
}

scale_loc_plot <- function(data, alpha){
  
  tmp.plot <- {
    ggplot(data, aes(x=fitted(data), y=sqrt(stdres(data))) ) + 
      geom_point(size = .75, alpha = .5) + 
      geom_smooth(method = "lm", color = "red") + 
      labs(title = "Scale Location",
           # caption = alpha,
           x = "Fitted values",
           y = "sqrt( Standardized Residuals )"
      )
  }
  
  return (tmp.plot)
}

res_vs_lev_plot <- function(data){
  
  tmp.plot <- {
    ggplot(data, aes(x=cooks.distance(data), y=stdres(data)) ) +
      geom_point(size = .75, alpha = .5) +
      geom_smooth(method = "lm", color = "red") +
      labs(title = "Residuals vs Leverage",
           # caption = alpha,
           x = "Leverage",
           y = "Standardized Residuals"
      )
  }
  
  return ( tmp.plot )
}



# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 

lev_cutoff <- function(data, alpha) {
  cutOff.lev <- (2 * length(caseInfStats$mod.unref[[alpha]][["coefficients"]]) / 
                   nrow(caseInfStats$dataFort.unref[[alpha]]))
}



# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 





# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 






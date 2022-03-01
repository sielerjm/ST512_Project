# Functions


# Calculate Alpha Scores --------------------------------------------------
#   Description: Generates alpha diversity scores from a list of alpha methods
#   Input: phyloseq object, list of alpha div. methods, 
#   Output: dataframe of alpha-diversity scores

alpha_base <- function(phyloseq.obj, alpha.methods, smpl.col.name, phylo.div = F){  
  tax.dt <- estimate_richness(
    physeq = phyloseq.obj,
    measures = alpha.methods[1:3]
  ) %>% as.data.table(keep.rownames = smpl.col.name) %>% setkeyv(smpl.col.name)
  tax.dt[, se.chao1 := NULL]
  
  if(isTRUE(phylo.div)){
    ## Need phylogeny data for this to work
    phy.dt <- pd(samp = otu.matrix(phyloseq.obj), tree = phy_tree(phyloseq.obj)) %>%
      as.data.table(keep.rownames = smpl.col.name) %>%
      setkeyv(smpl.col.name)
    
    names(phy.dt)[2:3] <- alpha.methods[4:5]
    tax.dt[phy.dt]
    
    return (tax.dt[phy.dt])
  }
  
  return (tax.dt)
}



# Normalize Alpha Scores --------------------------------------------------
#   Description: normalizes alpha diversity scores based on their distributions
#   Input: dataframe of alpha diversity scores, metadata table
#   Output: normalized datatable of alpha scores (0 to 1)

norm_alpha_score <- function(alpha.base, sample.df, methods){
  model.data.base <- copy(alpha.base[alpha.base$Sample %in% 
                                       row.names(sample.df)])
  
  for (alpha in methods) {
    
    # progress_Bar_Start(which(methods == alpha), length(methods))  # Progress bar
    
    if (ad.test(model.data.base[[alpha]])$p.value <= 0.05) {
      trans <- transformTukey(model.data.base[[alpha]], plotit = F, quiet = F, statistic = 2)
      trans <- (trans-min(trans))/(max(trans)-min(trans))   # Fixes normalization 0 to 1
      
      if (ad.test(trans)$p.value > 0.05) {
        model.data.base[[alpha]] <- trans
        # print(paste0("Transforming: ", alpha))  # TEST
        print(paste0("Finished: ", alpha))
      } else {
        model.data.base[[alpha]] <- (model.data.base[[alpha]] - min(model.data.base[[alpha]] ))/(max(model.data.base[[alpha]] )-min(model.data.base[[alpha]] ))  # Fixes normalization 0 to 1
        # print(paste0("dividing by max: ", alpha ))  # TEST
        print(paste0("Finished: ", alpha))
      } 
    } else {
      model.data.base[[alpha]] <- (model.data.base[[alpha]] - min(model.data.base[[alpha]] ))/(max(model.data.base[[alpha]] )-min(model.data.base[[alpha]] ))  # Fixes normalization 0 to 1
      print(paste0("Finished: ", alpha))
    }
  }
  
  return(model.data.base)
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



# Generic Box Plots -----------------------------------------
#   Description: Generates box plot 
#   Input: data, significant explanatory variables, resp variable, facet conditions
#   Output: box plot

gen_box_plot_2 <- function(data, x.var, y.var, facet.var.x = ".", facet.var.y = ".", 
                           y.lab = y.var, colors = pal.dark2, title = "", caption = "",
                           comparisons = ""){
  
  p <- ggplot(data,
              aes_string(x = x.var, y = y.var)) +
    geom_boxplot(aes(fill = eval(parse(text = x.var)) ) # converts string to var name
    ) +
    geom_quasirandom() + 
    facet_grid(as.formula(paste0(facet.var.y, " ~ ", facet.var.x)), scales = "free_y") + 
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    guides(fill=guide_legend(title=x.var)) +
    # theme(legend.position = "bottom") + 
    labs(
      title = ifelse(title != "", title, ""),
      caption = ifelse(caption != "", caption, ""),
      y = y.lab
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty(n = 8), limits = c(0, 1.1)) 
  
  return(p)
}

# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------


# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------


# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------


# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------


# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------


# Name of function --------------------------------------------------------
#   Description: 
#   Input: 
#   Output: 


# -------------------------------------------------------------------------



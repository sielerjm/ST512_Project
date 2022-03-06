# Libraries

## General Libraries
library(knitr)  # For knitting documents to HTML or PDF formats
library(ggplot2)  # For plotting pretty graphs
library(tinytex)  # knitting to pdf, latex stuff
library(tidyverse)  # Making your code look pretty and tidy
library(RColorBrewer)  # Making and using pretty color pallettes
library(data.table)  # Handle data tables
library(reshape2)  # for reshaping data using melt()
library(cowplot)  # Plotting customization
library(flextable)  # Making pretty tables
library(nortest)  # Allows us to run ad.test()

# ## Microbiome Analysis
library(phyloseq)  # microbiome calculations
library(vegan)  # distance functions
library(phyloseqCompanion)  # helper functions for manipulating phyloseq objects
library(car)  # Allows us to run Anova()
library(pander)  # visualizing tables
library(ggbeeswarm)  # pretty visuals for boxplots
library(gridExtra)  # use marrangeGrob, for combineing plots
library(report)  # run statistical reports
library(MASS) # run stdres()

## Installing packages

# devtools::install_github("kstagaman/phyloseqCompanion") 

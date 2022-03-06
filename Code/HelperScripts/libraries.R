# Libraries

## General Libraries
library(knitr)  # For knitting documents to HTML or PDF formats
library(ggplot2)  # For plotting pretty graphs
library(tinytex)
library(tidyverse)  # Making your code look pretty and tidy
library(RColorBrewer)  # Making and using pretty color pallettes
library(data.table)  # Handle data tables
library(reshape2)  # for reshaping data using melt()
library(cowplot)  # Plotting customization
library(flextable)  # Making pretty tables
# library(nortest)  # Allows us to run ad.test()

# ## Microbiome Analysis
library(phyloseq)  #
library(vegan)  #
library(phyloseqCompanion)  # helper functions for manipulating phyloseq objects
# library(rcompanion)  # Allows us to run transformTukey()
# library(Maaslin2)  # Differential abundance analysis


# ## Other, unorganized
# library(ggrepel)  # Adds label buffering to labels so they don't overlap, geom_label_repel()
# library(tibble)  # Allows us to make tibbles
# library(tinytex)  # LaTeX 
# library(parallel)
# library(beepr)  # Allows us to use beep(), notification when a function is done
# library(lubridate)
# library(stringr)  # Wrap text in plots # str_wrap(text, 80)
# library(ggExtra)  # Additional ggplot settings
# library(nortest)  # Allows us to run ad.test()
# library(recipes)  # Allows us to use tidy()
# library(caret)
library(car)  # Allows us to run Anova()
# library(lme4)
library(pander)  # visualizing tables
# # library(ggpattern)  # add patterns to plots

# ## Unused, but potentially useful:

# library(forcats)
# # library(viridis)
# # library(ggpubr)
# 
# # ## Libraries that Keaton Stagaman uses
# # library(RedoControl)  # not available for 4.02  # remotes::install_github("kstagaman/redoControl")
# # library(autoNumCaptions)  # not available for 4.02  # devtools::install_github("kstagaman/autoNumCaptions")
# # library(picante)
# # library(ape)
# # library(rcompanion)
# library(MASS)  # polr() - ordinal regression,
# library(kableExtra)
# # library(gt)
# # library(pROC)
library(ggbeeswarm)
library(gridExtra)  # use marrangeGrob, for combineing plots
# library(ggstatsplot)
# # library(WRS2)  # for data, ggstatsplot
# # library(afex)  # to run anova, ggstatsplot
# library(easystats)
library(report)
# library(parameters)
# library(ggpubr)
# library(CoDaSeq)  # For CLR transformations, PCOA plots
# library(zCompositions)  # For CLR transformations
# library(nlme)  # Ancom dependency
# library(compositions)  # Ancom dependency

## Installing packages

# devtools::install_github("kstagaman/phyloseqCompanion@e761fe4")  # e761fe4

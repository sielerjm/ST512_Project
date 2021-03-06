
# Analysis Helper ---------------------------------------------------------
# Description: Instantiates important variables and settings for analyses


# Naming Conventions ------------------------------------------------------
# Description: format for naming variables/functions consistently
#   - If multiple ords per `<>`, Capitalize subsequent words (eg. <moreThanOneWord>)

# VARIABLES: 
#   <highLevelVarType>.<subtype>.<subset>.<variables>.<Modifications>
#
#   dt.<subtype>.<subset>.<modifications>  # datatables
#   df.<subtype>.<subset>.<modifications> # dataframes
#   plot.<subset>.<y-var>.<x-vars...>  # plots/figures
#   table.<subset>.<y-var>.<x-vars...>  # tables
#   mod.<subtype>.<subset>.<y>.<x-vars...>  # models (lm, glm, etc.)
#   ps.<subtype>.<subset>  # phyloseq objects

# FUNCTIONS:
#   Should be descriptive, but short enough to know the main task

# Set Environmental Variables ---------------------------------------------

# Analysis ID
analysis.ID <- paste0(
  "ST512_Proj_",  # Data subsetted? If so, how? "<name>_"
  Sys.Date()  # Date of analysis
)

# Number of cores
# - Automated
#   - detectCores()  # Tells you how many cores your comp has available
#   - If cores greater than 4, use ~90%. If 4 or less, use ~50%
num.cores = ifelse(detectCores() > 4, round(detectCores()*.9), round(detectCores()*.5) )

# - Manual
# num.cores = ifelse(detectCores() > 4, round(detectCores()*.9), round(detectCores()*.5) )
# num.cores = 4


# Import Data -------------------------------------------------------------

# Load Phyloseq Object
# ps.all <- readRDS(paste0(objects.path, "/phyloseq_sampData_cleaned_noOutliers_2022-02-28.rds"))

# sample_data(ps.all) <- sample.data.frame(ps.all) %>%
#   rename(Body.Condition.Score = Body.Condition.Trad)


# Load Sample Data
df.all <- sample.data.frame(ps.all)
dt.all <- sample.data.table(ps.all) 



# Reorder Factor Levels ---------------------------------------------------

## Exposure --------------------------------------------------------------

# levels(df.all$Exposure)  # TEST
# df.all$Exposure <- factor(df.all$Exposure, levels=c("Unexposed", "Exposed"))
# dt.all$Exposure <- factor(df.all$Exposure, levels=c("Unexposed", "Exposed"))




# Subset Data -------------------------------------------------------------
# - Different analyses will required looking at different subsets of the data
#   - All at T0: Technically all controls. 
#       - How do diets differ?
#   - Controls, T0-T1: 
#       - How do diets differ across time?
#   - T1: 
#       - How do diets differ across treatments?
#   - Exposed/Final: 
#       - How does Diet impact X, depending on Treatment/Infection?
#       - How does Infection.Status impact X?


## Initial ----------------------------------------------------------

df.T0 <- df.all %>%
  filter(Timepoint == "3mpf")

# Datatable
dt.T0 <- dt.all %>%
  filter(Timepoint == "3mpf")

# Phyloseq Object
ps.T0 <- ps.all %>%
  subset_samples(Timepoint == "3mpf")

## Initial and Final ----------------------------------------------------

# Dataframe
df.conT0T1 <- df.all %>%
  filter(Treatment == "Control")

# Datatable
dt.conT0T1 <- dt.all %>%
  filter(Treatment == "Control")

# Phyloseq Object
ps.conT0T1 <- ps.all %>%
  subset_samples(Treatment == "Control")

## Final ----------------------------------------------------------------
# Dataframe
df.T1 <- df.all %>%
  filter(Timepoint == "6mpf")

# Datatable
dt.T1 <- dt.all %>%
  filter(Timepoint == "6mpf")

# Phyloseq Object
ps.T1 <- ps.all %>%
  subset_samples(Timepoint == "6mpf")

## Final and Exposed ------------------------------------------------
# - See what the effect of exposure had on final time point 

# Dataframe
df.expFin <- df.all %>%
  filter(Timepoint == "6mpf" & Exposure == "Exposed")

# Datatable
dt.expFin <- dt.all %>%
  filter(Timepoint == "6mpf" & Exposure == "Exposed")

# Phyloseq Object
ps.expFin <- ps.all %>%
  subset_samples(Timepoint == "6mpf" & Exposure == "Exposed")


# Alpha-Diversity ---------------------------------------------------------

methods.alpha <- c("Observed", "Shannon", "Simpson") %>% 
  purrr::set_names()


## Calculate Alpha Scores -------------------------------------------------

# Creates a datatable of alpha diversity scores for each sample

dt.alphaScores.all <- alpha_base(ps.all,  # Phyloseq object
                         methods.alpha,  # list of alpha methods
                         "Sample")  # Column name for sample IDs

dt.alphaScores.T0 <- alpha_base(ps.T0,  # Phyloseq object
                             methods.alpha,  # list of alpha methods
                             "Sample")  # Column name for sample IDs

dt.alphaScores.conT0T1 <- alpha_base(ps.conT0T1,  # Phyloseq object
                                methods.alpha,  # list of alpha methods
                                "Sample")  # Column name for sample IDs

dt.alphaScores.T1 <- alpha_base(ps.T1,  # Phyloseq object
                                methods.alpha,  # list of alpha methods
                                "Sample")  # Column name for sample IDs

dt.alphaScores.expFin <- alpha_base(ps.expFin,  # Phyloseq object
                                 methods.alpha,  # list of alpha methods
                                 "Sample")  # Column name for sample IDs


## Normalize Alpha Scores -------------------------------------------------

dt.alphaScores.norm.all <- norm_alpha_score(dt.alphaScores.all, df.all, methods.alpha)

dt.alphaScores.norm.T0 <- norm_alpha_score(dt.alphaScores.T0, df.T0, methods.alpha)
dt.alphaScores.norm.conT0T1 <- norm_alpha_score(dt.alphaScores.conT0T1, df.conT0T1, methods.alpha)
dt.alphaScores.norm.T1 <- norm_alpha_score(dt.alphaScores.T1, df.T1, methods.alpha)

dt.alphaScores.norm.expFin <- norm_alpha_score(dt.alphaScores.expFin, df.expFin, methods.alpha)


## Alpha Datatable -------------------------------------------------------

# make a dataframe containing sample data and alpha diversity
dt.alphaPlus.all <- dt.all[dt.alphaScores.norm.all, on = "Sample"] %>% setkeyv("Sample")

dt.alphaPlus.T0 <- dt.T0[dt.alphaScores.norm.T0, on = "Sample"] %>% setkeyv("Sample")
dt.alphaPlus.conT0T1 <- dt.conT0T1[dt.alphaScores.norm.conT0T1, on = "Sample"] %>% setkeyv("Sample")
dt.alphaPlus.T1 <- dt.T1[dt.alphaScores.norm.T1, on = "Sample"] %>% setkeyv("Sample")

dt.alphaPlus.expFin <- dt.expFin[dt.alphaScores.norm.expFin, on = "Sample"] %>% setkeyv("Sample")


# All Samples
# function(datatable1, datatable2, vars, var.name, samp.name = "Sample", col.name = "Score")
dt.alphaPlus.all.melt <- melt_to_datatable_2(datatable1 = dt.all, 
                                         datatable2 = dt.alphaScores.norm.all, 
                                         vars = methods.alpha, 
                                         var.name = "Alpha.Metric",
                                         samp.name = "Sample",
                                         val.name = "Alpha.Score")

# T0
dt.alphaPlus.T0.melt <- melt_to_datatable_2(datatable1 = dt.T0, 
                                         datatable2 = dt.alphaScores.norm.T0, 
                                         vars = methods.alpha, 
                                         var.name = "Alpha.Metric",
                                         samp.name = "Sample",
                                         val.name = "Alpha.Score")

# ControlsT0T1
dt.alphaPlus.conT0T1.melt <- melt_to_datatable_2(datatable1 = dt.conT0T1, 
                                            datatable2 = dt.alphaScores.norm.conT0T1, 
                                            vars = methods.alpha, 
                                            var.name = "Alpha.Metric",
                                            samp.name = "Sample",
                                            val.name = "Alpha.Score")

# T1
dt.alphaPlus.T1.melt <- melt_to_datatable_2(datatable1 = dt.T1, 
                                            datatable2 = dt.alphaScores.norm.T1, 
                                            vars = methods.alpha, 
                                            var.name = "Alpha.Metric",
                                            samp.name = "Sample",
                                            val.name = "Alpha.Score")

# Exposed Final
# Controls
dt.alphaPlus.expFin.melt <- melt_to_datatable_2(datatable1 = dt.expFin, 
                                             datatable2 = dt.alphaScores.norm.expFin, 
                                             vars = methods.alpha, 
                                             var.name = "Alpha.Metric",
                                             samp.name = "Sample",
                                             val.name = "Alpha.Score")


# Beta-Diversity ----------------------------------------------------------

# Distance lists
distList.all <- gen.dist.matrices(ps.T0, methods = "taxonomic", cores = num.cores)
methods.beta <- names(distList.all) %>% set_names(., .)

# T0
distList.T0 <- gen.dist.matrices(ps.T0, methods = "taxonomic", cores = num.cores)

# ControlsT0T1
distList.conT0T1 <- gen.dist.matrices(ps.conT0T1, methods = "taxonomic", cores = num.cores)

# T1
distList.T1 <- gen.dist.matrices(ps.T1, methods = "taxonomic", cores = num.cores)

# Exposed Final
distList.expFin <- gen.dist.matrices(ps.expFin, methods = "taxonomic", cores = num.cores)



# Differential Abundance ------------------------------------------------


## Load Data ---------------------------------------------------------------



if(!isTRUE(rerun.DiffAbund)){
  
  # Sys.Date()
  tmp.DiffAbund.date <- "2022-02-28"  # Needs to be date of last DiffAbund analysis run (eg. "2022-02-28")
  
  # Load Diff Abund objects
  load(paste0(objects.path,"/DiffAbund-OBJs_ZF-DietStudy_", tmp.DiffAbund.date, ".Rdata"))
  
} else {


## Diet --------------------------------------------------------------------



### Diet (both times)--------------------------------------------------------------------


DiffAbund_Diet_rEff <- diffAbund_Maaslin2(physeq = ps.conT0T1,
                                          physeq.ID = var_name_sub(ps.conT0T1),
                               fixed.effects = c("Diet"),
                               random.effects = c("Tank"),
                               reference.vars = c("Diet,Gemma"),
                               min.abundance = 0.01,
                               num_taxa = NULL  # Defaults to nrows of sig taxa
                               )



DiffAbund_Diet <- diffAbund_Maaslin2(physeq = ps.conT0T1,
                                     physeq.ID = var_name_sub(ps.conT0T1),
                                     fixed.effects = c("Diet"),
                                     # random.effects = c("Tank"),
                                     reference.vars = c("Diet,Gemma"),
                                     min.abundance = 0.01,
                                     num_taxa = NULL  # Defaults to nrows of sig taxa
)



### Diet (initial) -----------------------------------------------------



DiffAbund_DietInitial_rEff <- diffAbund_Maaslin2(physeq = ps.T0,
                                     physeq.ID = var_name_sub(ps.T0),
                                     fixed.effects = c("Diet"),
                                     random.effects = c("Tank"),
                                     reference.vars = c("Diet,Gemma"),
                                     min.abundance = 0.01,
                                     num_taxa = NULL  # Defaults to nrows of sig taxa
                                    )


DiffAbund_DietInitial <- diffAbund_Maaslin2(physeq = ps.T0,
                                            physeq.ID = var_name_sub(ps.T0),
                                                 fixed.effects = c("Diet"),
                                                 # random.effects = c("Tank"),
                                                 reference.vars = c("Diet,Gemma"),
                                                 min.abundance = 0.01,
                                                 num_taxa = NULL  # Defaults to nrows of sig taxa
)


### Diet (final) -----------------------------------------------------


DiffAbund_DietFinal_rEff <- diffAbund_Maaslin2(physeq = ps.T1,
                                               physeq.ID = var_name_sub(ps.T1),
                                            fixed.effects = c("Diet"),
                                            random.effects = c("Tank"),
                                            reference.vars = c("Diet,Gemma"),
                                            min.abundance = 0.01,
                                            num_taxa = NULL  # Defaults to nrows of sig taxa
                                            )


DiffAbund_DietFinal <- diffAbund_Maaslin2(physeq = ps.T1,
                                          physeq.ID = var_name_sub(ps.T1),
                                          fixed.effects = c("Diet"),
                                          # random.effects = c("Tank"),
                                          reference.vars = c("Diet,Gemma"),
                                          min.abundance = 0.01,
                                          num_taxa = NULL  # Defaults to nrows of sig taxa
)



## Sex ----------------------------------------------------------------

# # No sig effects with random effects
# 
# DiffAbund_Sex_rEff <- diffAbund_Maaslin2(physeq = ps.T0,
#                                          physeq.ID = var_name_sub(ps.T0),
#                                    fixed.effects = c("Sex"),
#                                    random.effects = c("Tank"),
#                                    reference.vars = c("Sex,M"),
#                                    min.abundance = 0.01,
#                                    num_taxa = NULL  # Defaults to nrows of sig taxa
#                                     )


DiffAbund_Sex <- diffAbund_Maaslin2(physeq = ps.T0,
                                    physeq.ID = var_name_sub(ps.T0),
                                    fixed.effects = c("Sex"),
                                    # random.effects = c("Tank"),
                                    reference.vars = c("Sex,M"),
                                    min.abundance = 0.01,
                                    num_taxa = NULL  # Defaults to nrows of sig taxa
)


## Body Condition Score -------------------------------------------------------


1
# DiffAbund_BodyCond_rEff <- diffAbund_Maaslin2(physeq = ps.T0,
#                                               physeq.ID = var_name_sub(ps.T0),
#                                     fixed.effects = c("Body.Condition.Score"),
#                                     random.effects = c("Tank"),
#                                     # reference.vars = c("Sex,M"),
#                                     min.abundance = 0,
#                                     num_taxa = NULL  # Defaults to nrows of sig taxa
# )




DiffAbund_BodyCond <- diffAbund_Maaslin2(physeq = ps.T0,
                                         physeq.ID = var_name_sub(ps.T0),
                                         fixed.effects = c("Body.Condition.Score"),
                                         # random.effects = c("Tank"),
                                         # reference.vars = c("Sex,M"),
                                         min.abundance = 0,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)


### Median ~2.5 -------------------------------------------------------------

ps.obj = subset_samples(ps.conT0T1, Diet == "ZIRC") 
data = subset(df.conT0T1, Diet == "ZIRC")
BC.med <- median(data$Body.Condition.Score)


sample_data(ps.obj) <- sample.data.frame(ps.obj)  %>%
  mutate(Body.Cond_lvl = case_when(
    Body.Condition.Score < BC.med ~ "< median",
    Body.Condition.Score >= BC.med ~ ">= median"),
    .after = Body.Condition.Score)
# View(sample_data(ps.obj))

DiffAbund_BodyCondLvl.2 <- diffAbund_Maaslin2(physeq = ps.obj,
                                         physeq.ID = var_name_sub(ps.obj),
                                         fixed.effects = c("Body.Cond_lvl", "Timepoint"),
                                         # random.effects = c("Tank"),
                                         # reference.vars = c("Sex,M"),
                                         min.abundance = 0.01,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)



## Exposure  -----------------------------------------------------

# # No sig effects with random effects

# 
# DiffAbund_Exposure_rEff <- diffAbund_Maaslin2(physeq = ps.T1,
#                                               physeq.ID = var_name_sub(ps.T1),
#                                          fixed.effects = c("Exposure"),
#                                          random.effects = c("Tank"),
#                                          reference.vars = c("Exposure,Unexp"),
#                                          min.abundance = 0.01,
#                                          num_taxa = NULL  # Defaults to nrows of sig taxa
# )


DiffAbund_Exposure <- diffAbund_Maaslin2(physeq = ps.T1,
                                         physeq.ID = var_name_sub(ps.T1),
                                         fixed.effects = c("Exposure"),
                                         # random.effects = c("Tank"),
                                         reference.vars = c("Exposure,Unexposed"),
                                         min.abundance = 0,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)


### ZIRC 6mo ----------------------------------------------------------------

ps.obj = subset_samples(ps.T1, (Diet == "ZIRC" & Timepoint == "6mpf")) 


DiffAbund_ExposureZIRC <- diffAbund_Maaslin2(physeq = ps.obj,
                                              physeq.ID = var_name_sub(ps.obj),
                                              fixed.effects = c("Exposure"),
                                              # random.effects = c("Tank"),
                                              # reference.vars = c("Sex,M"),
                                              min.abundance = 0,
                                              num_taxa = NULL  # Defaults to nrows of sig taxa
)


## Infection Status -----------------------------------------------------

# # No sig effects with random effects
# 
# DiffAbund_Infection_rEff <- diffAbund_Maaslin2(physeq = ps.expFin,
#                                           physeq.ID = var_name_sub(ps.expFin),
#                                           fixed.effects = c("Infection.Status"),
#                                           random.effects = c("Tank"),
#                                           reference.vars = c("Infection.Status,Absent"),
#                                           min.abundance = 0.01,
#                                           num_taxa = NULL  # Defaults to nrows of sig taxa
# )


DiffAbund_Infection <- diffAbund_Maaslin2(physeq = ps.expFin,
                                          physeq.ID = var_name_sub(ps.expFin),
                                         fixed.effects = c("Infection.Status"),
                                         # random.effects = c("Tank"),
                                         reference.vars = c("Infection.Status,Absent"),
                                         min.abundance = 0.01,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)



## Interactions ------------------------------------------------------------

### Diet*Infection ----------------------------------------------------------


ps.obj <- get_intMatPSobj(physeq = ps.expFin, 
                          data = dt.expFin, 
                          vars = c("Diet", "Infection.Status"))


tmp.colnames <- colnames(sample_data(ps.obj)[, (ncol(sample_data(ps.obj))-1):ncol(sample_data(ps.obj))])

# # No significant effects with random effects
# 
# DiffAbund_DietInf_rEff <- diffAbund_Maaslin2(physeq = ps.obj,
#                                              physeq.ID = var_name_sub(ps.expFin),
#                                              fixed.effects = c("Diet", "Infection.Status", tmp.colnames), #c("Diet", "Exposure"),
#                                              random.effects = c("Tank"),
#                                              reference.vars = c("Diet,Gemma", "Infection.Status,Absent"),
#                                              min.abundance = 0.01,
#                                              num_taxa = NULL  # Defaults to nrows of sig taxa
# )


DiffAbund_DietInf <- diffAbund_Maaslin2(physeq = ps.obj,
                                        physeq.ID = var_name_sub(ps.expFin),
                                        fixed.effects = c("Diet", "Infection.Status", tmp.colnames), #c("Diet", "Exposure"),
                                        # random.effects = c("Tank"),
                                        reference.vars = c("Diet,Gemma", "Infection.Status,Absent"),
                                        min.abundance = 0.01,
                                        num_taxa = NULL  # Defaults to nrows of sig taxa
)

### Diet*Exposure -----------------------------------------------------------




ps.obj <- get_intMatPSobj(physeq = ps.T1, 
                                     data = dt.T1, 
                                     vars = c("Diet", "Exposure"))


tmp.colnames <- colnames(sample_data(ps.obj)[, (ncol(sample_data(ps.obj))-1):ncol(sample_data(ps.obj))])

# # No sig effects with random effects
# 
# DiffAbund_DietExp_rEff <- diffAbund_Maaslin2(physeq = ps.obj,
#                                              physeq.ID = var_name_sub(ps.T1),
#                                          fixed.effects = c("Diet", "Exposure", tmp.colnames), #c("Diet", "Exposure"),
#                                          random.effects = c("Tank"),
#                                          reference.vars = c("Diet,Gemma", "Exposure,Unexp"),
#                                          min.abundance = 0.01,
#                                          num_taxa = NULL  # Defaults to nrows of sig taxa
# )


DiffAbund_DietExp <- diffAbund_Maaslin2(physeq = ps.obj,
                                        physeq.ID = var_name_sub(ps.T1),
                                         fixed.effects = c("Diet", "Exposure", tmp.colnames), #c("Diet", "Exposure"),
                                         # random.effects = c("Tank"),
                                         reference.vars = c("Diet,Gemma", "Exposure,Unexposed"),
                                         min.abundance = 0.01,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)


### Diet*Time ---------------------------------------------------------------

ps.obj <- get_intMatPSobj(physeq = ps.conT0T1, 
                                     data = dt.conT0T1, 
                                     vars = c("Diet", "Timepoint"))

tmp.colnames <- colnames(sample_data(ps.obj)[, (ncol(sample_data(ps.obj))-1):ncol(sample_data(ps.obj))])



DiffAbund_DietTime_rEff <- diffAbund_Maaslin2(physeq = ps.obj,
                                              physeq.ID = var_name_sub(ps.conT0T1),
                                         fixed.effects = c("Diet", "Timepoint", tmp.colnames), #c("Diet", "Exposure"),
                                         random.effects = c("Tank"),
                                         reference.vars = c("Diet,Gemma", "Timepoint,3mpf"),
                                         min.abundance = 0.01,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)


DiffAbund_DietTime <- diffAbund_Maaslin2(physeq = ps.obj,
                                         physeq.ID = var_name_sub(ps.conT0T1),
                                         fixed.effects = c("Diet", "Timepoint", tmp.colnames), #c("Diet", "Exposure"),
                                         # random.effects = c("Tank"),
                                         reference.vars = c("Diet,Gemma", "Timepoint,3mpf"),
                                         min.abundance = 0.01,
                                         num_taxa = NULL  # Defaults to nrows of sig taxa
)




### Diet*Body Condition Score -----------------------------------------------

ps.obj <- get_intMatPSobj(physeq = ps.T0, 
                          data = dt.T0, 
                          vars = c("Diet", "Body.Condition.Score"))

tmp.colnames <- colnames(sample_data(ps.obj)[, (ncol(sample_data(ps.obj))-1):ncol(sample_data(ps.obj))])

# # No sig effects with random effects
# 
# DiffAbund_DietBodyCond_rEff <- diffAbund_Maaslin2(physeq = ps.obj,
#                                                   physeq.ID = var_name_sub(ps.T0),
#                                          fixed.effects = c("Diet", "Body.Condition.Score", tmp.colnames), #c("Diet", "Exposure"),
#                                          random.effects = c("Tank"),
#                                          reference.vars = c("Diet,Gemma"),
#                                          min.abundance = 0.01,
#                                          num_taxa = NULL  # Defaults to nrows of sig taxa
# )
# 


DiffAbund_DietBodyCond <- diffAbund_Maaslin2(physeq = ps.obj,
                                             physeq.ID = var_name_sub(ps.T0),
                                             fixed.effects = c("Diet", "Body.Condition.Score", tmp.colnames), #c("Diet", "Exposure"),
                                             # random.effects = c("Tank"),
                                             reference.vars = c("Diet,Gemma"),
                                             min.abundance = 0.01,
                                             num_taxa = NULL  # Defaults to nrows of sig taxa
)



## Save --------------------------------------------------------------------



# Save R objects for later loading:
save(DiffAbund_Diet_rEff, DiffAbund_Diet, 
      DiffAbund_DietInitial_rEff, DiffAbund_DietInitial, 
      DiffAbund_DietFinal_rEff, DiffAbund_DietFinal, 
      DiffAbund_Sex, 
      DiffAbund_BodyCond,
      DiffAbund_Exposure, 
      DiffAbund_Infection, 
      DiffAbund_DietInf, DiffAbund_DietExp,
      DiffAbund_DietTime_rEff, DiffAbund_DietTime, 
      DiffAbund_DietBodyCond,  # Objects to save
     file = paste0(objects.path, "/DiffAbund-OBJs_ZF-DietStudy_", Sys.Date() ,".Rdata"))  # file path

}

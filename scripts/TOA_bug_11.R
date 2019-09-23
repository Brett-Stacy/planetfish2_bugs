## 13 Sep 19 - R 3.5.3 - earthfish version 0.1.0

## Let's find that bug!

## Manual tuning of growth CV

## Packages ----
# library(planetfish2)
library(earthfish)
library(casal)

## House ----

# rm(list = ls())

## number of iterations and scenario name
n_iters <- 1000
scenario <- "TOA_bug_11"

## define a file name
file_name <- scenario

## Brett's file path
file_path <- paste0("C:/Users/bstacy/Documents/GitHub/planetfish2_bugs/Output/", scenario, "/")
## PB file path
# file_path <- paste0("C:/Work/Manuscripts/2019_Stacy_etal_Exploratory_fisheries/Antarctic_toothfish_scenario/single_region/", scenario, "/")

## not sure what requires the WD to be set
setwd(file_path)
getwd()





### Specify Antarctic toothfish biological parameters ----
# Ages
TOA_max_age = 35 # Yates and Ziegler 2018

# Growth. Von Bertalanfy. Yates and Ziegler 2018
TOA_L_inf = 1565
TOA_K     = 0.146
TOA_t_0   = 0.015
TOA_CV    = 0.035 # changed to test with Katie

# # Growth. Von Bertalanfy. TOP
# TOA_L_inf = 2870
# TOA_K     = 0.02056
# TOA_t_0   = -4.28970
# TOA_CV    = 0.100

# Growth. Von Bertalanfy. TOA Mormede et al. 2014
# TOA_L_inf = 1690.7
# TOA_K     = 0.093
# TOA_t_0   = -0.256
# TOA_CV    = 0.102

# Growth. Von Bertalanfy. TOA Other
# TOA_L_inf = 2265
# TOA_K     = 0.093
# TOA_t_0   = -0.256
# TOA_CV    = 0.102

# Weight-Length. Yates and Ziegler 2018
TOA_wl_c = 3.0088e-12
TOA_wl_d = 3.2064

# Maturity. Yates and Ziegler 2018
TOA_maturity_ogive = "logistic"
TOA_a_50 = 14.45 # 14.45
TOA_a_95 = 6.5  # 6.5

# Natural Mortality. Yates and Ziegler 2018
TOA_M = 0.13

# Stock-recruitment Steepness h from Beverton-Holt
TOA_h = 0.75







### Model parameters ----
## mean recruitment

########## STUDY PERIOD
study_year_range = c(1990, 2010) # c(1968, 2018)
no_fish_range = 1 ##** Don't fish the first 10 years


R_mu <- 1e6 # 1e6
## recruitment variability
R_sigma <- 3e-1 ############################################### 3e-01
# B0_calc_method = "init_age" # one of "casal", "stoch", or "init_age"

# Total catch across single area
total_catch <- 1000


########## SAMPLING
n_years_aged = 10 ##** age fish for last 20 years. used in para$ass$sample_years
age_years = if(n_years_aged == 0) NULL else((study_year_range[2] - n_years_aged):study_year_range[2])
### BS: 30/05/19 add len_years
n_years_lengthed = 0
len_years = if(n_years_lengthed == 0) NULL else((study_year_range[2] - n_years_lengthed):study_year_range[2])
# The number of tags released in area 1 each year ##### just area 1?
n_tags = 2500 # 2500
# Number of years to release tags. leave out last year.
n_years_tags = 5 # 5
tag_years = (study_year_range[2] - n_years_tags + 1):study_year_range[2] - 1

## define longline selectivity
LL_sel <- list(top=10, sigma_left=2, sigma_right=10)
## add a logistic selectivity


### OM ----
## specify the default parameters
para	<- get_om_data()

para$control$Assyr_range = para$om$year # set assessment year range to match om year range

# Set age parameters
para$om$age = c(1, TOA_max_age)
para$om$ages = 1:TOA_max_age
para$om$n_ages = length(para$om$ages)
para$om$names_ages = paste0("age", "_", para$om$ages, sep = "")

# Set growth parameters
para$om$growth = list(f = c(TOA_L_inf, TOA_K, TOA_t_0, TOA_CV),
                      m = c(TOA_L_inf, TOA_K, TOA_t_0, TOA_CV))

# Set WL parameters
para$om$WL = list(f = list(a = TOA_wl_c, b = TOA_wl_d), m = list(a = TOA_wl_c, b = TOA_wl_d))

# Set maturity parameters
para$om$pin_mat = TOA_maturity_ogive
para$om$maturity = list(f = list(x50 = TOA_a_50, x95 = TOA_a_95), m = list(x50 = TOA_a_50, x95 = TOA_a_95))

# Set natural mortality parameters
para$om$natM = rep(TOA_M, para$om$n_ages)

# Set Stock-recruitment h
para$om$rec_h = TOA_h




## redefine the recruitment used for spawning biomass
para$om$rec_mu <- R_mu
para$om$rec_sigma <- R_sigma
## redefine the number of areas
para$om$region <- c(1,1)
para$om$regions <- c(1)
para$om$n_regions <- length(para$om$regions)

## B0 is determined by method 1,2, or 3
para$om$B0_calculation_method = B0_calc_method



## redefine the fisheries
para$om$fishery <- c("LL1")
para$om$n_fisheries <- length(para$om$fishery)

## set selectivity to NULL then define selectivities
para$om$pin_sel <- NULL
para$om$pin_sel$LL1 <- "double_normal"
para$om$select$LL1 <- LL_sel

## catches for the two fisheries
para$om$catch	<- array(data=0, dim=c(para$om$n_years, para$om$n_fisheries,
                                     para$om$n_seasons, para$om$n_regions),
                       dimnames=list("Year"=para$om$years,"Fishery"=para$om$fishery,
                                     "Season"=para$om$seasons,"Region"=para$om$regions))
## fille the arrays with the catch in the specified proportions
para$om$catch[,"LL1", 1,1]	<- rep(total_catch, para$om$n_years)
para$om$catch[1,,,]	 		<- 0		# Catch in first year set to 0, such that SSB in first year is unfished biomass

## overwrite the effort (not sure it is used)
para$om$effort <- para$om$catch

## catch splits (I don't think it is used)
para$om$catchsplits	<- array(data=0, dim=c(para$om$n_years, para$om$n_fisheries,
                                           para$om$n_seasons,para$om$n_regions),
                             dimnames=list("Year"=para$om$years,"Fishery"=para$om$fishery,
                                           "Season"=para$om$seasons,"Region"=para$om$regions))
para$om$catchsplits[,"LL1", 1,1] <- rep(1, para$om$n_years)
para$om$catchsplits[1,,,]	<- 0		# Catch in first year set to 0, such that SSB in first year is unfished biomass

## remove Trawl selectivity
##** better to just specify the complete selectivity
para$om$select$Trawl <- NULL

## define the movement matrix for the population ################## BS 8/5: may have to set these == 0 because there are default values in there?
###* do we need something in the movement

# move_by_age <- as.data.frame(matrix(c(rep(move_1_to_2, para$om$n_ages),
#                                       rep(move_2_to_1, para$om$n_ages)),
#                                     nrow=2, ncol=para$om$n_ages, byrow=TRUE))
## for Sex, Year and Season, 0 means the movement rule represents all of that category
# para$om$move_rules <- data.frame("Origin" = c(1,2),"Destination" = c(2,1), "Sex" = c(0,0),
#                                  "Year" = c(0,0), "Season" = c(0, 0),
#                                  setNames(move_by_age, para$om$names_ages))

## define the movement matrix
# move_tag_by_age <- as.data.frame(matrix(c(rep(move_tags_1_to_2, para$om$n_ages),
#                                           rep(move_tags_2_to_1, para$om$n_ages)),
#                                         nrow=2, ncol=para$om$n_ages, byrow=TRUE))

## separate dataframe for the movement of tags
# para$om$move_tag_rules <- data.frame("Origin" = c(1,2),"Destination" = c(2,1), "Sex" = c(0,0),
#                                      "Year" = c(0,0), "Season" = c(0, 0),
#                                      setNames(move_tag_by_age, para$om$names_ages))

### Sampling ----
## only release tags in Area 1
para$sampling$pin_tag_N <- "Fixed" # this defines how tagging specified
para$sampling$tag_N <- c(n_tags, 0)
para$sampling$tag_rate <- c(2,0)

# Change sampling length classes and n_classes
# para$sampling$len_classes = seq(100, round(L_inf, digits = -2), 50)
# para$sampling$len_classes = seq(100, 3000, 50) # seq(300, 2000, 50) # yates and ziegler 2018
# para$sampling$n_lengths = length(para$sampling$len_classes)

## Tagging selectivity
para$sampling$pin_tag_sel <- list()
para$sampling$pin_tag_sel[[para$om$fishery[1]]] <- para$om$pin_sel[[para$om$fishery[1]]]
## define the selectivity parameters
para$sampling$tag_select <- list()
para$sampling$tag_select[[para$om$fishery[1]]] <- para$om$select[[para$om$fishery[1]]]

###* Brett thinks this isn't being implemented. 23/5/19: It is being implemented but it isn't clear where it shows up in CASAL files.
## age 1000 fish in Region 1
para$sampling$catchage_N <- 1000 # 1000
#### BS 30/05/19 add catchlen_N too
# para$sampling$catchlen_N = 1000 # 1000




### Assessment ----
para <- get_casal_para(para)

## Plust Group? BS 6/6/19 CONCLUSION: no effect.
# para$ass$age_plus_group = "False"

## Tag sampling type? BS 26/06/19 CONCLUSION: this feature hasn't been coded yet, see create_estim_cls()
# para$ass$tag_sampling_type = "age"


### add TOA LHPs to ASSESSMENT


para$ass$estgrowth = list(c(TOA_L_inf, TOA_K, TOA_t_0, TOA_CV))

para$ass$estWL = list(c(TOA_wl_c, TOA_wl_d))

para$ass$estpin.mat = TOA_maturity_ogive
para$ass$estmaturity = list(c(TOA_a_50, TOA_a_95))
para$ass$maturity_props_all = c("allvalues ", round(ogive("logistic", para$ass$ages, list(x50 = TOA_a_50, x95 = TOA_a_95)), 4))

para$ass$estnatM[[1]] = TOA_M

para$ass$rec_steepness = TOA_h








### specify regions and fisheries in the AM
para$ass$regions <- "R1"
para$ass$Fish <- matrix(c("LL", 1, "R1", "SelLL", "qLL", 1, 100, 100),
                        ncol=8, byrow=TRUE,
                        dimnames=list(c(),c("Fishery","Season","Region","Sel","q",
                                            "ProjCatch", "catchage_N", "catchlen_N")))
## match the fisheries in the OM with the AM
para$ass$match_fishery <- matrix(c("LL1", "LL"), ncol=1, byrow=TRUE,
                                 dimnames=list(c("OM","Ass"),c()))
## match the regions in the OM with the AM
para$ass$match_region <- matrix(c(1, "R1"), ncol=1, byrow=TRUE,
                                dimnames=list(c("OM","Ass"),c()))
## Initial values and bounds for B0

## make these boundries wider
## lower upper, casal lower casal upper
para$ass$initialB0 <-  c(5e3, 5e5, 5e3, 5e5)
# para$ass$initialB0 <-  c(2e5, 3e5, 5e4, 1e6)
para$ass$B0 <-  runif(1, para$ass$initialB0[1],para$ass$initialB0[2])
para$ass$estim_initialization.B0 <- c("1", (para$ass$initialB0[3]),
                                      (para$ass$initialB0[4]),
                                      "uniform", "initialization.B0")

para$ass$list_fishery <- "LL"
para$ass$list_sel <- "SelLL"
para$ass$list_season <- "1" #(this was "1"  "1")
para$ass$list_region <- "R1"

para$ass$list_q <- "qLL"
para$ass$selectivity_names <- "SelLL"
## Set to NULL to have no selectivity estimated selectivity
para$ass$estimate_selectivity <- para$ass$selectivity_names
para$ass$qq_names <- "qLL"
## specify the selectivity in the assessment
para$ass$selN_all <- list()
para$ass$selN_all[[1]] <- c("double_normal", as.character(LL_sel))
##** removing Trawl selectivity here, not sure what these values should be
para$ass$est_selN_all[[1]] <- NULL

para$ass$qqvalues ## there are 3 of these, should there only be one?
para$ass$future_constant_catches <- 200 # what does this specify?
# BS: ##### change para$ass$sample_years to accomodate edit above: expand study years.
# BS: 30/05/19 add catchlen_yrs to activate sampling lengths of fish ass well as age
para$ass$sample_years = am_sampling(years = para$om$years,
                                    ycurr = para$om$year[2],
                                    catchage_yrs = age_years,
                                    catchlen_yrs = len_years,
                                    tagging_yrs = tag_years)$sample_years ##** This is the range of years various sampling (sizing, ageing, tagging, etc.) took place. note, when long year range, tagging row doesn't show up when print.


## movement parameters
para$ass$migration_est_pin <- FALSE
para$ass$n_migrations <- 0
## for safety set the other parameters to NULL
para$ass$migration_names <- NULL
para$ass$migration_times <- NULL
para$ass$migrate_from <- NULL
para$ass$migrate_to <- NULL
para$ass$migrators <- NULL
para$ass$rates_all <- NULL
para$ass$migration_rates_all_low <- NULL
para$ass$migration_rates_all_upp <- NULL
para$ass$migration_rates_all_prior <- NULL

## might need modify R1 values as well
para$ass$output$`numbers_at[Numbers_at_age_R2]` <- NULL

## Modify the CASAL control parameters
para[["control"]]$casal_path <- file_path
para[["control"]] <- update_casal_file_names(para[["control"]]) 	# Update casal file names with in/outputprefix
## turn the TAC finder off (quicker running) and we dont need the TAC finder
para[["control"]]$pin_TAC_finder <- 0

## set CASAL to calculate SSB after 100% F and M
para$ass$spawning_part_mort <- 1

##*** now we modify the tag loss rate in the AM
para$ass$tag_shedding_rate <- 0.0084


#### Turn off estimations
para$ass$estimate_selectivity = NULL
para$ass$estim_recruitment.YCS[[2]] = rep(1, para$om$n_years)
para$ass$estim_recruitment.YCS[[3]] = rep(1, para$om$n_years)
# para$ass$estim_recruitment.YCS[4] = "uniform"

#### Turn off other stuff?
# para$ass$size_based = "True"
# para$ass$rec_sigma = 0



# ## this appears to be correctly allocating arrays
# res	<- setup_om_objects(para=para)
# ## now populate the OM arrays
# res	<- populate_om_objects(para=para, res=res)
# ## specify the initial population
# res <- get_initial_pop(para, res=res)
# res <- run_annual_om(para, res=res, FALSE)


## Check if OM matches AM parameters ----
## source check_lhps and check the life history parameters
# source("../../check_lhps.R")
# check_lhps(para)

check_match(para)


### Loop ----

## specify objects to save simulation outputs
dim_names	<- c("OM_ssb0", paste0("OM_ssb_R1_", para$om$years),
               paste0("OM_rec_R1_", para$om$years),
               "AM_ssb0_",paste0("AM_ssb_", para$om$years),
               paste0("AM_rec_", para$om$years),
               "SelLL_P1", "SelLL_P2", "SelLL_P3", "Starting_AM_B0")


## specify objects to save simulation outputs
dim_names2	<- c("OM_ssb0", paste0("OM_ssb_R1_", para$om$years),
               "AM_ssb0_",paste0("AM_ssb_", para$om$years))

# #################################################################### add different dim_names if using AT selectivity
# dim_names	<- c("OM_ssb0", paste0("OM_ssb_R1_", para$om$years), paste0("OM_ssb_R2_",para$om$years),
#                paste0("OM_rec_R1_", para$om$years), paste0("OM_rec_R2_", para$om$years),
#                "AM_ssb0_",paste0("AM_ssb_", para$ass$years), paste0("AM_rec_", para$ass$years),
#                "SelLL_P1", "SelLL_P2", "SelLL_P3", "SelLL_P4", "SelLL_P5", "Starting_AM_B0")
#

dim_length <- length(dim_names)
dim_length2 = length(dim_names2)

## construct the output array
output <- array(data = 0, dim = c(n_iters, dim_length),
                dimnames = list("Iter"=1:n_iters, dim_names))
output2 = array(data = 0, dim = c(n_iters, dim_length2),
                dimnames = list("Iter"=1:n_iters, dim_names2))
## some conveniences for accessing arrays in the OM
R1 <- 1
S1 <- para$om$season[2]
## paths for the CASAL outputs
casal_path <- para[["control"]]$casal_path
mpd_dat <- para[["control"]]$mpd_dat
output_log <- para[["control"]]$output_log

##* Save the parameter specifications (in case something goes wrong)
# sink(paste0(file_name, "_Para.txt"))
# para
# sink()

## add the scenario name to para
para$scenario$name <- scenario

## a better way to save the parameters is to save an Rds
saveRDS(para, file = paste0(para$control$casal_path, "para.Rds"))

## loop over the number of iterations
for(i_iter in 1:n_iters){
  ## Set up om objects
  res	<- setup_om_objects(para=para)
  #### Populate om objects (mod) with biological data (M and fecundity), fishery effort & selectivity, and observation sample sizes
  res	<- populate_om_objects(para=para, res=res)
  #### Get initial population numbers and calculate SSB0
  ## the warning about init_age_comp' is getting anoying
  res	<- suppressWarnings(get_initial_pop(para=para, res=res))
  #### Run Annual OM loops with/without assessment
  #para[["control"]]$pin_casal_assess <- 1
  res <- run_annual_om(para=para, res=res) #, intern=TRUE) #
  ## Save res object for inspection
  if(i_iter == 1) saveRDS(res, file = paste0(para$control$casal_path, "res.Rds"))
  ## set output quantities to NULL, could use rm() instead
  ssb <- rec <- SSB0 <- OM_SSB_R1 <- OM_Rec_R1 <- NULL
  ## calculated quantities
  ## ssb[quant, year, sex, season, area] # rec is the same
  ssb <- res$mod$ssb
  rec <- res$mod$rec
  ## Spawning Biomass
  OM_SSB0 <- res$mod$ssb0
  OM_SSB_R1 <- apply(ssb[1,,,S1,R1],c(1),sum)
  OM_SSB_R12 = rowSums(calc_SSB("res", res$pop$n, para$om$growth$f, para$om$WL$f, para$om$pin_mat, para$om$maturity$f))
  ## Recruitment
  OM_Rec_R1 <- apply(rec[1,,,S1,R1],c(1),sum)
  ##*** Potentially add selectivity, however, perhaps not required
  ## length of output
  om_ncols <- length(OM_SSB0) + length(OM_SSB_R1) + length(OM_Rec_R1)
  om_ncols2 = length(OM_SSB0) + length(OM_SSB_R12)
  # apply(ssb(stock)[,,,om$season[2],],c(2),sum)
  ##** I actually want the value for area 1 in my simulations
  ##** I'll need to sum up ssb for the first year and area (example below)
  output[i_iter, 1:om_ncols] <- c(OM_SSB0, OM_SSB_R1, OM_Rec_R1)
  output2[i_iter, 1:om_ncols2] = c(OM_SSB0, OM_SSB_R12)
  ## add the AM output if it exists for this iteration
  if(file.exists(paste0(casal_path, para[["control"]]$mpd_dat)) &
     length(scan(file = paste0(casal_path, para[["control"]]$mpd_dat),
                 what = "character")) > 0){
    nname1 <- para[["control"]]$output_log		# instead of output_logYear = take output log file from last assessment year only
    casal_quants <- casal::extract.quantities(file=nname1, path=casal_path)
    casal_freeparams <- casal::extract.free.parameters(file=nname1, path=casal_path)
    ## quantities form the Assessment model
    AM_SSB0 <- casal_quants$B0
    AM_SSB_R1 <- casal_quants$SSBs$SSB
    AM_Rec_R1 <- casal_quants$recruitments$recruitment
    ## save the selectivity as it can be useful for investigating bias
    if(length(casal_freeparams$`selectivity[SelLL].all`) > 0){
      AM_SelLL	<- c(casal_freeparams$`selectivity[SelLL].all`, para$ass$B0)
    }else AM_SelLL <- c(0, 0, 0, 0)
    ## add the rest of the output
    output[i_iter, (om_ncols+1):ncol(output)] <- c(AM_SSB0, AM_SSB_R1, AM_Rec_R1, AM_SelLL)
    output2[i_iter, (om_ncols2+1):ncol(output2)] = c(AM_SSB0, AM_SSB_R1)
  }
  # print(round(c(OM_SSB0, OM_SSB_R1, OM_SSB_R2, OM_Rec_R1, OM_Rec_R2),0))
  # print(round(c(AM_SSB0, AM_SSB_R1, AM_Rec_R1, AM_SelLL),0))
  # OM_SSB_R1 - AM_SSB_R1
  # OM_Rec_R1 - AM_Rec_R1
} # end MCMC loop


### Save Output ----
# write to file
write.csv(output, file=paste0(casal_path,file_name, "_Niter_", n_iters, ".csv"),
          quote=FALSE, na="NA", row.names=FALSE)

write.csv(output2, file=paste0(casal_path,file_name, "_Niter_", n_iters, "output2", ".csv"),
          quote=FALSE, na="NA", row.names=FALSE)




################################## BS Plots ----

library(fishnostics2)
temp1 = read.csv(paste0(casal_path, "TOA_bug_11_Niter_1000.csv"))
years = 1990:2010
true_ssb1 = temp1[, grep("OM_ssb_R1", colnames(temp1))]
est_ssb1 = temp1[, grep("AM_ssb_", colnames(temp1))]

# TS plot
colnames(true_ssb1) = years
plot_ts_uncertainty(d = true_ssb1/1000, d2 = est_ssb1/1000)
legend("bottomleft",c("True SSB from OM", "Estimated SSB from CASAL"),
       col=c("blue", "red"), lty=c(1,2), lwd=2, bty="n")




par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1")
plot_SSB(output, item = "AM_ssb_", mean = F)


par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1", mean = F, ylim = c(33000, 60000))
plot_SSB(output, item = "AM_ssb_", mean = F, ylim = c(33000, 60000))

# With Recruitment Variability Output1
temp1 = read.csv("TOA_bug_2_Niter_301.csv")
par(mfrow = c(1,2))
plot_SSB(temp1, item = "OM_ssb_R1", mean = F, ylim = c(9000, 60000))
plot_SSB(temp1, item = "AM_ssb_", mean = F, ylim = c(9000, 60000))


par(mfrow = c(1,3))
plot_SSB_err(temp1, type = "initial")
plot_SSB_err(temp1, type = "current")
plot_SSB_err(temp1, type = "status")

boxplot(temp1[,"OM_ssb_R1_1990"])
boxplot(temp1[, "AM_ssb_1990"])


library(fishnostics2)
years = 1990:2010
true_ssb1 = temp1[, grep("OM_ssb_R1", colnames(temp1))]
est_ssb1 = temp1[, grep("AM_ssb_", colnames(temp1))]

colnames(true_ssb1) = years
plot_ts_uncertainty(d = true_ssb1/1000, d2 = est_ssb1/1000)
legend("bottomleft",c("True SSB from OM", "Estimated SSB from CASAL"),
       col=c("blue", "red"), lty=c(2,1), lwd=2, bty="n")




# With Recruitment Variability Output2
temp = read.csv("TOA_bug_2_Niter_101output2.csv")
par(mfrow = c(1,2))
plot_SSB(temp, item = "OM_ssb_R1", mean = F, ylim = c(9000, 60000))
plot_SSB(temp, item = "AM_ssb_", mean = F, ylim = c(9000, 60000))


par(mfrow = c(1,3))
plot_SSB_err(temp, type = "initial")
plot_SSB_err(temp, type = "current")
plot_SSB_err(temp, type = "status")

boxplot(temp[,"OM_ssb_R1_1990"])
boxplot(temp[, "AM_ssb_1990"])


library(fishnostics2)
years = 1990:2010
om_names = paste0("OM_ssb_R1_", years)
am_names = paste0("AM_ssb_", years)
true_ssb = temp[, grep("OM_ssb_R1", colnames(temp))]
est_ssb = temp[, grep("AM_ssb_", colnames(temp))]

colnames(true_ssb) = years
plot_ts_uncertainty(d = true_ssb/1000, d2 = est_ssb/1000) # d is blue, d2 is red
legend("bottomleft",c("True SSB from OM", "Estimated SSB from CASAL"),
       col=c("blue", "red"), lty=c(2,1), lwd=2, bty="n")

err <- process_two_series(res = temp, true_vars = om_names, est_vars = am_names,
                             col_names = years, FUN = "rel_err")
boxplot(err, ylab="Relative error", main="Baseline Brett")
abline(h=0, col="red")











temp = read.csv("TOA_bug_2_Niter_20.csv")
par(mfrow = c(1,2))
plot_SSB(temp, item = "OM_ssb_R1", ylim = c(9000, 60000), main = "TOA_bug_2_Niter_20")
plot_SSB(temp, item = "AM_ssb_", mean = F, ylim = c(9000, 60000))








par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1", ylim = c(350000, 500000))
plot_SSB(output, item = "AM_ssb_", mean = F, ylim = c(350000, 500000))

par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1", ylim = c(0, 30000))
plot_SSB(output, item = "AM_ssb_", mean = F, ylim = c(0, 30000))


par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1", ylim = c(55000, 140000))
plot_SSB(output, item = "AM_ssb_", mean = F, ylim = c(55000, 140000))


par(mfrow = c(1,2))
plot_SSB(output, item = "OM_ssb_R1", ylim = c(200000, 330000))
plot_SSB(output, item = "AM_ssb_", mean = F, ylim = c(200000, 330000))




















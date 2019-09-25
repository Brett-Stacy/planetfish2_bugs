## Brett Stacy
##
## 24/9/19
##
## PATAGONIAN TOOTHFISH BASELINE SIMULATION
## FOR CHAPTER 3
## Mess with CV. Specifically see what happens in sample_lengths equation at end of script


n_iters = 1

## Packages ----
library(earthfish)
library(casal)
library(fishnostics2)

## House ----
casal_path = "C:/Users/STA384/Documents/GitHub/planetfish2_bugs/Output/TOA_bug_13/"
# casal_path = "C:/Users/bstacy/Work/Chapter_3_output/TOP/baseline/"
setwd(casal_path)
para = get_om_data()

## LHP ----

# Ages
max_age = 30

# Growth. Von Bertalanfy
L_inf = 2870
K     = 0.02056
t_0   = -4.28970
CV    = 0.100

# Weight-Length
wl_c = 2.59e-12
wl_d = 3.2064

# Maturity
maturity_ogive = "ramp"
mat_param1     = 11
mat_param2     = 17

# Natural Mortality
M = 0.155

# Stock-recruitment Steepness h from Beverton-Holt
h = 0.7

## Model Parameters ----

########## STUDY PERIOD
study_year_range = c(1990, 2010)
ass_year_range   = c(1990, 2010)
no_fish_range    = 1


########## STUDY AREA
n_regions = 1


########## POPULATION
R_mu           = 1.5e7
R_sigma        = 0.3
B0_calc_method = "casal" # one of "casal", "stoch", or "init_age"


########## FISHERY
fishery_names = c("LL1", "LL2")
catch_LL1     = 6000
catch_LL2     = 0
pin_sel       = "double_normal"
LL_sel        = list(top=10, sigma_left=2, sigma_right=10)


########## DATA COLLECTION
# catchage
n_years_aged = 10
n_aged       = 1000
age_years    = if(n_years_aged == 0) NULL else((study_year_range[2] - n_years_aged):study_year_range[2])

# catchlen
n_years_lengthed = 0
len_years        = if(n_years_lengthed == 0) NULL else((study_year_range[2] - n_years_lengthed):study_year_range[2])

# tagging
n_tags       = 2500
n_years_tags = 5
tag_years    = (study_year_range[2] - n_years_tags + 1):study_year_range[2] - 1








## Control ----

para$control$Assyr_range         = ass_year_range
para[["control"]]$casal_path     = casal_path
para[["control"]]$pin_TAC_finder = 0



## OM ----

########## LHP
# age
para$om$age        = c(1, max_age)
para$om$ages       = 1:max_age
para$om$n_ages     = length(para$om$ages)
para$om$names_ages = paste0("age", "_", para$om$ages, sep = "")

# growth
para$om$growth = list(f = c(L_inf, K, t_0, CV),
                      m = c(L_inf, K, t_0, CV))

# WL
para$om$WL = list(f = list(a = wl_c, b = wl_d), m = list(a = wl_c, b = wl_d))

# maturity
para$om$pin_mat  = maturity_ogive
para$om$maturity = switch(para$om$pin_mat,
                          ramp     = list(f = list(start = mat_param1, peak = mat_param2), m = list(start = mat_param1, peak = mat_param2)),
                          logistic = list(f = list(x50 = mat_param1, x95 = mat_param2), m = list(x50 = mat_param1, x95 = mat_param2)))

# natural mortality
para$om$natM = rep(M, para$om$n_ages)

# stock-recruitment h
para$om$rec_h = h



########## POPULATION
para$om$rec_mu    = R_mu
para$om$rec_sigma = R_sigma
para$om$B0_calculation_method = B0_calc_method



########## STUDY AREA
para$om$region    = c(1, n_regions)
para$om$regions   = c(1:n_regions)
para$om$n_regions = length(para$om$regions)



########## FISHERY
para$om$fishery     = fishery_names
para$om$n_fisheries = length(para$om$fishery)
para$om$pin_sel     = list(LL1 = pin_sel, LL2 = pin_sel)
para$om$select      = list(LL1 = LL_sel, LL2 = LL_sel)

# catch
para$om$catch	= array(data=0, dim=c(para$om$n_years, para$om$n_fisheries,
                                     para$om$n_seasons, para$om$n_regions),
                       dimnames=list("Year"=para$om$years,"Fishery"=para$om$fishery,
                                     "Season"=para$om$seasons,"Region"=para$om$regions))
para$om$catch[, para$om$fishery[1], 1, 1] = rep(catch_LL1, para$om$n_years)
para$om$catch[, para$om$fishery[2], 1, 1] = rep(catch_LL2, para$om$n_years)
para$om$catch[1:no_fish_range, , , ]	    = 0

# effort
para$om$effort = para$om$catch

# catch splits. For allocating TAC split between fisheries
para$om$catchsplits	= array(data=0, dim=c(para$om$n_years, para$om$n_fisheries,
                                           para$om$n_seasons,para$om$n_regions),
                             dimnames=list("Year"=para$om$years,"Fishery"=para$om$fishery,
                                           "Season"=para$om$seasons,"Region"=para$om$regions))
para$om$catchsplits[, para$om$fishery[1], 1, 1] = rep(1, para$om$n_years)
para$om$catchsplits[1:no_fish_range, , , ]	    = 0



## Sampling ----

# tags
para$sampling$pin_tagging = TRUE
para$sampling$pin_tag_N   = "Fixed" # "Fixed" or "Catch"
para$sampling$tag_N       = c(n_tags, 0)

# selectivity
para$sampling$pin_tag_sel = list(LL1 = pin_sel, LL2 = pin_sel)
para$sampling$tag_select  = list(LL1 = LL_sel, LL2 = LL_sel)

# catchA
para$sampling$catchage_N  = n_aged




## ASS ----
para = get_casal_para(para)


########## LHP
para$ass$estgrowth          = list(c(L_inf, K, t_0, CV))
para$ass$estWL              = list(c(wl_c, wl_d))
para$ass$estpin.mat         = maturity_ogive
para$ass$estmaturity        = para$om$maturity[[1]]
para$ass$maturity_props_all = c("allvalues ", round(ogive(para$ass$estpin.mat, para$ass$ages, para$ass$estmaturity), 4))
para$ass$estnatM[[1]]       = M
para$ass$rec_steepness      = h


########## POPULATION
para$ass$initialB0               = c(5e3, 5e5, 5e3, 5e5)
para$ass$B0                      = runif(1, para$ass$initialB0[1],para$ass$initialB0[2])
para$ass$estim_initialization.B0 = c("1", (para$ass$initialB0[3]),
                                      (para$ass$initialB0[4]),
                                      "uniform", "initialization.B0")



########## STUDY AREA
para$ass$regions      = "R1"
para$ass$list_region  = "R1"
para$ass$match_region = matrix(c(1, "R1"), ncol=1, byrow=TRUE,
                                dimnames=list(c("OM","Ass"),c()))
para$ass$list_season  = "1"


########## FISHERY
# name
para$ass$list_fishery  = "LL"
para$ass$list_sel      = "SelLL"
para$ass$Fish          = matrix(c("LL", 1, "R1", "SelLL", "qLL", 1, 100, 100),
                                 ncol=8, byrow=TRUE,
                                 dimnames=list(c(),c("Fishery","Season","Region","Sel","q",
                                            "ProjCatch", "catchage_N", "catchlen_N")))
para$ass$match_fishery = matrix(c("LL1", "LL"), ncol=1, byrow=TRUE,
                                 dimnames=list(c("OM","Ass"),c()))

# selectivity
para$ass$selN_all[[1]]        = c(pin_sel, as.character(LL_sel))
para$ass$selectivity_names    = "SelLL"


# estimates
para$ass$qq_names                   = NULL # when NULL, doesn't estimate q
para$ass$estimate_selectivity       = NULL # when NULL, doesn't estimate selectivity
para$ass$estim_recruitment.YCS[[1]] = 0 # when != 1, doesn't estimate YCS
para$ass$n_migrations               = 0 # when 0, turns off migration from pop and est csl files


# sampling
para$ass$sample_years = am_sampling(years = para$om$years,
                                    ycurr = para$om$year[2],
                                    catchage_yrs = age_years,
                                    catchlen_yrs = len_years,
                                    tagging_yrs = tag_years)$sample_years

# output
para$ass$output$`numbers_at[Numbers_at_age_R2]`       = NULL
para$ass$output$`numbers_at[Numbers_at_age_R1]`$years = para$ass$years


# future catch
para$ass$future_constant_catches = 200 # what does this specify?


# other
para$ass$spawning_part_mort = 1 # set CASAL to calculate SSB after 100% F and M



## Check ----

check_match(para)

## Loop ----


## specify objects to save simulation outputs
dim_names	<- c("OM_ssb0", paste0("OM_ssb_R1_", para$om$years),
               paste0("OM_rec_R1_", para$om$years),
               "AM_ssb0_",paste0("AM_ssb_", para$om$years),
               paste0("AM_rec_", para$om$years),
               "SelLL_P1", "SelLL_P2", "SelLL_P3", "Starting_AM_B0")


dim_length <- length(dim_names)


## construct the output array
output <- array(data = 0, dim = c(n_iters, dim_length),
                dimnames = list("Iter"=1:n_iters, dim_names))

## some conveniences for accessing arrays in the OM
R1 <- 1
S1 <- para$om$season[2]
## paths for the CASAL outputs
casal_path <- para[["control"]]$casal_path
mpd_dat <- para[["control"]]$mpd_dat
output_log <- para[["control"]]$output_log


# ## add the scenario name to para
# para$scenario$name <- scenario

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
  ## Recruitment
  OM_Rec_R1 <- apply(rec[1,,,S1,R1],c(1),sum)
  ##*** Potentially add selectivity, however, perhaps not required
  ## length of output
  om_ncols <- length(OM_SSB0) + length(OM_SSB_R1) + length(OM_Rec_R1)
  # apply(ssb(stock)[,,,om$season[2],],c(2),sum)
  ##** I actually want the value for area 1 in my simulations
  ##** I'll need to sum up ssb for the first year and area (example below)
  output[i_iter, 1:om_ncols] <- c(OM_SSB0, OM_SSB_R1, OM_Rec_R1)
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
  }
  # print(round(c(OM_SSB0, OM_SSB_R1, OM_SSB_R2, OM_Rec_R1, OM_Rec_R2),0))
  # print(round(c(AM_SSB0, AM_SSB_R1, AM_Rec_R1, AM_SelLL),0))
  # OM_SSB_R1 - AM_SSB_R1
  # OM_Rec_R1 - AM_Rec_R1
} # end MCMC loop














## Save ----

write.csv(output, file=paste0(casal_path, "Output_Niter_", n_iters, ".csv"),
          quote=FALSE, na="NA", row.names=FALSE)

## Plots ----


temp1 = read.csv(paste0(casal_path, "Output_Niter_1.csv"))
years = 1990:2010
true_ssb1 = temp1[, grep("OM_ssb_R1", colnames(temp1))]
est_ssb1 = temp1[, grep("AM_ssb_", colnames(temp1))]

# TS plot
colnames(true_ssb1) = years
plot_ts_uncertainty(d = true_ssb1/1000, d2 = est_ssb1/1000)
legend("bottomleft",c("True SSB from OM", "Estimated SSB from CASAL"),
       col=c("blue", "red"), lty=c(1,2), lwd=2, bty="n")

##### sample_lengths ----


ssages <- para$om$ages + para$om$growth_props
ln = res$fleet$LL1$landings_n[,"2010",1,1,1]
plot(ln)


# obs[[ff]]$tag_len_n[,y,i,ss,rr] <- sample_lengths(ssages,sampling$len_classes,
#                                       obs[[ff]]$tag_age_n[,y,i,ss,rr],
#                                       om$growth[[om$sex[i]]])


sl = sample_lengths(ssages, para$sampling$len_classes,
               res$fleet$LL1$landings_n[,"2010",1,1,1], # [age, year, sex, season, area]
               para$om$growth[[para$om$sex[1]]])


plot(sl)
points(ln, col = "red")







# sample_lengths function:
ages1 = ssages
lenbins = para$sampling$len_classes
agecomp = res$fleet$LL1$landings_n[,"2010",1,1,1]
growth = para$om$growth[[para$om$sex[1]]]





#' Size sample
#'
#' Sample lengths
#' @param ages1 Number at age of the sample
#' @param lenbins  Length bins by year, season and region
#' @param agecomp Number at age of the sample
#' @param growth growth parameters
#' @export
sample_lengths <- function (ages1, lenbins, agecomp, growth) {
  # Mean length
  alk <- calc_VBlen(ages1, agecomp, growth)
  # Calculate normal distribution of length by length classes & sum up
  res <- vector(mode="numeric", length=length(lenbins))
  for (aa in 1:length(agecomp)) {
    # SD = Mean * CV, cumulative distribution
    res1 <- pnorm(lenbins, mean=alk[aa], sd=alk[aa]*growth[4])
    res11 <- res1[1]
    res1[1:(length(res1)-1)] <- (res1[2:length(res1)] - res1[1:(length(res1)-1)])
    res1[1] <- res1[1] + res11
    res1[length(res1)] <- 0
    res <- res + res1 * agecomp[aa]
  }
  #return(round(res,0))
  return(res)
}
sample_lengths(ages1, lenbins, agecomp, growth)



# play with it
# sample_lengths <- function (ages1, lenbins, agecomp, growth) {
  # Mean length
  alk <- calc_VBlen(ages1, agecomp, growth) # length at age
  # Calculate normal distribution of length by length classes & sum up
  res <- vector(mode="numeric", length=length(lenbins))
  for (aa in 1:length(agecomp)) {
    # SD = Mean * CV, cumulative distribution
    res1 <- pnorm(lenbins, mean=alk[aa], sd=alk[aa]*growth[4]) # define res1 at each lengthbin, the CDF using the length at that age as a mean and the mean*CV as the SD

    plot(lenbins, res1, type = "l")
    abline(v = alk[aa], lty = 2)
    abline(h = 0.5, lty = 2)


    # res1 <- pnorm(lenbins, mean=alk[aa], sd=1)

    res11 <- res1[1] # define res11 as the first number in res.
    res1[1:(length(res1)-1)] <- (res1[2:length(res1)] - res1[1:(length(res1)-1)]) # replace first set of res1 with last set of res1 minums first set
    plot(res1, ylim = c(0,1))
    res1[1] <- res1[1] + res11
    plot(res1, ylim = c(0,1))
    res1[length(res1)] <- 0
    res <- res + res1 * agecomp[aa] # add each res to the previous iteration of res
  }
  res
  #return(round(res,0))
  # return(res)
# }
sample_lengths(ages1, lenbins, agecomp, growth)





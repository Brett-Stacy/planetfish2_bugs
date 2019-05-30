#### 8. Catch-at-length
list_catchL <- NULL
for (ff in 1:length(params$list_fishery)) {
  fishery <- paste(params$list_fishery[ff],"_catchL",sep="")
  fisheryN <- paste("catch_at[",fishery,"]",sep="")
  fish <- params$Fish[ff,]
  if (sum(params$catch[[params$list_fishery[ff]]]) > 0 & sum(params$sample_years["catchlen",1:ycurr,ff]) > 0) {		# Catch > 0 and Sampling = 1
    # Data (sum over sex, N is total numbers)
    yy1 <- params$sample_years["catchage",1:ycurr,fish["Fishery"]]
    obsyy <- dimnames(params$sample_years[,yy1 > 0,])$year
    catchbyyear <- apply(params$catch[[params$list_fishery[ff]]],2,sum)
    catchyy <- names(catchbyyear[catchbyyear > 0])
    obsyy <- obsyy[obsyy %in% catchyy]
    dat <- params$catch_len[[fish["Fishery"]]][,obsyy,,,]
    dat[is.na(dat)] <- 0
    trun <- which(dimnames(dat)$len==max(as.integer(dimnames(dat)$len[apply(dat,c(1),sum)>0]))) # Range with obs > 0 (summed over all years)
    dat <- dat[1:trun,,]
    datN <- apply(dat,2,sum)
    if (params$by_sex == 0) dat	<- apply(dat,c(1,2),sum)				# Single-sex
    dat <- sweep(dat,2,datN,"/")	# Proportions
    dat[dat==Inf] <- 0						# Where sample size = 0, replace Inf with 0
    # dat[dat==0] <- 0.01						# Robustify against lognormal LL
    dat <- round(dat,4)
    # Data for estimation.csl file (no area, step, q or selectivity (defined through fishery)
    casalest[[fisheryN]]$command <- "catch_at"
    casalest[[fisheryN]]$value <- fishery						# fishery_catchL
    casalest[[fisheryN]]$fishery <- params$list_fishery[ff]		# fishery
    casalest[[fisheryN]]$at_size <- "True"
    casalest[[fisheryN]]$sexed <- if(params$by_sex == 0) "False" else "True"
    casalest[[fisheryN]]$sum_to_one <- "False"
    casalest[[fisheryN]]$plus_group <- params$len_plus_group
    casalest[[fisheryN]]$years <- c(dimnames(dat)$year)       	# List of years with observations
    casalest[[fisheryN]]$class_mins <- if(params$len_plus_group == "False") params$class_mins[1:(trun+1)] else params$class_mins[1:trun]
    if(params$by_sex == 0) {
      for (yy in 1:length(obsyy))
        casalest[[fisheryN]][obsyy[yy]] <- paste(dat[,dimnames(dat)$year==obsyy[yy]],collapse=" ")
    } else {			# With sex partition: CASAL requires first males then females (in the same line)
      for (yy in 1:length(obsyy))
        casalest[[fisheryN]][obsyy[yy]] <-
          paste(paste(dat[,dimnames(dat)$year==obsyy[yy],dimnames(dat)$sex=="m"],collapse=" "),
                paste(dat[,dimnames(dat)$year==obsyy[yy],dimnames(dat)$sex=="f"],collapse=" "))
    }
    for (yy in 1:length(obsyy)) {
      if(params$pin_diff_N_in_assessment == 1) {		# Adjust sample sizes if required
        casalest[[fisheryN]][paste("N_",obsyy[yy],sep="")] <- params$Fish[ff,"catchlen_N"]# params$catchage_N[[ff]]
      } else {										# Otherwise take sample sizes from original samples in OM
        casalest[[fisheryN]][paste("N_",obsyy[yy],sep="")] <- datN[yy]
      }
    }
    casalest[[fisheryN]]$dist <- params$catch_at_dist
    casalest[[fisheryN]]$r <- params$catch_at_r
    # List used below in @estimate
    list_catchL <- c(list_catchL,fishery)
  }
}
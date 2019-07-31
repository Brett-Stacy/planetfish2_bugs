	
	#### Checking out Brett's population dynamics
	
	
	##############################################
	



	RootDir  <- "C:\\Users\\philip_zie\\AAA_Phil\\A_Work\\2_Assessments\\Simulations_Brett\\2019-07-26_Baseline\\"
	## Working directory
	setwd(paste(RootDir,"",sep=""))
	
	library(casal)
	library(lattice)
	source("1 Functions Diagnostics.r")

	casalprefix		<- "casal_"
	casaloutprefix	<- "casalout_"
	pop_csl			<- paste(casalprefix, "population.csl", sep="")	
	est_csl			<- paste(casalprefix, "estimation.csl", sep="")	
	output_log		<- paste(casaloutprefix, "output.log", sep="")
	mpd_dat			<- paste(casaloutprefix, "mpd.dat", sep="")
	
	
	## Subdirectories 
	# DataBase data
    dirSKJ   		<- paste(RootDir,"SKJ\\",sep="")
    dirTOA   		<- paste(RootDir,"TOA\\",sep="")
    dirTOP   		<- paste(RootDir,"TOP\\",sep="")
	testdir			<- paste(RootDir,"SKJ_Test\\",sep="")

	################################################################
	#### Extract data from CASAL files
	skj <- list()
	dir1 <- dirSKJ
	skj[["est"]]	<- extract.csl.file(paste(dir1,est_csl,sep=""))	
	skj[["pop"]]	<- extract.csl.file(paste(dir1,pop_csl,sep=""))	
	skj[["quant"]]	<- extract.quantities(paste(dir1,output_log,sep=""))	
	skj[["mpd"]]	<- extract.mpd(paste(dir1,output_log,sep=""))	
	skj[["fits"]]	<- extract.fits(paste(dir1,output_log,sep=""))	

	toa <- list()
	dir1 <- dirTOA
	toa[["est"]]	<- extract.csl.file(paste(dir1,est_csl,sep=""))	
	toa[["pop"]]	<- extract.csl.file(paste(dir1,pop_csl,sep=""))	
	toa[["quant"]]	<- extract.quantities(paste(dir1,output_log,sep=""))	
	toa[["mpd"]]	<- extract.mpd(paste(dir1,output_log,sep=""))	
	toa[["fits"]]	<- extract.fits(paste(dir1,output_log,sep=""))	

	top <- list()
	dir1 <- dirTOP
	top[["est"]]	<- extract.csl.file(paste(dir1,est_csl,sep=""))	
	top[["pop"]]	<- extract.csl.file(paste(dir1,pop_csl,sep=""))	
	top[["quant"]]	<- extract.quantities(paste(dir1,output_log,sep=""))	
	top[["mpd"]]	<- extract.mpd(paste(dir1,output_log,sep=""))	
	top[["fits"]]	<- extract.fits(paste(dir1,output_log,sep=""))	



	ppop <- toa[["pop"]]
	pest <- toa[["est"]]
	pquant <- toa[["quant"]]
	pfits <- toa[["fits"]]

	ppop <- skj[["pop"]]
	pest <- skj[["est"]]
	pquant <- skj[["quant"]]
	pfits <- skj[["fits"]]
	
	ages 	<- seq(ppop$min_age$value, ppop$max_age$value)
	vb_para <- ppop$size_at_age
	vb		<- as.numeric(as.character(vb_para$Linf))*(1-exp(-as.numeric(as.character(vb_para$k))*(ages-as.numeric(as.character(vb_para$t0)))))
	plot(x=ages,y=vb, type="l",ylab="Length (mm)",xlab="Ages (y)",font.lab=2)
	lw_para	<- ppop$size_weight 
	lw		<- as.numeric(as.character(lw_para$a))*vb^(as.numeric(as.character(lw_para$b)))
	plot(x=vb,y=lw*1000, type="l",ylab="Weight (kg)",xlab="Length(mm)",font.lab=2)
	mat		<- as.numeric(as.character(ppop$maturity_props$all[2:length(ppop$maturity_props$all)]))
	plot(x=ages,y=mat, type="l",ylab="Proportion mature",xlab="Ages (y)",font.lab=2)
	sel_para	<- as.numeric(as.character(ppop$'selectivity[SelLL]'$all[2:length(ppop$'selectivity[SelLL]'$all)]))
	if (ppop$'selectivity[SelLL]'$all[1] %in% "double_normal")
		selLL 	<- 2^(-(((ages-sel_para[1])/ifelse(ages <= sel_para[1],sel_para[2],sel_para[3]))^2))
	# selLL	<- pquant$'Ogive parameter values'	# Not estimated
	plot(x=ages,y=selLL, type="l",ylab="Selectivity",xlab="Ages (y)",font.lab=2)
	lines(x=ages,y=mat, type="l",col="blue")


	# Fits to ageing data
	plot_fits_in_trellis(dat=pfits, dat_name="LL_catchA", xlab="Age (years)", ylab="Proportion")
	boxplot_fits_age(dat=pfits, dat_name=c("LL_catchA"),ylab="Mean age (year)")
	plot_residuals(dat=pfits, dat_name=c("LL_catchA"),ylab=c("Age"),plotdims=c(6,6))

	# Fits to tagging data
	plot_fits_tags(dat=pfits,plotdims=c(16,12))
	# savePlot(filename = paste("Plot Fits Tag ByLength.png",sep=""),type = "png")
	plot_fits_tagN(dat=pfits,plotdims=c(8,8))
	# savePlot(filename = paste("Plot Fits Tag Numbers.png",sep=""),type = "png")
	plot_fits_tags_Exp_Obs(dat=pfits,plotdims=c(9,6))
	# savePlot(filename = paste("Plot Fits Tag Exp-Obs.png",sep=""),type = "png")





	#### Run tests
	run_casal 	<- paste(testdir,"casal -e -O ", testdir,mpd_dat," -f ", testdir,casalprefix," > ", testdir,output_log, sep="") 	
	shell(run_casal, intern = TRUE)

	test_fits	<- extract.fits(paste(testdir,output_log,sep=""))		
	plot_fits_tags(dat=test_fits,plotdims=c(16,12))
	plot_fits_tagN(dat=test_fits,plotdims=c(8,8))
	plot_fits_tags_Exp_Obs(dat=test_fits,plotdims=c(9,6))




	###############################
	#### Check Assessment Fits
	mpd 		<- extract.mpd(file=output_log, path=casal_path)
	res_fits 	<- extract.fits(file=output_log, path=casal_path)  
	names(res_fits)

	# Survey Numbers-at-age and Numbers-at-length 
		par(windows(width=7,height=4.5))
		plot_fits_survey_biomass(dat=res_fits, dat_name=c("Surv1A","Surv1L"), xlab="Year", ylab="Biomass")
		savePlot(filename=paste(casal_path, runN,"_Plot Fits Survey Biomass",sep=""), type="png")
		plot_fits_survey(dat=res_fits, dat_name=c("Survey"), xlab="Year", ylab="Biomass")
		savePlot(filename=paste(casal_path, runN,"_Plot Fits Survey Biomass",sep=""), type="png")
		par(windows(width=8,height=8))
		plot_fits_in_trellis(dat=res_fits, dat_name="Surv1A", xlab="Age (years)", ylab="Numbers")
		savePlot(filename=paste(casal_path,	runN,"_Plot Fits Surv1A",sep=""), type="png")


	# Catch-at-Age
	par(windows(width=8,height=8))
	plot_fits_in_trellis(dat=res_fits, dat_name="Catch_Trawl1A", xlab="Age (years)", ylab="Proportion")
	savePlot(filename=paste(casal_path,	runN,"_Plot Fits Fishery Trawl1A",sep=""), type="png")

	## Predicted and fitted median age
	boxplot_fits_age(dat=res_fits, dat_name=c("Surv1A","Catch_Trawl1A","Catch_Trawl2A","Catch_LL1A","Catch_LL2A","Catch_TrapA"),ylab="Mean age (year)")
	savePlot(filename=paste(casal_path,	runN,"_Plot Fits Fishery All Median Age",sep=""), type="png")

	## Tag
	if(length(cpara$area) == 1) {
		plot_fits_tags(dat=res_fits,plotdims=c(16,12))
		savePlot(filename = paste(casal_path,runN,"_Plot Fits Tag ByLength",".png",sep=""),type = "png")
		plot_fits_tagN(dat=res_fits,plotdims=c(8,8))
		savePlot(filename = paste(casal_path,runN,"_Plot Fits Tag Numbers",".png",sep=""),type = "png")
		plot_fits_tags_Exp_Obs(dat=res_fits,plotdims=c(9,6))
		savePlot(filename = paste(casal_path,runN,"_Plot Fits Tag Exp-Obs",".png",sep=""),type = "png")
		plot_fits_tags_MedianL(dat=res_fits,plotdims=c(8,8))
		savePlot(filename = paste(casal_path,runN,"_Plot Fits Tag MedianLength",".png",sep=""),type = "png")
	}
	if(length(cpara$area) > 1) {
		plot_fits_tags_area(dat=res_fits,plotdims=c(16,12),casal_path,runN)
		plot_fits_tagN_area(dat=res_fits,plotdims=c(8,8),casal_path,runN)
		plot_fits_tags_Exp_Obs_area(dat=res_fits,plotdims=c(9,6),casal_path,runN)
		plot_fits_tags_MedianL_area(dat=res_fits,plotdims=c(8,8),casal_path,runN)
	}
	
	
	# Pearson's Residuals (Residuals scaled by sqrt of variance)
	plot_residuals(dat=res_fits, dat_name=c("Surv1A"),ylab=c("Age"),plotdims=c(6,6))
	savePlot(filename = paste(casal_path,runN,"_Plot Residuals1.png",sep=""),type = "png")

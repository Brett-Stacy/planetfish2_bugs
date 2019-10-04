README for TOA bug scripts


TOA_bug_1.R - TOA LHPs before earthfish development
TOA_bug_2.R - TOA LHPs after earthfish development. use Ch3 scripts instead
TOA_bug_3.R - SKJ LHPs. use Ch3 scripts instead
TOA_bug_4.R - TOP updated LHPs
TOA_bug_5.R - TOA LHPs. uses wind-up years before assessment.
TOA_bug_6.R - FAKE LHPs with straight line growth
TOA_bug_7.R - TOP original LHPs. part_mort = 0.5 - performs bad
TOA_bug_8.R - TOP original LHPs. New script flow. used for markdown diagnostics. part_mort = 1
TOA_bug_9.1.R - TOP same as TOA_bug_8 but with growth CV = 0.08
TOA_bug_9.2.R - TOP same as TOA_bug_8 but with growth CV = 0.05
TOA_bug_9.3.R - TOP same as TOA_bug_8 but with growth CV = 0.15
TOA_bug_9.4.R - TOP same as TOA_bug_8 but with growth CV = 0.0
TOA_bug_10.R - TOP same as TOA_bug_8 but with extended age classes
TOA_bug_11.R - TOA same as TOA_bug_2 but playing with CV again
TOA_bug_12.R - TOP from Chapter 3, check if tags getting into CASAL from IUU fleet
TOA_bug_13.R - TOP from Chapter 3 baseline. Play with CV again: try sample_lengths() function.
TOA_bug_14.R - TOP same as TOA_bug_8 but changed sum_to_one to "True" for @catch_at in est.csl.
TOA_bug_15.R - TOP same as TOA_bug_14.R but trying CPUE instead of tags as abundance measure.
TOA_bug_16.R - TOP same as TOA_bug_8 but trying to match up catchA stuff.
TOA_bug_17.R - TOP same as TOA_bug_8 but switching scanned and recaptured numbers in @tag_recapture in earthfish.
TOA_bug_18.R - TOP same as TOA_bug_8 but testing the capability of adding "age-size" and "age" to casal input files.











THINGS I KNOW:
TAGGING ABUNDANCE:
Increasing the tags from 2500 to 25000 increases the discrepency.
Decreasing the OM growth CV shifts the AM SSB up. Increasing it shifts the AM SSB down.
With original TOP LHPs and Pauls scripts, and 2500 tags, and spawning_part_mort = 1, the SSB match up nicely.
When recruitment variability is added to my base case from ch.3, the discrepency narrows. i.e. the AM SSB matches better. In fact, I can change increase it past 0.3 i.e. 0.5 and it matches even better.

CPUE ABUNDANCE:

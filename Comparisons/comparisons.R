## Compare stuff from my Planetfish2/CASAL models to Paul's
# 3/6/19



# res output from operating model. Compare TOA baseline res to TOP baseline res ----
res.TOA = readRDS("C:/Users/STA384/Documents/GitHub/planetfish2_bugs/Comparisons/res_TOA_31_5_19")
res.TOP = readRDS("C:/Users/STA384/Documents/GitHub/planetfish2_bugs/Comparisons/res_TOP_31_5_19")

diffobj::diffObj(res.TOA, res.TOP)



# res output from TOA OM compared to input of AM ----

datass.TOA = readRDS("C:/Users/STA384/Documents/GitHub/planetfish2_bugs/Comparisons/datass_TOA_6_6_19")


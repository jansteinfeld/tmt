#############################################################################
# create simple template
#############################################################################
formula = "start(start) += S1(B1,B2,B3) += S2(B4,B5,B6,B7)"
tmt_msttemplate(formula, full = TRUE, eval = TRUE)
tmt_msttemplate(formula, full = TRUE, eval = FALSE)

#############################################################################
# create complex template
#############################################################################

formula = "nativ(no,yes) ~ education(low,medium,heigh) ~ 
            CBM(3:6) += S1(B1,B2,B3) += S2(B4,B5,B6,B7)"
tmt_msttemplate(formula, full = TRUE, eval = TRUE)
tmt_msttemplate(formula, full = TRUE, eval = FALSE)

#############################################################################
# create template for the input as matrix
#############################################################################
tmt_msttemplate()
library(lavaan)

# Exploratory Factor Analysis
fa.parallel(data_enc[,3:ncol(data_enc)])

fa.parallel(data_enc[,10:ncol(data_enc)])

# Two-Factor seems to be fitting good as we can attribute one factor to Pro-Environmental Mindset and another to Self-Care(Personal Behaviour) Mindset

# Fit Factor
fit_efa <- factanal(data_enc[,3:ncol(data_enc)], 2, rotation="promax")
fit_efa

fa.diagram(fit_efa$loadings)

pdf("my_fa_diagram_nfacts_4.pdf", height=30, width=5)
fa.diagram(fit_efa$loadings, main="Factor Analysis")
dev.off()

fit_efa <- factanal(data_enc[,3:ncol(data_enc)], 4, rotation="promax")
fit_efa

fa.diagram(fit_efa$loadings)

# Model Fit
# The Factors I am aiming for are-
"f1 =~ SS1 + SS2 + SS3 + SS4 + SS5 + PC2 +  PC4 + PC5 + CON1 + CON4 + ER5" # Social Side of an Individual
"f2 =~ CON2 + CON5 + PC1 + PC3 + PC6 + ES1 + ES2 + ES3 + ES4 + ES5 + REC2 + REC4 + RES1 + RES3 + RES5" # Own Contribution or Believe about own self
"f3 =~ REC1 + REC3 + REC5 + REC6 + REC7 + SA1 + SA2 + SA3 + SA4 + SA5 + SA6 + ER1 + ER2 + ER3 + ER4 + RES2 + RES4 + CON3" # Pro-Environmental Believe

model <- "f1 =~ ER4 + REC5 + RES4 + ES2 + PC1 + REC6 + ES5 + SA5 + ER2 + SA2 + REC3 + RES2 + REC4
          f2 =~ RES5 + CON2 + CON5 + RES1 + ES4 + REC2 + PC3 + PC6 + ES3
          f3 =~ SA4 + PC4 + REC1 + SA6 + ER3 + SA3 + PC5 + ER5 + CON3 + REC7
          f4 =~ SS2 + SS3 + SS5 + SS4 + ER1 + ES1 + CON1 + SS1" 

model <- "f1 =~ REC5 + PC4 + SA5 + SA4 + SA6 + SS5 + ER5 + RES4 + REC6 + ER2 + PC5 + CON3 + ER4 + SA3 + REC4 + ER3 + REC3 + RES2 + ES1 + ES2 + REC7 + SS3 + CON1 + ER1 + SS2 + RES3
          f2 =~ RES5 + CON2 + RES1 + ES4 + CON5 + REC2 + SS4 + PC6 + PC3 + SA2 + ES3 + ES5 + REC1
          f1 ~~ 0*f2"

cfa_fit <- cfa(model, data_enc[,3:ncol(data_enc)], std.lv=TRUE)
summary(cfa_fit, fit.measures=TRUE, standardized=TRUE)

model <- "f1 =~ ER4 + RES4 + ES2 + PC1 + ES5 + SA5 + ER2 + SA2 + RES2 + CON4 + SA5
          f2 =~ RES5 + CON2 + CON5 + RES1 + ES4 + PC3 + PC6 + ES3
          f3 =~ SA4 + PC4 +SA6 + ER3 + SA3 + PC5 + ER5 + CON3
          f4 =~ SS2 + SS3 + SS5 + SS4 + ER1 + ES1 + CON1 + SS1 + PC3" 

model <- "SA =~ SA1 + SA2 + SA3 + SA4 + SA5 + SA6
          PC =~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6
          SS =~ SS1 + SS2 + SS3 + SS4 + SS5
          ER =~ ER1 + ER2 + ER3 + ER4 + ER5
          ES =~ ES1 + ES2 + ES3 + ES4 + ES5
          RES =~ RES1 + RES2 + RES3 + RES4 + RES5
          CON =~ CON1 + CON2 + CON3 + CON4 + CON5" 

model <- "SA =~ SA1 + SA2 + SA3 + SA4 + SA5 + SA6
          PC =~ PC1 + PC2 + PC3 + PC4 + PC5 + PC6"
cfa_fit <- cfa(model, data_enc[,10:ncol(data_enc)], std.lv=TRUE)
summary(cfa_fit, fit.measures=TRUE, standardized=TRUE)

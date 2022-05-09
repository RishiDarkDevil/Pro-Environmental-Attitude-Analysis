library(lavaan)

# Exploratory Factor Analysis
fa.parallel(data_enc[,3:ncol(data_enc)])

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
# (1) Pro-Environmental Believes[Bookish]
# (2) My Believes about Society or Things that should be done
# (3) My Contribution to Saving Environment[Which I actually do]
# (4) My believes about me 
model <- "f1 =~ REC1 + REC3 + REC5 + REC6 + REC7 + PC4 + SS3 + SS5 + ER1 + ER2 + ER3 + ER4 + ER5 + RES2 + RES4 + CON1 + CON3
          f2 =~ SA1 + SA2 + SA3 + SA4 + SA5 + SA6 + PC2 + PC5 + SS1 + 
          f3 =~ SS2 +
          f4 =~ PC1 + PC3" 

"f1 =~ SS1 + SS2 + SS3 + SS4 + SS5 + CON1 + "

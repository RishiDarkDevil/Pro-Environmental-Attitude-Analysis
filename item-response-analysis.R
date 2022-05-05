library(mirt)

# Multidimensional Item Response Theory

# Fitting mirt models
mirt_mods <- list()
for(i in seq_along(Qsn_Name)) {
  mirt_mods[[i]] <- mirt(data_enc %>% select(matches(Qsn_Name[i])), verbose = FALSE, itemtype = "graded", SE = TRUE)
}
  
mirt_mod_summ <- mirt_mods %>%
  map(~M2(., type = "C2", calcNULL = FALSE))

# RMSEA < 0.06 means item are adequate fit in the model
mirt_item_fit <- mirt_mods %>%
  map(~itemfit(.))

# IRT parameters
IRT_parms_items <- mirt_mods %>%
  map(~coef(., IRTpars = TRUE, simplify = TRUE)$items)
IRT_parms_items

# Category Characteristic Curves
cat_char_curves <- mirt_mods %>%
  map(~plot(., type='trace', which.item = c(1,2,3,4,5,6), facet_items=T, 
            as.table = TRUE, auto.key=list(points=F, lines=T, columns=4, space = 'top', cex = .8), 
            theta_lim = c(-3, 3), 
            main = ""))

plot(mirt_mods[[8]])

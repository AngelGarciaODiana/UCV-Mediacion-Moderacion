da <- med_1
names(da)

Model <- "abandono ~ b*afecto
          afecto ~ a*estres
          
          
          indi1 := a*b
          
          total := a+b+(a*b)
          proporcion := indi1/total"

library(lavaan)

med <- sem(model = Model, data = da, estimator = "MLR")
summary(med, fit.measures = T, standardized = T, rsquare = T)


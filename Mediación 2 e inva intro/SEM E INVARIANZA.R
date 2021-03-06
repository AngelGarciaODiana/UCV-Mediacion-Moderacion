library(psych)
library(dplyr)
library(stargazer)

#IMPORTAMOS LA DATA
da <- DATA

names(da)

#******************************************************************************
#                            DESCRIPTIVOS Y CFA
#******************************************************************************

#DESCRIBIMOS Y ANALIZAMOS UNA DE LAS VARIABLES
as.data.frame(psych::describe(da)) %>%
  round(3) %>%
  filter(range != 1) %>%
  select(-trimmed, -mad, -vars) %>%
  relocate(min, max, range, .before = mean) %>%
  relocate(median, .after = mean) %>%
  relocate(sd, .before = se)

#EJECUTAMOS ANÁLISIS FACTORIAL CONFIRMATORIO
library(tidyverse)
library(lavaan)
library(semPlot)
library(dplyr)

#ESPECIFICAMOS UN MODELO
My_model<-"F1=~BRS1+BRS2+BRS3+BRS4+BRS6
           BRS4~~BRS6"


#ESTABLECEMOS LOS VALORES DE AJUSTE
fit<- cfa(model = My_model, data = da, estimator="WLSMV", mimic= "Mplus", ordered = TRUE)

summary(fit, fit.measures = TRUE, standardized=T, r=TRUE)


#GRAFICAMOS
semPlot::semPaths(fit,what="std", residuals=FALSE, 
                  nCharNodes = 0, edge.label.cex =0.8, 
                  legend=FALSE, style = "lisrel", 
                  rotation = 1, 
                  sizeLat=5, sizeMan = 5, 
                  layout = "tree", whatLabels = "std")








#******************************************************************************
#                           SEM CON LAS DEMAS VARIABLES
#******************************************************************************

library(lavaan)
library(tidyverse)
library(readr)
library(semPlot)
library(semTools)

Model.esp <- 'COV=~COVID3+COVID4+COVID11+COVID12+COVID13+COVID5a+COVID6+COVID7+COVID8+COVID10a+COVID14
                     COVID5a~~COVID10a
                     COVID11~~COVID12
                     COVID12~~COVID13
                     COVID6~~COVID10a
                     COVID5a~~COVID6
                     COVID11~~COVID13
                     COVID6~~COVID8
              
              AFR=~EEA14+EEA1+EEA21+EEA11+EEA17+EEA22+EEA3+EEA9+EEA15+EEA18+EEA6+EEA12+EEA5+EEA16+EEA10+EEA20+EEA19+
                   EEA13+EEA7
                   EEA16~~EEA20
                   EEA21~~EEA17
                   EEA19~~EEA13
              
              LOC=~IE16+IE14+IE17+IE15+IE8+IE12+IE13+IE3+IE11+IE24+IE6+IE22+IE2+IE10+IE20+IE23+IE21+IE19+IE1+IE9+
                   IE5+IE18+IE7+IE4
                   IE13~~IE11
                   IE1~~IE9
                   IE3~~IE11
                   IE24~~IE10
                   IE9~~IE7
                   IE3~~IE24
                   IE6~~IE2
                   IE5~~IE18
                   IE14~~IE3
              
              REA=~RP3+RP9+RP2+RP1+RP14+RP13+RP5+RP6+RP4+RP8+RP7+RP12+RP11+RP10
                   RP6~~RP4

              AFR ~ b*REA+f*LOC
              LOC ~ e*COV
              REA ~ a*COV
              
              AFR ~ c*COV
              LOC ~ d*REA

              CO.RE:=a*b
              CO.LC:=e*f
              CO.RE.LC.AF:=a*d*f
              
              total:=a+b+c+d+e+f+(a*b)+(e*f)+(a*d*f)
              Proporcion:=(CO.RE+CO.LC+CO.RE.LC.AF)/total'


#WLSMV
Model.est<- sem(Model.esp, data = da, estimator = "WLSMV")
parameterestimates(Model.est)
summary(Model.est, fit.measures = TRUE, standardized = TRUE, modindices = TRUE, rsq= TRUE)

#BOOTSTRAP
set.seed(1234)
boot.sem <- sem(Model.est, data=da, se="bootstrap", bootstrap=100)
boot <- tibble(parameterestimates(boot.sem, ci=TRUE, level = 0.95, boot.ci.type = "perc") %>%
  filter(op == '~' | op == ':='))

setwd("C:/Users/Angel Garcia ODiana/Desktop")
write_excel_csv(boot, file = "param.csv", append = FALSE)

#ML Y BOOTSTRAP
set.seed(1234)
Model.est<- sem(Model.esp, data = da, se = 'bootstrap', bootstrap = 5000)
parameterestimates(Model.est)
summary(Model.est, fit.measures = TRUE, standardized = TRUE, modindices = TRUE, rsq= TRUE)

#GRAFRICO 1
semPlot::semPaths(Model.est, "std", layout="tree2", rotation = 2, 
                  residuals=FALSE, nCharNodes = 0, edge.label.cex =1, 
                  legend=FALSE, style = "lisrel", sizeLat=5, sizeMan = 5)

#GRAFICO 2
semPaths(Model.est, "model", "std", layout = 'spring',
         label.cex=1, edge.label.cex=0.5, fade=FALSE)










#******************************************************************************
#                           ANÁLISIS DE INVARIANZA
#******************************************************************************

#SELECCIONAMOS UNA VARIABLE DE LA DATA
da <- select(DATA, COUNT, BRS1, BRS2, BRS3, BRS4, BRS5, BRS6)

names(da)

#DECRIBIMOS LA DATA DIVIDIDA
psych::describeBy(da, group = da$COUNT)

#INV. CONFIGURAL
inva.config <- cfa(My_model, data = da, estimator = "WLSMV", group = "COUNT")
summary(cfa.config, fit.measures = TRUE, standardized = TRUE)

#INV. METRICA
inva.metric <- cfa(My_model, data = da, estimator = "WLSMV", group = "COUNT", 
                  group.equal = "loadings")
summary(cfa.metric, fit.measures = TRUE, standardized = TRUE)

#COMPARAMOS INVARIANZAS
coms <- compareFit(inva.config, inva.metric)

#EXPORTAMOS LOS RESULTADOS
stargazer(coms@fit, summary = FALSE, type = "html", out = "fit.html")
stargazer(coms@nested, summary = FALSE, type = "html", out = "nested.html")
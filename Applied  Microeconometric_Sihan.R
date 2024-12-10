install.packages("readxl")
library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(plm)
library(lmtest)
library(sandwich)
library(naniar)
library(mice)
library(gplots)
library(ggplot2)

#Data organization_original dataset

Dataset <- read_excel("D:/9 MASTER 2/Applied Microeconometrics_R/Dataset.xlsx")
View(Dataset)
Dataset<-Dataset[, c(1, 2, 3, 7, 36, 37, 40, 41)]
data4 <- Dataset[Dataset$year %in% c(2017, 2020), ]
orig_dataset<-data4[,c(2, 1, 3, 4, 5, 6, 7, 8)]

#Data organization_number of cancer death
#Data source: https://www.insee.fr/fr/statistiques/4505239#dictionnaire
#https://www.insee.fr/fr/statistiques/fichier/4505239/ODD_CSV.zip

ODD_DEP <- read.csv("D:/9 MASTER 2/Applied Microeconometrics_R/Enrichment of data/ODD_DEP.csv", 
                    sep = ";", header = TRUE)
data<-ODD_DEP
data <- data[, -c(11:67)]
rdata <- data[data$variable == "nb_deces_cancer", ]
rdata <- rdata[, c(1, 10, 11, 14)]
print(rdata)
#reverse some columns and rows and rename them
long_data <- rdata %>% pivot_longer(
  cols = c(A2017, A2020),  
  names_to = "year", 
  values_to = "number"     
)
data0 <- long_data %>% 
  pivot_wider(
    names_from = sous_champ,  
    values_from = number    
  )
nbmor_data <- data0[, c(2, 1, 3, 4)]
nbmor_data <- nbmor_data %>%
  arrange(desc(year == "A2017"))
nbmor_data$year <- gsub("^A", "", nbmor_data$year)
nbmor_data$nb_death_cancer<-nbmor_data$homme+nbmor_data$femme



#Data organization_poverty rate l30
#Data source:
#https://www.observatoire-des-territoires.gouv.fr/taux-de-pauvrete-moins-de-30-ans
#https://www.observatoire-des-territoires.gouv.fr/outils/cartographie-interactive/api/v1/functions/GC_API_download.php?type=stat&nivgeo=dep&dataset=filosofi&indic=taux_pauv_m30

X2020_poverty_rate_l30_filosofi <- read_excel("D:/9 MASTER 2/Applied Microeconometrics_R/Enrichment of data/2020_poverty rate l30_filosofi.xlsx",
                                              sheet = "Data")
View(X2020_poverty_rate_l30_filosofi)
data1<-X2020_poverty_rate_l30_filosofi
povl30_data<-data1[-c(1:3),]
colnames(povl30_data)<-povl30_data[1,]
povl30_data<- povl30_data[-1,]
colnames(povl30_data)[3]<-"year"
colnames(povl30_data)[4]<-"poverty_rate_l30"
povl30_data<- povl30_data[,c(3, 1, 2, 4)]



#Data organization_density of doctor_2017
#Data source: https://data.ameli.fr/explore/embed/dataset/demographie-effectifs-et-les-densites/table/?utm_source=chatgpt.com&disjunctive.profession_sante&disjunctive.region&disjunctive.departement&disjunctive.libelle_classe_age&disjunctive.libelle_sexe&sort=departement&refine.profession_sante=Radiothérapeutes&refine.region=76&refine.region=75&refine.region=84&refine.region=44&refine.region=11&refine.region=27&refine.region=24&refine.region=93&refine.region=28&refine.region=32&refine.region=52&refine.region=53&refine.region=01&refine.region=02&refine.region=99&refine.region=06&refine.region=04&refine.region=03&refine.region=94&refine.libelle_classe_age=Tout âge&refine.libelle_sexe=tout sexe&refine.annee=2017

X2017_docto_demographie_effectifs_et_les_densites <- read.csv("D:/9 MASTER 2/Applied Microeconometrics_R/Enrichment of data/2017_docto_demographie-effectifs-et-les-densites.csv",
                                                              sep=";", header=TRUE)
View(X2017_docto_demographie_effectifs_et_les_densites)
data2<-X2017_docto_demographie_effectifs_et_les_densites[,c(1, 2, 5, 6, 11)]
#change the order of comlum department
data2 <- data2 %>%
  mutate(
    numeric_part = as.numeric(gsub("[^0-9]", "", departement)),
    is_alpha = grepl("[A-Za-z]", departement),
    # Alphanumerics after "29" are assigned a value slightly greater than 29 but less than 30
    sort_key = case_when(
      is_alpha & numeric_part == 2 ~ 29.5, # Place 2A and 2B after 29
      TRUE ~ numeric_part           # Use numeric part for others
    )
  ) %>%
  arrange(sort_key) %>%
  select(-numeric_part, -is_alpha, -sort_key)

colnames(data2)[1]<-"year"
colnames(data2)[2]<-"type_doctor"
colnames(data2)[3]<-"codgeo"
colnames(data2)[4]<-"libgeo"
colnames(data2)[5]<-"dens_doc"
densdoc2017_data<-data2[,c(1, 3, 4, 2, 5)]


#Data organization_density of doctor_2020
#Data source: https://data.ameli.fr/explore/embed/dataset/demographie-effectifs-et-les-densites/table/?utm_source=chatgpt.com&disjunctive.profession_sante&disjunctive.region&disjunctive.departement&disjunctive.libelle_classe_age&disjunctive.libelle_sexe&refine.annee=2020&refine.profession_sante=Radiothérapeutes&refine.region=76&refine.region=75&refine.region=84&refine.region=44&refine.region=11&refine.region=27&refine.region=24&refine.region=93&refine.region=28&refine.region=32&refine.region=52&refine.region=53&refine.region=94&refine.region=01&refine.region=02&refine.region=03&refine.region=04&refine.region=06&refine.region=99&refine.libelle_classe_age=Tout âge&refine.libelle_sexe=tout sexe&sort=annee
X2020_doctor_demographie_effectifs_et_les_densites <- read.csv("D:/9 MASTER 2/Applied Microeconometrics_R/Enrichment of data/2020_doctor_demographie-effectifs-et-les-densites.csv",
                                                               sep=";", header=TRUE)
View(X2020_doctor_demographie_effectifs_et_les_densites)
data3<-X2020_doctor_demographie_effectifs_et_les_densites[,c(1, 2, 5, 6, 11)]
#change the order of comlum department
data3 <- data3 %>%
  mutate(
    numeric_part = as.numeric(gsub("[^0-9]", "", departement)),
    is_alpha = grepl("[A-Za-z]", departement),
    # Alphanumerics after "29" are assigned a value slightly greater than 29 but less than 30
    sort_key = case_when(
      is_alpha & numeric_part == 2 ~ 29.5, # Place 2A and 2B after 29
      TRUE ~ numeric_part           # Use numeric part for others
    )
  ) %>%
  arrange(sort_key) %>%
  select(-numeric_part, -is_alpha, -sort_key)

colnames(data3)[1]<-"year"
colnames(data3)[2]<-"type_doctor"
colnames(data3)[3]<-"codgeo"
colnames(data3)[4]<-"libgeo"
colnames(data3)[5]<-"dens_doc"
densdoc2020_data<-data3[,c(1, 3, 4, 2, 5)]




#Data organization_population in 2020
#Data source link：https://www.insee.fr/fr/statistiques/6683035?sommaire=6683037
donnees_departements <- read.csv("D:/9 MASTER 2/Applied Microeconometrics_R/Enrichment of data/donnees_departements.csv",
                                 sep = ";", header= TRUE)
View(donnees_departements)
data5<- donnees_departements[,c(3, 4, 9)]
colnames(data5)[1]<-"codgeo"
colnames(data5)[2]<- "libgeo"
colnames(data5)[3]<-"pop"
#add year column
pop2020_data <- cbind(year = 2020, data5)

#Merge all datasets
# Convert year to character in each dataset
orig_dataset <- orig_dataset %>% mutate(year = as.character(year))
pop2020_data <- pop2020_data %>% mutate(year = as.character(year))
densdoc2017_data <- densdoc2017_data %>% mutate(year = as.character(year))
densdoc2020_data <- densdoc2020_data %>% mutate(year = as.character(year))
povl30_data <- povl30_data %>% mutate(year = as.character(year))
nbmor_data <- nbmor_data %>% mutate(year = as.character(year))

#Merge densdoc_2017 and densdoc_2020
densdoc_data <- rbind(densdoc2017_data, densdoc2020_data)
#Merge pop2020_data and povl30_data
add_data<- full_join(pop2020_data, povl30_data, by = c("codgeo", "libgeo", "year"))
#Merge densdoc_data and orig_dataset
orig_dataset2<- full_join(densdoc_data, orig_dataset, by = c("codgeo", "libgeo", "year"))
#Merge orig_dataset2 and nbmor_data
orig_dataset1<- full_join(orig_dataset2, nbmor_data, by = c("codgeo", "year"))
#Merge orig_dataset1 and add_data
orig_dataset0 <- orig_dataset1 %>%
  left_join(add_data, by = c("year", "codgeo", "libgeo")) %>%
  mutate(
    pop = coalesce(as.numeric(pop.x), as.numeric(pop.y)),  # Convert to numeric
    poverty_rate_l30 = coalesce(as.numeric(poverty_rate_l30.x), as.numeric(poverty_rate_l30.y))  # Convert to numeric
  ) %>%
  select(everything(),  # keep all the columns
         -pop.x, -poverty_rate_l30.x,  # delete addtional columns
         -pop.y, -poverty_rate_l30.y   
  )
#dataset
data_R<- orig_dataset0 %>%
  filter(!(libgeo %in% c("Tout département", "FRANCE")))
print(data_R)
summary(data_R)

# Miising data
vis_miss(data_R)# For visualizing missing data
colSums(is.na(data_R)) #MAR  #Average PM 2.5: 44 #Average PM 10:12 #POVERTY: 6 # POP: 1
data_R<- data_R[,-7]#delete Average PM 2.5
data_R$'Average PM10'[is.na(data_R$`Average PM10`)] <- mean(data_R$`Average PM10`, na.rm = TRUE)# Mean imputation for missing data of Average PM 10
data_R<- data_R[-c(101,202),]#delete the row with missing data of pop
data_R$'poverty_rate_l30'[is.na(data_R$`poverty_rate_l30`)] <- mean(data_R$`poverty_rate_l30`, na.rm = TRUE)# Mean imputation for missing data of poverty_rate_l30
summary(data_R)
is.pbalanced(data_R)
#Add column cancer rate
data_R$cancer_rate <- data_R$Cancers / data_R$pop
#Add column cancer death rate
data_R$death_cancer_rate <- data_R$nb_death_cancer / data_R$Cancers

#Final dataset
rdata<- data_R[,-c(6, 8, 9, 10, 11)]
rdata <- rdata[, c(2, 1, 3, 4:ncol(rdata))]
rdata <- rdata[order(rdata[[1]]), ]
summary(rdata)
apply(rdata, 2, sd)



#Research question: How do healthcare access, air pollution, and socioeconomic factors affect changes in cancer mortality rates over time across departments in France?

#Dimensions of the data
dim(rdata) # 200 9
# Convert to panel data
pdata <- pdata.frame(rdata, index = c("codgeo", "year"))
#Panel data is balanced?
is.pbalanced(pdata) # True

pdata$Average.PM10 <- as.numeric(as.character(pdata$Average.PM10))
pdata$death_cancer_rate <- as.numeric(as.character(pdata$death_cancer_rate))

# Graph 1: Cancer Mortality Rate vs Average PM10
p1 <- ggplot(pdata, aes(x = Average.PM10, y = death_cancer_rate, color = factor(year))) +
  geom_point(alpha = 0.6) +  
  geom_smooth(se = FALSE, method = "loess") +  
  labs(
    title = "Cancer Mortality Rate vs Average PM10",
    x = "Average PM10 (µg/m³)",
    y = "Cancer Mortality Rate",
    color = "Year"
  ) +
  theme_minimal()

print(p1)

# Graph 2: Cancer Mortality Rate vs Poverty Rate
p2 <- ggplot(pdata, aes(x = poverty_rate_l30, y = death_cancer_rate, color = factor(year))) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(se = FALSE, method = "loess") +  # Smoothed trend line
  labs(
    title = "Cancer Mortality Rate vs Poverty Rate",
    x = "Poverty Rate (<30 Years Old) (%)",
    y = "Cancer Mortality Rate",
    color = "Year"
  ) +
  theme_minimal()
print(p2)
# Graph 3: Cancer Mortality Rate vs Density of Radiotherapists
p3 <- ggplot(pdata, aes(x = dens_doc, y = death_cancer_rate, color = factor(year))) +
  geom_point(alpha = 0.6) +  # Scatter points
  geom_smooth(se = FALSE, method = "loess") +  # Smoothed trend line
  labs(
    title = "Cancer Mortality Rate vs Density of Radiotherapists",
    x = "Density of Radiotherapists (per 10,000 inhabitants)",
    y = "Cancer Mortality Rate",
    color = "Year"
  ) +
  theme_minimal()
print(p3)

#OLS

# Evolution of death_cancer_rate by year and codgeo
coplot(death_cancer_rate ~ year | codgeo, type = "b", data = pdata)
# Linear regression for death_cancer_rate
model_ols_death <- lm(death_cancer_rate ~ `Average.PM10` + poverty_rate_l30 + dens_doc, data = pdata)
summary(model_ols_death)
# Heterogeneity in death_cancer_rate across regions
plotmeans(death_cancer_rate ~ codgeo, main = "Heterogeneity Across Departments", data = pdata)
# Create the plot without x-axis numbers
plotmeans(
  death_cancer_rate ~ codgeo,  # Replace with your column names
  main = "Heterogeneity Across Departments",
  data = pdata,
  xlab = "Department",
  ylab = "Cancer Mortality Rate",
  xaxt = "n",          # Suppress x-axis labels and ticks
  n.label = FALSE      # Remove `n=` labels
)

# Heterogeneity in death_cancer_rate across years
plotmeans(death_cancer_rate ~ year, main = "Heterogeneity Across Years", data = pdata)


# Fixed effect model

#fe_model on death cancer rate
fe_model_death <- plm(death_cancer_rate ~ `Average.PM10` + poverty_rate_l30 + dens_doc, data = pdata, model = "within")
summary(fe_model_death)
#The fixed effects/constants for each department are obtained as follows :
fixef(fe_model_death)
#Compare fixed effect model and OLS
pFtest(fe_model_death,model_ols_death) # The fixed effects model is more appropriate. Individual-specific effects are significant and must be accounted for.

# Breusch-Pagan test for heteroskedasticity in the fixed effects model
bptest(fe_model_death, studentize = FALSE) #No Evidence of Heteroskedasticity


# Fixed Effects Model with Time Fixed Effects
fe_model_time <- plm(death_cancer_rate ~ Average.PM10 + poverty_rate_l30 + dens_doc + factor(year),
                     data = pdata, index = c("codgeo", "year"), model = "within")
summary(fe_model_time)
# Compare the fixed effects model with and without time-fixed effects
pFtest(fe_model_time, fe_model_death)
# Extract coefficients for year effects
time_effects <- coef(fe_model_time)[grep("factor(year)", names(coef(fe_model_time)))]



#Random effect model
# Random Effects Model with Time Fixed Effects
re_model_time <- plm(death_cancer_rate ~ Average.PM10 + poverty_rate_l30 + dens_doc + factor(year),
                     data = pdata, model = "random", index = c("codgeo", "year"))

summary(re_model_time)

#Test for Heteroscedasticity
# Breusch-Pagan test for heteroskedasticity in the random effects model
bptest(death_cancer_rate ~ `Average.PM10` + poverty_rate_l30 + dens_doc + factor(codgeo), 
       data = pdata, studentize = FALSE) #Heteroskedasticity exists

# Compare fixed and random effect models_Hausman test
phtest(fe_model_time, re_model_time)# The fixed effects model is more appropriate. Individual-specific effects are correlated with the explanatory variables.




library(tidyverse)
library(MASS)
library(GGally)
library(fBasics)
library(caTools)
library(car)
library(lubridate)



rm(list=ls())

set.seed(3234)


raw_df <- read_csv("owid-covid-data.csv")

# El proceso de backward no funciona correctamente cuando hay NA's, por lo que tuvimos que hacer los NA's como 0, dejando la fecha en paz

raw_df_na_as_0 <- raw_df %>% 
  mutate_if(~ is.numeric(.), ~ ifelse(is.na(.), 0, .))

# agrupamos con nuestros países 

paises_a_analizar <- c("Belgium", "Greece", "Portugal", "Sweden")

df_by_countries <- raw_df_na_as_0 %>% 
  filter(location %in% paises_a_analizar)

# Filtramos variables que buscamos analizar (basados en in VIF inicial)

filtered_df <- df_by_countries %>% 
  dplyr::select(., -iso_code, -continent, -handwashing_facilities, -tests_units,
                -new_cases, -new_cases_smoothed, -new_cases_per_million, -new_deaths_per_million,
                -total_cases, -new_deaths, -new_deaths_smoothed, -new_tests_smoothed, -new_tests_per_thousand, 
                -icu_patients, -hosp_patients, -total_deaths, -weekly_hosp_admissions, -total_tests, -new_tests,
                -new_vaccinations, -total_vaccinations, -people_fully_vaccinated, -total_boosters, -new_vaccinations_smoothed,
                -new_people_vaccinated_smoothed, -people_vaccinated, -excess_mortality_cumulative_absolute, -excess_mortality,
                -total_vaccinations_per_hundred, -people_fully_vaccinated_per_hundred,  -total_cases_per_million, -excess_mortality_cumulative,
                -total_boosters_per_hundred)

# Dividimos 70 - 30 nuestra base para hacer el análisis 

train.base <- filtered_df %>% 
  sample_frac(.70)

val.base <- filtered_df %>% 
  setdiff(train.base)

# Buscamos VIF altos con la siguiente función 


full_model <- lm(new_deaths_smoothed_per_million ~ ., data = train.base)

summary(full_model)


backward_model <- stepAIC(full_model, direction = "backward", trace = FALSE)

summary(backward_model)

vif(backward_model)

# Ya tenemos todas las variables con un VIF menor a 10 y tofas con un nivel de significancia menor a 0.05, excepto por la división de las 
# variables de character, ya que se dejan en el modelo por una que sí sea significativa 

# Análisis de residuales ####

# 3.1 Comprobar si la regresión es lineal respecto los parámetros ####

# Utilizamos la R^2 para ver si es lineal respecto a los parámetros

suma_m <- summary(backward_model)
suma_m$adj.r.squared

# Es una R^2 de [1] 0.7180475, lo cual nos dice que nuestro modelo explica una varianza del 71% de el total de nuestra variable Y 
# Debido a lo complicado del tema a tratar, una R^2 de .71 la estamos considerando como una buena R^2

# 3.2 Comprobamos heterocedisticidad (la varianza de los errores es constante) ####

prueba_heter <- as.data.frame(cbind(backward_model$fitted.values, backward_model$residuals))

colnames(prueba_heter) <- c("Y_estimada", "Residuales")

grafico_heter <- prueba_heter %>% 
  ggplot() + 
  geom_point(aes(Y_estimada, Residuales)) + 
  theme_classic()

plotly::ggplotly(grafico_heter)

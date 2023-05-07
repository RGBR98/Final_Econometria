library(tidyverse)
library(MASS)
library(GGally)
library(fBasics)
library(caTools)
library(car)
library(lubridate)
library(lmtest)




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

# 3.3 Independencia en los errores ####

# Al ser una serie de tiempo, sí hay que buscar la independencia de los errores 

durbinWatsonTest(backward_model)

# Como el p-value es mayor a 0.05, no rechazamos la hipótesisi nula, la cual es 

# H0 (null hypothesis): There is no correlation among the residuals.

# HA (alternative hypothesis): The residuals are autocorrelated.

# por lo que podemos asumir que nuestros residuales son independientes entre ellos , podemos continuar con nuestra comprobación de supuestos

# 3.4 Presencia de errores atípicos ####

# Esto se hace calculando la raíz de el cuadrado medio de la suma de cuadrados de los errores (MSE)
# el cual se obtiene de la tabala ANOVA de nuestros residuales el mean 

anova <- anova(backward_model)
sqrt_mse <- sqrt(anova[6,3])

# Ya con MSE^(1/2), podemos sacar la división de los residuales entre la raíz de MSE y compararlos con xi
# Esas variables las metemos en un DF 

df_ea_back_model <- as.data.frame(cbind(train.base$new_cases_smoothed_per_million, train.base$reproduction_rate, train.base$icu_patients_per_million,
                                        train.base$hosp_patients_per_million, train.base$weekly_icu_admissions_per_million, 
                                        train.base$weekly_hosp_admissions_per_million, train.base$new_tests_smoothed_per_thousand, 
                                        train.base$tests_per_case, train.base$people_vaccinated_per_hundred, 
                                        train.base$new_vaccinations_smoothed_per_million, train.base$new_people_vaccinated_smoothed_per_hundred,
                                        train.base$stringency_index, train.base$excess_mortality_cumulative_per_million, 
                                        backward_model$residuals/sqrt_mse))

colnames(df_ea_back_model) <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "X10","X11", "X12", "X13", "Errores")

grafico_ea <- df_ea_back_model %>% 
  ggplot() + 
  geom_hline(yintercept = -4, colour = "red") + 
  geom_hline(yintercept = 4, colour = "red")  +
  geom_point(aes(x = X1, y = Errores, color = "new_cases_smoothed_per_million")) + 
  geom_point(aes(x = X2, y = Errores, color = "reproduction_rate")) +
  geom_point(aes(x = X3, y = Errores, color = "icu_patients_per_million")) +
  geom_point(aes(x = X4, y = Errores, color = "hosp_patients_per_million")) +
  geom_point(aes(x = X5, y = Errores, color = "weekly_icu_admissions_per_million")) + 
  geom_point(aes(x = X6, y = Errores, color = "weekly_hosp_admissions_per_million")) + 
  geom_point(aes(x = X7, y = Errores, color = "new_tests_smoothed_per_thousand")) +
  geom_point(aes(x = X8, y = Errores, color = "tests_per_case")) +
  geom_point(aes(x = X9, y = Errores, color = "people_vaccinated_per_hundred")) +
  geom_point(aes(x = X10, y = Errores, color = "new_vaccinations_smoothed_per_million")) + 
  geom_point(aes(x = X11, y = Errores, color = "new_people_vaccinated_smoothed_per_hundred")) + 
  geom_point(aes(x = X12, y = Errores, color = "stringency_index")) +
  geom_point(aes(x = X13, y = Errores, color = "excess_mortality_cumulative_per_million")) +
  ggtitle("X vs ui / raiz(mse)") +
  ylab("ui / raiz(mse)") +
  labs(title = "Datos atípicos") + 
  scale_color_manual(values = c("new_cases_smoothed_per_million" = "blue", "reproduction_rate" = "green", "icu_patients_per_million" = "black", 
                                "hosp_patients_per_million" = "red", "weekly_icu_admissions_per_million" = "purple",
                                "weekly_hosp_admissions_per_million" = "pink", "new_tests_smoothed_per_thousand" = "brown", "tests_per_case" = "yellow", 
                                "people_vaccinated_per_hundred" = "gray", "new_vaccinations_smoothed_per_million" = "orange", 
                                "new_people_vaccinated_smoothed_per_hundred" = "gold",
                                "stringency_index" = "lightblue", "excess_mortality_cumulative_per_million" = "lightgreen")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

grafico_ea

plotly::ggplotly(grafico_ea) 

# No hay datos atipicos, podemos continuar

# 3.5 Verificar la normalidad en los errores ####

# 3.5.1 La Q-Q plot

# Obtenemos residuales 

ui_back <- as.data.frame(backward_model$residuals)
colnames(ui_back) <- c("Ui")

ui_back %>% 
  ggplot(aes(sample = Ui)) + 
  stat_qq() +
  stat_qq_line() + 
  ggtitle("QQ Plot Residuales") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

#  Rechazamos el supuesto de normalidad de los errores debido a las dos colas que muestra el gráfico de QQ plot
# no obstante, ya que el modelo de regresión lineal simple ajustado es robusto ante el supuesto de normalidad
# podemos continuar usando estas variable






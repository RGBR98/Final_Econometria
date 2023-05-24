library(tidyverse)
library(MASS)
library(GGally)
library(fBasics)
library(caTools)
library(car)
library(lubridate)
library(lmtest)
library(zoo)

rm(list=ls())

set.seed(626)

raw_df <- read_csv("owid-covid-data.csv")

filtered_df <- raw_df %>% 
  dplyr::select(-iso_code, -handwashing_facilities)

# Olas COVID_19

olas <- filtered_df %>% 
  ggplot() + 
  geom_line(aes(x = date, y = new_deaths),  color = "black")

plotly::ggplotly(olas)

# Según el gráfico hubo 5 olas, las cuales empezaron en 2020-03-15, 2020-10-01, 2021-03-15, 2021-07-01 y 2022-01-01
# Dividimos el gráfico con las 5 olas 

ola_1 <- as.Date("2020-03-15")
ola_2 <- as.Date("2020-10-01") 
ola_3 <- as.Date("2021-07-01") 
ola_4 <- as.Date("2022-01-01") 
fin_olas <- as.Date("2022-04-21")

# Análisis Covid --------------------------

olas <- filtered_df %>% 
  ggplot() + 
  geom_line(aes(x = date, y = new_deaths_smoothed),  color = "black") + 
  geom_vline(xintercept = as.numeric(ola_1), color = "red", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_2), color = "yellow", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_3), color = "green", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_4), color = "gold", linetype = "dashed") + 
  theme_minimal()

plotly::ggplotly(olas)

# De igual forma vamos a agregar eventos importantes que llegaron a afectar al COVID
# El 2020-12-11 Salió la primera vacuna del COVID aprobada por el FDA manufacturada por phizer
# En marzo del 2020 iniciaron las medidas de contención, dada por la variable stringency_index

vacunas <- as.Date("2020-12-14")
inician_contingencias <- as.Date("2020-01-21")

olas <- filtered_df %>% 
  ggplot(aes(x = date, y = new_deaths_smoothed, color = continent, group = continent)) + 
  geom_path() + 
  geom_vline(xintercept = as.numeric(ola_1), color = "red", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_2), color = "yellow", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_3), color = "green", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_4), color = "gold", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(vacunas), color = "green", linetype = "solid") + 
  geom_vline(xintercept = as.numeric(inician_contingencias), color = "orange", linetype = "solid") + 
  theme(panel.background = element_rect(fill = "lightblue")) +
  theme_minimal()

plotly::ggplotly(olas)

# Se puede observar que hay varias variables con NA's en continentes. Buscamos que países son los que están en esos datos
# para ver si nos afectan en nuestro análisis
# Una de las variables es World, claramente si afecta al modelo porque esas son acumulados del mundo
# Debido a esto podemos eliminar los contiennetes que tengan NA, lo hacemos sobre el mismo DF ya que es el que vamos a utilizar
# para nuestro modelo 

filtered_continent_df <- filtered_df[complete.cases(filtered_df$continent), ]

olas_filtrada <- filtered_continent_df %>% 
  ggplot(aes(x = date, y = new_deaths_smoothed, color = continent, group = continent)) + 
  geom_path() + 
  geom_vline(xintercept = as.numeric(ola_1), color = "red", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_2), color = "grey", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_3), color = "green", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_4), color = "gold", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(vacunas), color = "green", linetype = "solid") + 
  geom_vline(xintercept = as.numeric(inician_contingencias), color = "orange", linetype = "solid") +
  coord_cartesian(ylim = c(0, 5000)) + #pongo el 5000 para mejor visualización
  theme_minimal()

plotly::ggplotly(olas_filtrada)


# Ahora checamos el comportamiento de las muertes COVID en USA 

filtered_usa_df <- raw_df[raw_df$location == "United States",]

grafico_usa <- filtered_usa_df %>% 
  ggplot(aes(x = date, y = new_deaths_smoothed)) + 
  geom_path() + 
  geom_vline(xintercept = as.numeric(ola_1), color = "red", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_2), color = "grey", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_3), color = "green", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(ola_4), color = "gold", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(fin_olas), color = "pink", linetype = "dashed") + 
  geom_vline(xintercept = as.numeric(vacunas), color = "green", linetype = "solid") + 
  geom_vline(xintercept = as.numeric(inician_contingencias), color = "orange", linetype = "solid") +
  theme_minimal()

plotly::ggplotly(grafico_usa)


# -------------------------------------------------------

# Nos vamos a enfocar en la vacunación y cómo disminuyó la vacuna el efecto del COVID y sus muertes

# Eliminamos variables que no son necesarias, metiendolas en un vector

variables_no_significativas <- c("continent", "tests_units", "excess_mortality_cumulative_per_million", "excess_mortality",
                                 "excess_mortality_cumulative", "excess_mortality_cumulative_absolute", "population",
                                 "human_development_index", "life_expectancy", "hospital_beds_per_thousand", "male_smokers",
                                 "female_smokers", "diabetes_prevalence", "cardiovasc_death_rate", "extreme_poverty",
                                 "gdp_per_capita", "aged_70_older", "aged_65_older", "median_age", "new_people_vaccinated_smoothed_per_hundred",
                                 "new_vaccinations_smoothed_per_million", "people_fully_vaccinated_per_hundred", "total_vaccinations_per_hundred",
                                 "new_vaccinations_smoothed", "total_boosters_per_hundred", "population_density",
                                 "people_vaccinated_per_hundred", "tests_per_case", "new_tests_smoothed_per_thousand",
                                 "new_tests_per_thousand", "new_tests_smoothed", "total_tests_per_thousand",
                                 "weekly_hosp_admissions_per_million", "weekly_hosp_admissions", "weekly_icu_admissions_per_million",
                                 "weekly_icu_admissions", "hosp_patients_per_million", "icu_patients_per_million",
                                 "new_cases_smoothed", "total_cases_per_million", "new_cases_per_million",
                                 "new_cases_smoothed_per_million", "new_deaths_smoothed_per_million", "new_deaths_per_million",
                                 "new_deaths_smoothed", "total_deaths_per_million", "new_tests", "total_tests",
                                 "total_boosters", "new_people_vaccinated_smoothed")

train_lm_df <- filtered_df %>% 
  filter(location %in% "United States") %>% 
  dplyr::select(-variables_no_significativas) %>% 
  dplyr::filter(date >= vacunas & date < ola_3)

# Checamos NA´s

missing_values <- is.na(train_lm_df)

missing_counts <- colSums(missing_values)

missing_counts

# Hay datos NA en new vaccinations 

# Hcemos interpolación para arreglar ese dato 

train_lm_df$new_vaccinations <- na.approx(train_lm_df$new_vaccinations)

# Con la info filtrada, iniciamos con los gg pairs para ver cuales son las variables lineales respecto a total deaths  

first_set <- train_lm_df %>% 
  ggpairs(columns = c(3,4,5,7, 6)) + 
  ggtitle("First set")

first_set

second_set <- train_lm_df %>% 
  ggpairs(columns = c(8:11, 6)) + 
  ggtitle("Second set")

second_set

third_set <- train_lm_df %>% 
  ggpairs(columns = c(12:15, 6)) + 
  ggtitle("Third set")

third_set

# Como se ve en los ggpairs, las variables con relación lineal respecto a las nuevas muertes son:
# Con relación positiva: icu_patients, hosp_patients, new_cases
# relación negativa: total_vaccinations, people_vaccinated
# Las nuevas vacunas no tienen una relación establecida con el total de muertes

# Checamos relación entre las variables

# ICU tiene relación perfecta con hosp, dejamos icu fuera
# total vacc y people_vacc 

# Metemos esas variables en nuestro modelo lineal a ver como se comportan 

m0 <-  lm(new_deaths ~ +people_vaccinated  +positive_rate + reproduction_rate +new_vaccinations +hosp_patients
          -date, train_lm_df)

summary(m0)

vif(m0) 

# Hay un VIF alto en hosp_patients y new_cases y positive rate, el que tiene mayor relevncia para nosotros son los casos
# Positive rate y hosp cases con ICU cases son co lineales entre si, dejamos positive rate
# por lo que eliminamos hospitales 

m1 <- lm(new_deaths ~ +positive_rate + reproduction_rate +new_vaccinations
         -date, train_lm_df)

summary(m1)

anova(m1)

vif(m1)

# Todas las variables cumplen con significacncia, por lo que continuamos con el modelo


# Datos atípicos --------------------------
 
anova_m1 <- anova(m1)
sqrt_mse_m1 <- sqrt(anova_m1[4,3])

ea_df_m1 <- as.data.frame(cbind(train_lm_df$positive_rate,train_lm_df$reproduction_rate,
                                train_lm_df$new_vaccinations, m1$residuals/sqrt_mse_m1))

colnames(ea_df_m1) <- c("X1", "X2", "X3", "Errores")

grafico_ea_m1 <- ea_df_m1 %>% 
  ggplot() + 
  geom_hline(yintercept = -4, colour = "red") + 
  geom_hline(yintercept = 4, colour = "red")  +
  geom_point(aes(x = X1, y = Errores, color = "positive_rate")) + 
  geom_point(aes(x = X2, y = Errores, color = "reproduction_rate")) +
  geom_point(aes(x = X3, y = Errores, color = "new_vaccinations")) +
  ggtitle("X vs ui / raiz(mse)") +
  ylab("ui / raiz(mse)") +
  labs(title = "Datos atípicos") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(values = c("people_vaccinated" = "blue", "positive_rate" = "green", 
                                "reproduction_rate" = "black", "new_vaccinations" = "red"))

grafico_ea_m1

# no hay datos atípicos 

# Homodedasticidad ------------------------

prueba_heter <- as.data.frame(cbind(m1$fitted.values, m1$residuals))

colnames(prueba_heter) <- c("Y_estimada", "Residuales")

s_m1 <- sqrt(sum(m1$residuals^2)/(99-5))

graf_heter_1 <- ggplot(data = prueba_heter) +
  geom_point(aes(x = Y_estimada, y = Residuales)) +
  geom_hline(yintercept = 2*s_m1) + 
  geom_hline(yintercept = -2*s_m1)

# Se observa un ligero patrón en cono, por lo que vamos a aplicar una transformación Box-Cox

Y <- train_lm_df$new_deaths

Y_filtrada <- subset(Y, Y > 0)

resultado  <- boxcox(Y_filtrada ~ 1, , plotit = FALSE)

data_transformada_box_cox <- resultado$x

lambda <- resultado$x[which.max(resultado$y)] 

new_deaths_box_cox <- ifelse( Y == 0, 0, (Y + abs(min(Y_filtrada)) + 0.001 ) ^ lambda)

modelo_box_cox <- update(m1, formula = new_deaths_box_cox ~ .)

# Modifica new_death, por la nueva Y que fue transformada

train_lm_df <- train_lm_df %>% 
  mutate(new_deaths = new_deaths_box_cox)

# Checamos anova, summary y VIF

anova(modelo_box_cox)

summary(modelo_box_cox)

vif(modelo_box_cox)

# Checamos heterocedasticidad nuevamente

prueba_heter_bc <- as.data.frame(cbind(modelo_box_cox$fitted.values, modelo_box_cox$residuals))

colnames(prueba_heter_bc) <- c("Y_estimada", "Residuales")

grafico_heter_bc <- prueba_heter_bc %>% 
  ggplot() + 
  geom_point(aes(Y_estimada, Residuales)) + 
  theme_classic()

plotly::ggplotly(grafico_heter_bc)


# Se puede observar que ya no hay heterocedasticidad, por lo que podemos continuar 

# independencia de los errores ------------

# Al ser una serie de tiempo, sí hay que buscar la independencia de los errores 

dwtest(modelo_box_cox,alternative ="two.sided",iterations = 1000)

# H0: No hay autocorrelación
# H1: hay autocorrelación

# Como el P-value es menor a .05, rechazamos H0 lo cual nos da que sí hay autocorrelación 
# Se arregla con un lag

lag_muertes <-  lag(train_lm_df$new_deaths)

train_lm_df <- cbind(train_lm_df, lag_muertes)

train_lm_df <- train_lm_df %>% 
  filter(!is.na(lag_muertes))

# Le quitamos el primer dato a la Y por el lag

new_deaths_box_cox <- new_deaths_box_cox[-1]

modelo_box_cox_updated <- update(modelo_box_cox, . ~ . + lag_muertes, data = train_lm_df)
  
# Checamos durwin

dwtest(modelo_box_cox_updated, alternative ="two.sided",iterations = 1000)

# Estamos en el area inconclusa de Dw test, ya que estamos entre dl y du, por lo que por 
# el momento dejamos el test en este punto 

summary(modelo_box_cox_updated)
anova(modelo_box_cox_updated)

vif(modelo_box_cox_updated)

# Verificamos normalidad -----------------------

ui_back <- as.data.frame(modelo_box_cox_updated$residuals)
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

# ----------------------------- Se cumplen los supuestos, continuamos con la validación
# La base de validación se va a hacer con la ola_3 en adelante


val_lm_df <- filtered_df %>% 
  filter(location %in% "United States") %>% 
  dplyr::select(-variables_no_significativas) %>% 
  dplyr::filter(date >= ola_3 & date < fin_olas)


# aplicamos Box_cox

val_lm_df <- val_lm_df %>% 
  mutate(new_deaths = new_deaths^lambda)

# Aplicamos el lag

lag_muertes <-  lag(val_lm_df$new_deaths)

val_lm_df <- cbind(val_lm_df, lag_muertes)

val_lm_df <- val_lm_df %>% 
  filter(!is.na(lag_muertes)) 

# Intervalos 

confianza <- as.data.frame(predict.lm(object = modelo_box_cox_updated, newdata = val_lm_df, interval = 'confidence',level=.95))
colnames(confianza) <- c("pred","lwr","upr")

prediccion <- as.data.frame(predict.lm(object = modelo_box_cox_updated, newdata = val_lm_df, interval = 'predict',level=.95))
colnames(prediccion)<-c("pred1","lwr1","upr1")

val_lm_df <- as.data.frame(c(val_lm_df, confianza, prediccion))

p <- ggplot(val_lm_df, aes(date, new_deaths)) +
  geom_point(color= "black") 

#

grafico_pred <- p +
  stat_smooth(aes(y = lwr1), color = "red", linetype = "dashed", se = FALSE) +
  stat_smooth(aes(y = upr1), color = "red", linetype = "dashed", se = FALSE) +
  stat_smooth(aes(y = pred), color = "blue", se = FALSE) +
  stat_smooth(aes(y = lwr), color = "orange", linetype = "dashed", se = FALSE) +
  stat_smooth(aes(y = upr), color = "orange", linetype = "dashed", se = FALSE) +
  ggtitle("Gráfico de Predicción") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

plotly::ggplotly(grafico_pred)



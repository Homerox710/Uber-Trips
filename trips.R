# -------------------------------------------------------------------

  # Librerias a utilizar
library(tidyverse) # Para manipular los datos
library(ggplot2)   # Para graficar
library(dplyr)     # Para manipular los datos
library(plotly)    # Para grÃ¡ficos interactivos
library(lubridate) # Para manejar las fechas

                  #---- DepuraciÃ³n de los datos ----

  # Cargamos el archivo orignal y lo exploramos

trips_data <- data.table::fread(file = "trips_data.csv")

summary(trips_data)
#------------------------------------------------------------------------

  # Filtramos los datos para eliminar los viajes cancelados
  # Esto porque los viajes cancelados nos probocan sesgo 

trips_data <- trips_data %>%
  subset(`Trip or Order Status` == "COMPLETED")
#------------------------------------------------------------------------

  # Transformamos las variables que vamos a analizar
  # Las categóricas a factor, las numéricas a number y las fechas a Date

trips_data <- trips_data %>% 
  mutate(`Product Type` = as.factor(`Product Type`)) %>% 
  mutate(`Trip or Order Status` = as.factor(`Trip or Order Status`))%>% 
  #mutate(`Distance (miles)` = as.numeric(`Distance (miles)`)) %>%
  mutate(`Fare Amount` = as.numeric(`Fare Amount`)) %>%
  mutate(`Fare Currency` = as.factor(`Fare Currency`)) %>%
  mutate(`Request Time` = as.Date(`Request Time`))
#------------------------------------------------------------------------

  # Creamos las columnas del data_frame para el estudio 

ProductType <- trips_data$`Product Type`

OrderStatus <- trips_data$`Trip or Order Status`

  #Distance <- trips_data$`Distance (miles)`

FareAmount <- trips_data$`Fare Amount`

FareCurrency <- trips_data$`Fare Currency`

  # ---- Fechas, estas corresponden a la fecha en que pedimos un servicio ----

RequestTimeDate <- trips_data$`Request Time`

  # Extraemos el mes

MonthName <- months(RequestTimeDate)

  # Convertimos los meses a factor y ordenamos los niveles

MonthName <- sort(factor(MonthName, levels = month.name))

  # Extraemos los nombres de los días de la semana de las fechas

DayOfWeek <- weekdays(RequestTimeDate)

  # Extraemos la semana del año de la fecha

WeekOfYear <- paste(year(RequestTimeDate),week(RequestTimeDate),sep="-")

  # Creamos el data_frame limpio para el análisis

clean_data <- data.frame(ProductType = ProductType, OrderStatus = OrderStatus,
                         RequestTimeDate = RequestTimeDate, 
                         #Distance = Distance,
                         FareAmount = FareAmount, FareCurrency = FareCurrency,
                         MonthName = MonthName, DayOfWeek = DayOfWeek,
                         WeekOfYear = WeekOfYear)

#------------------------------------------------------------------------

#                       ---- EDA ----

#------------------------------------------------------------------------

  # Cantidad de viajes relacionada con el tipo de producto

ggplotly(
  ggplot(clean_data,aes(x =ProductType, fill = ProductType))+
    geom_bar(fill="lightblue",color="black")+  
    theme(panel.background = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    labs(x = "Product Type", y = "Trips"))
#------------------------------------------------------------------------

  # Porcentaje de viajes según el tipo de producto

ggplot(clean_data, aes(x = ProductType)) +  
  geom_bar(aes(y = (..count..)/sum(..count..)),
           fill="lightblue",color="black")+
  scale_y_continuous(labels=scales::percent)+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Product Type", y = "Percent")

#------------------------------------------------------------------------

  # Creamos esta variable para calcular los gastos mensuales

gasto_mensual <- clean_data %>% 
  group_by(MonthName) %>%
  dplyr::summarize( sum(FareAmount))

  # Resumen de nuesto gasto mensual

summary(gasto_mensual$`sum(FareAmount)`)

#------------------------------------------------------------------------


  # Creamos esta variable para calcular nuestros gastos semanales

gasto_semanal <- clean_data %>% 
  group_by(WeekOfYear) %>%
  dplyr::summarize( sum(FareAmount))

  # Resumen de nuesto gasto semanal

summary(gasto_semanal$`sum(FareAmount)`)


#------------------------------------------------------------------------

  # Gráfico línea de tendencia con respecto a los gastos mensuales

ggplotly(ggplot(gasto_mensual, aes(x=MonthName, y=`sum(FareAmount)`, group=1)) +
           geom_path()+
           geom_point()+
           theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())+
           labs(x = "Month", y = "Expense"))

#------------------------------------------------------------------------

  # Gráfico línea de tendencia con respecto a los gastos semanales

ggplotly(ggplot(gasto_semanal, aes(x=WeekOfYear, y=`sum(FareAmount)`, group=1)) +
           geom_path()+
           geom_point()+
           theme(panel.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())+
           labs(x = "Month", y = "Expense"))

#------------------------------------------------------------------------
 
 # Frecuencia de viajes según el día de la semana

ggplot(clean_data, aes(x = DayOfWeek, fill = DayOfWeek)) +
  geom_bar(width = 1, color="black")+ coord_polar()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Day of Week", y = "Frecuency")
#------------------------------------------------------------------------

  # Frecuencia de viajes según el mes

ggplot(clean_data, aes(x = MonthName, fill = MonthName)) +
  geom_bar(width = 1, color="black")+ coord_polar()+
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  labs(x = "Day of Week", y = "Frecuency")
# -------------------------------------------------------------------

        # ---- Analisis estadístico ----

#------------------------------------------------------------------------

  # Función para crear tablas de frecuencia

TableCreation <- function(y){
  data.frame(table(y),
             round(x = prop.table(x = table(y)) * 100, 1))
}

#------------------------------------------------------------------------

  # Revisa los datos y crea 4 categorias entre el rango de tus gastos

summary(clean_data$FareAmount)

  # Este es un ejemplo, elige 4 categorías en el rango 

FareAmountGroups <- cut(clean_data$FareAmount,
                        breaks = c(-Inf, 949, 1499, 2499, 3999,Inf),
                        labels = c("< 950", "950 - 1499", "1500 - 2499","2500-3999","4000+" ))

  # Creámos la tabla de frecuencia para los gastos y 
  # la convertimos en data frame

tabla <- as.data.frame( TableCreation(FareAmountGroups))

  # Estas variables son para poder visualizar los datos con 
  # labels descriptivas

Rango <- tabla$y

Frequency <- tabla$Freq

Percent <- tabla$Freq.1

  # Si tenemos datos dispersos, esta tabla de 
  # frecuencia nos permite identificar mejor nuestros gastos

tablaFrecuencia <- data.frame(Rango, Frequency, Percent)

tablaFrecuencia

#------------------------------------------------------------------------

  # Histograma de frecuencia de la tarifa de nuestros viajes 

ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
            geom_histogram(fill="lightblue",color="black")+
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+
            labs(x = "Fare", y = "Frecuency"))
#------------------------------------------------------------------------


  # Gráfico QQNORM
#qqnorm(clean_data$FareAmount)+
#  theme(panel.background = element_blank(),
#        panel.grid.major = element_blank(),
#        panel.grid.minor = element_blank())
#------------------------------------------------------------------------

  # Histograma de densidad de nuestros gastos

ggplotly( ggplot(clean_data, aes(x = FareAmount)) + 
            geom_histogram(aes(y = ..density..),
                           fill="lightblue",color="black")+
            theme(panel.background = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank())+
            labs(x = "Fare", y = "Desnsity")+
            geom_density(alpha=.2,fill="red"))
#------------------------------------------------------------------------

  # Obtenemos media y las medidas de disperción de los gastos

mediaTarifa <- mean(clean_data$FareAmount)

VarianzaTarifa <- var(clean_data$FareAmount)

desvEstTarifa <-sd(clean_data$FareAmount)

Coef <- (desvEstFA/mediaFA)*100

  # Creé este data frame para poder visualizar mejor estos datos
  # Interpretalos, ¿Hay mucha variabilidad en tus gastos?

MedidasDisp <- data.frame(MediaTarifa, Varianza, 
                          DesviacionEst,`CoefVar(%)`)
MedidasDisp

#------------------------------------------------------------------------

  # Calculamos la curtosis
summary(clean_data$FareAmount)
library(psych)

kurtosi(clean_data$FareAmount)/sqrt(6/23) 

#------------------------------------------------------------------------

  # Función para calcular la moda

mode <- function(x) {
  return((names(which.max(table(x)))))
}

  # Descubramos la moda de nuestras variables de estudio 

mode(clean_data$ProductType)

mode(clean_data$FareAmount)

summary(clean_data$FareAmount)

#------------------------------------------------------------------------
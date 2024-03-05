##############################
### ANALISIS DE DATOS FORM ###
##############################

### insert FORM image

### FORM is a Mexican company located at The Parque Industrial El Sabinal at Apodoca, Nuevo León. Briefly, FORM develops packaging solutions which add value
### to the supply chain our their clients. In so doing, FORM's solutions not only protect its costumers' products, but also address the costumers' needs during 
### the transportation and storage operations. 

### loading libraries 
library(foreign)
library(dplyr)        # data manipulation 
library(forcats)      # to work with categorical variables
library(ggplot2)      # data visualization 
library(janitor)      # data exploration and cleaning 
library(Hmisc)        # several useful functions for data analysis 
library(psych)        # functions for multivariate analysis 
library(naniar)       # summaries and visualization of missing values NAs
library(dlookr)       # summaries and visualization of missing values NAs
library(corrplot)     # correlation plots
library(jtools)       # presentation of regression analysis 
library(lmtest)       # diagnostic checks - linear regression analysis 
library(car)          # diagnostic checks - linear regression analysis
library(olsrr)        # diagnostic checks - linear regression analysis 
library(kableExtra)   # HTML table attributes
library(arsenal)

setwd("D:\\Tec21\\CD2001C\\FORM - Datos")

##################
### Producción ###
##################
produccion<-read.csv("D:\\Tec21\\CD2001C\\FORM - Datos\\form_produc_datos.csv") # using data from 01-August-2022 to 31-August-2022
produccion<-as.data.frame(produccion)
summary(produccion)                           # get the variables' names to select columns

# select columns / variables 
produccion_alt<-select(produccion,Fecha,CLIENTE,PIEZAS.PROG.,TMO..MIN.,ESTACION.ARRANQUE,Laminas.procesadas,TIEMPO.CALIDAD)
colnames(produccion_alt)<-c('fecha','cliente','piezas_prog','tiempo_min','estacion_arranque','laminas_procesadas','tiempo_calidad')

# lets verify type of variables (quantitative and qualitative)
str(produccion_alt)

# lets convert character variables to numeric 
produccion_alt$piezas_prog<-as.numeric(produccion_alt$piezas_prog)                 ### missing values are converted to NA's
produccion_alt$tiempo_min<-as.numeric(produccion_alt$tiempo_min)                   ### missing values are converted to NA's
produccion_alt$laminas_procesadas<-as.numeric(produccion_alt$laminas_procesadas)   ### missing values are converted to NA's
produccion_alt$tiempo_calidad<-as.numeric(produccion_alt$tiempo_calidad)           ### missing values are converted to NA's

# lets convert character variables to factor so we can calculate descriptive statistics 
produccion_alt$cliente<-as.factor(produccion_alt$cliente)  
produccion_alt$estacion_arranque<-as.factor(produccion_alt$estacion_arranque) 

# identify missing values
colSums(is.na(produccion_alt))

# replace missing values by the median: piezas_prog, tiempo_min, estacion_arranque, laminas_procesadas, tiempo_calidad
produccion_alt<-produccion_alt %>% mutate(piezas_prog=ifelse(is.na(piezas_prog),median(piezas_prog,na.rm=T),piezas_prog))
produccion_alt<-produccion_alt %>% mutate(tiempo_min=ifelse(is.na(tiempo_min),median(tiempo_min,na.rm=T),tiempo_min))
produccion_alt<-produccion_alt %>% mutate(estacion_arranque=ifelse(is.na(estacion_arranque),median(estacion_arranque,na.rm=T),estacion_arranque))
produccion_alt<-produccion_alt %>% mutate(laminas_procesadas=ifelse(is.na(laminas_procesadas),median(laminas_procesadas,na.rm=T),laminas_procesadas))
produccion_alt<-produccion_alt %>% mutate(tiempo_calidad=ifelse(is.na(tiempo_calidad),median(tiempo_calidad,na.rm=T),tiempo_calidad))

# lets verify that we replaced missing values
colSums(is.na(produccion_alt))
summary(produccion_alt)

# lets convert "fecha" to date format 
produccion_alt$fecha<-as.Date(produccion_alt$fecha,format="%m/%d/%Y") 

str(produccion_alt)
summary(produccion_alt) # lets verify the structure and descriptive statistics of our final version - produccion  

# exporting cleaned dataset
write.csv(produccion_alt,"D:\\Tec21\\CD2001C\\FORM - Datos\\form_produccion_final.csv", row.names=FALSE)

# (*) Hint: The following questions can be answered using the library(dplyr) #

# which client represent the highest level of "piezas_programadas"?
produccion_alt1<-produccion_alt %>% select(cliente,piezas_prog) %>% group_by(cliente) %>% summarise(piezas_prog=sum(piezas_prog)) %>% arrange(desc(piezas_prog))
ggplot(produccion_alt1, aes(x=reorder(cliente,piezas_prog), y=piezas_prog)) +
  geom_bar(stat="identity")+
  coord_flip()

# how is the "tiempo_calidad# performance by the top 5 clients?
produccion_alt2<-produccion_alt %>% select(cliente,piezas_prog,tiempo_calidad) %>% group_by(cliente) %>% summarise(across(everything(),mean,na.rm=TRUE)) %>% arrange(desc(piezas_prog))
ggplot(produccion_alt2, aes(x=reorder(cliente,piezas_prog), y=piezas_prog, fill=(tiempo_calidad))) +
  geom_bar(stat="identity")+
  coord_flip()+
  guides(fill=guide_legend(reverse=FALSE))

produccion_alt2.1<-produccion_alt2 %>% filter(cliente %in% c('STABILUS 1','STABILUS 3','YANFENG','TRMX','DENSO')) 

summary_tab1<-tableby(cliente ~ piezas_prog + tiempo_calidad, data=produccion_alt2.1, test=FALSE) 
summary(summary_tab1, title= "Summary Statistics of Producción by Client")

# how is the relationship between "laminas_procesadas" and "piezas_prog"? Please describe this relationsip for the top 5 clients. 
produccion_alt3<-produccion_alt %>% select(cliente,piezas_prog,laminas_procesadas,tiempo_calidad) %>% group_by(cliente) %>% 
  summarise(piezas_prog=sum(piezas_prog),laminas_procesadas=sum(laminas_procesadas),tiempo_calidad=sum(tiempo_calidad)) %>% arrange(desc(piezas_prog))

produccion_alt3<-produccion_alt3[-c(7,8,9),] # remove the last 3 rows 

ggplot(produccion_alt3, aes(x=laminas_procesadas, y=piezas_prog, color=cliente, shape=cliente)) + 
  geom_point(shape=19,size=5)+
   labs(title="Piezas Programadas por Tiempo de Calidad",
       x="Laminas Procesadas", y = "Piezas Programadas")

ggplot(produccion_alt3, aes(x=tiempo_calidad, y=piezas_prog, color=cliente, shape=cliente)) + 
  geom_point(shape=19,size=5)+
  labs(title="Piezas Programadas por Tiempo de Calidad",
       x="Tiempo Calidad", y = "Piezas Programadas")

# which clients display the greatest dispersion on "piezas_prog"?
produccion_alt4<-produccion_alt %>% select(cliente,piezas_prog) %>% arrange(desc(piezas_prog))

produccion_alt4<-produccion_alt4[-c(1099),] # remove client "VL-017-14086" 

ggplot(produccion_alt4, aes(x=cliente, y=piezas_prog)) + 
  geom_boxplot(color="red", fill="orange", alpha=0.2)

# how is the "tiempo_min" performance by the top 5 clients? 
produccion_alt5<-produccion_alt %>% select(cliente,piezas_prog,tiempo_min) %>% group_by(cliente) %>% 
  summarise(piezas_prog=sum(piezas_prog),tiempo_min=sum(tiempo_min)) %>% arrange(desc(piezas_prog))

produccion_alt5<-produccion_alt5[-c(7,8,9),] # remove the last 3 rows 

ggplot(produccion_alt5,aes(x=reorder(tiempo_min,piezas_prog), y=piezas_prog,fill=cliente)) +
  geom_bar(stat="identity")

##################
###     RH    ####
##################
rh<-read.csv("D:\\Tec21\\CD2001C\\FORM - Datos\\form_rh_datos.csv") 

# lets very the structure of the dataset
str(rh) 

# select columns / variables
rh_alt<-rh %>% select(-one_of('No..De.Empleado','APELLIDOS','NOMBRE','RFC','FECHA.DE.ALTA','Primer.mes','X4to.mes','BAJA','NO.SEGURO.SOCIAL','FACTOR.CRED.INFONAVIT','NO..CREDITO.INFONAVIT','LUGAR.DE.NACIMIENTO',
                          'CURP','CALLE','NUMERO.INTERNO','COLONIA','CODIGO.POSTAL','TARJETA.CUENTA'))

# lets rename the selected columns / variables
colnames(rh_alt)<-c('fecha_nacimiento','fecha_actual','genero','puesto','depto','salario_diario','mpio','estado','estado_civil')

# lets convert "fecha_nacimiento" to date format 
rh_alt$fecha_nacimiento<-as.Date(rh_alt$fecha_nacimiento,format="%m/%d/%Y") 
rh_alt$fecha_actual<-as.Date(rh_alt$fecha_actual,format="%m/%d/%Y") 

# lets calculate the variable "age" in years so we can realize additional characteristics of "colaboradores"
library(lubridate)
edad<-trunc((rh_alt$fecha_nacimiento %--% rh_alt$fecha_actual) / years(1)) # %--% creates a time interval based on as.date() format
rh_alt$edad<-edad

# lets convert character variables to factor so we can display descriptive statistics
rh_alt$genero<-as.factor(rh_alt$genero)
rh_alt$puesto<-as.factor(rh_alt$puesto)
rh_alt$depto<-as.factor(rh_alt$depto)
rh_alt$mpio<-as.factor(rh_alt$mpio)
rh_alt$estado<-as.factor(rh_alt$estado)
rh_alt$estado_civil<-as.factor(rh_alt$estado_civil)

# lets verify the structure of our rh_alt dataset 
str(rh_alt)
summary(rh_alt)

rh_alt<-rh_alt[-c(53),] # remove row # 53 because it shows a missing value. 
summary(rh_alt) # no missing values

# lets display data visualization plots so we can identify relevant insights from our rh_alt dataset
tapply(rh_alt$salario_diario,
       list(rh_alt$genero,rh_alt$estado_civil), mean)

# replace the "outlier" of salario_diario with the median
rh_alt$salario_diario<-replace(rh_alt$salario_diario,rh_alt$salario_diario>1000000,181)

# lets display data visualization plots so we can identify relevant insights from our rh_alt dataset
tapply(rh_alt$salario_diario,
       list(rh_alt$genero,rh_alt$estado_civil), mean)

hist(rh_alt$edad, freq=TRUE, col='light blue', main="Histograma de Edad",xlab="Edad en Años")

ggplot(rh_alt, aes(x=genero, y=edad, fill=genero)) + 
  geom_boxplot() 

ggplot(rh_alt, aes(x=genero, y=salario_diario, fill=genero)) + 
  geom_boxplot() 

ggplot(rh_alt, aes(genero,salario_diario,fill=genero)) +                                    
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set2") + ggtitle("Salario Diario por Genero")
  

ggplot(rh_alt, aes(x=genero, y=salario_diario, fill=genero)) + 
  geom_bar(stat="identity") + 
  facet_grid(~estado_civil) + scale_fill_brewer(palette = "Set1")

summary_tab2<-tableby(genero ~ salario_diario + edad + estado_civil, data=rh_alt,test=FALSE)
summary(summary_tab2)

####################
### Distribución ###
####################
delivery_performance<-read.csv("D:\\Tec21\\CD2001C\\FORM - Datos\\form_delivery_performance.csv")
str(delivery_performance)
delivery_performance<-as.data.frame(delivery_performance) 
delivery_performance$fecha<-as.Date(delivery_performance$fecha,format="%m/%d/%Y") 
delivery_performance$cliente<-as.factor(delivery_performance$cliente)  

# lets verify the dataset's structure
summary(delivery_performance)

# lets plot delay performance by client 
ggplot(delivery_performance,aes(x=fecha, y=delay_performance,color=cliente))+
  geom_line()+
  labs(x="Fecha",y="Delay in Minutes", color="Legend")+
  ggtitle("Delays in Performance by Client")

# lets remove two clients displaying 0's values of delay performance 
delivery_performance<-delivery_performance[delivery_performance$cliente!="Magna",]
delivery_performance<-delivery_performance[delivery_performance$cliente!="Varroc",]

# lets get the mean value of delay performance 
summary(delivery_performance$delay_performance)

# lets plot again the 2 clients characterized by the longest delay
ggplot(delivery_performance,aes(x=fecha, y=delay_performance,fill=cliente))+
  geom_bar(stat="identity")+
  geom_hline(yintercept=33,linetype="dashed",color="black")+
  labs(x="Fecha",y="Delay in Minutes", color="Legend")+
  ggtitle("Delays in Performance by Client")

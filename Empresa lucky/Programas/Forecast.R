library(RODBC)
library(tidyverse)
library(fabletools)
library(tsibble)
library(fpp3)
library(tictoc)

#----------------Importación data--------------------------
con<-odbcConnect(DBMSencoding = "UTF-8",
                 "SQL_RETAILERS",
                 uid="mcallejas",
                 pwd=rstudioapi::askForPassword("Database password")
)
odbcCloseAll()#CIERRA CONECCIÓN (IMPORANTE DESPUES DE IMPORTAR CERRAR LA CONEXIÓN)
rm(con)

tic()
CocinaBella <- sqlQuery(con,"select * from base_empx")
CocinaBella.cat <- sqlQuery(con,"select * from cat_empx")
Cosmomex <- sqlQuery(con,"select * from base_empy")
Cosmomex.cat <- sqlQuery(con,"select * from cat_empy")
toc() #318.19 seg, 5.3 min


#----------------Generación data para modelos-------------------

base.a <- CocinaBella %>% 
  inner_join(CocinaBella.cat, by = c("ID_PROD" = "ID")) %>% 
  mutate(Mes = yearmonth(Mes)) %>% 
  summarise(Existencias = sum(INV),
            Desplazamientos = sum(VTA),
            .by = c(Mes,LINEA))

base.b <- Cosmomex %>% 
  inner_join(Cosmomex.cat, by = c("ID_PROD" = "ID")) %>% 
  mutate(Mes = yearmonth(Mes)) %>% 
  summarise(Existencias = sum(INV),
            Desplazamientos = sum(VTA),
            .by = c(Mes,LINEA))

#tsibble de las tres lineas de negocio
ts.linea <- bind_rows(base.a,base.b) %>% 
  summarise(Existencias = sum(Existencias),
            Desplazamientos = sum(Desplazamientos),
            .by = c(Mes,LINEA)) %>% 
  tsibble(key = LINEA, index = Mes)

#tsibble total
ts.total <- ts.linea %>%
  summarise(Existencias = sum(Existencias), 
            Desplazamientos = sum(Desplazamientos))


#save(ts.total, file = "ts.total.Rdata")
#save(ts.linea, file = "ts.linea.Rdata")


#----------------Tendencias-------------------

#Tendencia total (método X-11)
trend.total <- ts.total %>% 
  model(X_13ARIMA_SEATS(formula = Desplazamientos ~ x11())) %>% 
  components() %>% 
  select(trend) %>% 
  filter(as.Date(Mes) >= ymd("2020-01-01"))
  
autoplot(trend.total)

ts.total %>% 
  model(X_13ARIMA_SEATS(formula = Desplazamientos ~ x11())) %>% 
  components() %>% 
  autoplot()

#Tendencia por linea de negocio (método X-11)
trend.linea <- ts.linea %>% 
  model(X_13ARIMA_SEATS(formula = Desplazamientos ~ x11())) %>% 
  components() %>% 
  select(trend) %>% 
  filter(as.Date(Mes) >= ymd("2020-01-01"))

autoplot(trend.linea)


#----------------Forecast Total-------------------
#Total, exploración
#Gráfico general
ts.total %>% 
  mutate(Mes = as.Date(Mes)) %>% 
  autoplot(Desplazamientos, col = "blue",linewidth = 1.3) +
  scale_x_date(date_breaks = "4 month", date_labels = "%b %y") +
  labs(title = "Desplazamientos totales") +
  xlab("Mes-Año") +
  theme(axis.text.x = element_text(angle=90, size =10),
        axis.text.y = element_text(size = 10))

#Busqueda de outliers
ts.total$Desplazamientos %>% boxplot()

#Correlación de Existencias vs Desplazamientos
ts.total %>% 
  ggplot(aes(Existencias,Desplazamientos)) +
  geom_point() +
  geom_smooth()
cor(ts.total$Existencias,ts.total$Desplazamientos) #Correlacipon de 0.8832 (Existe alta correlación)


#Generación data entrenamiento y de validación
ts.total.train <- ts.total %>% 
  filter_index("2018-01" ~ "2022-03")

ts.total.test <- ts.total %>% 
  filter_index("2022-04" ~ .)


#Diferencias para generar la ts  estacionaria, exploración ACF, PACF
ts.total.train %>% 
  features(Desplazamientos,unitroot_ndiffs,  lag = 10)
ts.total.train %>% 
  gg_tsdisplay(Desplazamientos,)
ts.total.train %>% 
  mutate(Desplazamientos.diff = difference(Desplazamientos,1)) %>% 
  gg_tsdisplay(Desplazamientos.diff, plot_type = "partial")


#Generación de modelos (para arima se probasron múltiples combinaciones 
#en base a los gráficos de acf y pacf con 1 diff para transformar la ts a estacionaría)
models.forecast <- ts.total.train %>% 
  model("auto.arima" = ARIMA(Desplazamientos ~ pdq() + PDQ() ,
              approximation = FALSE, stepwise = FALSE),
        "arima" = ARIMA(Desplazamientos ~ pdq(1,1,1) + trend() + Existencias),
        "ETS" = ETS(Desplazamientos ~ error() + trend() + season()),
        "linear" = TSLM(Desplazamientos ~ trend() + season() + Existencias )
        ) 


#Evaluación y selección de modelo
#AICc
models.forecast[1] %>% report() #auto.arima AICc= 965.94
models.forecast[2] %>% report() #arima AICc= 965.03
models.forecast[3] %>% report() #ETS AICc= 1039.21
models.forecast[4] %>% report() #linear adjuster R sauqred = 0.85

#residuales
models.residuals <- list(
  "res_auto.arima" = residuals(models.forecast[1]) %>%  select(.resid),
  "res_arima" = residuals(models.forecast[2]) %>%  select(.resid),
  "res_ETS" = residuals(models.forecast[3]) %>%  select(.resid),
  "res_linear" = residuals(models.forecast[4]) %>%  select(.resid)
)


models.residuals$res_auto.arima$.resid %>% tseries::jarque.bera.test() #No normal
models.residuals$res_arima$.resid %>% tseries::jarque.bera.test() #No normal
models.residuals$res_ETS$.resid %>% tseries::jarque.bera.test() #Normal
models.residuals$res_linear$.resid %>% tseries::jarque.bera.test() #Normal

models.forecast %>% augment() %>% features(.innov, box_pierce, lag = 10) #Residuales no autocorrelacionados
models.forecast %>% augment() %>% features(.innov, ljung_box, lag = 10) #Residuales no autocorrelacionados

gg_tsresiduals(models.forecast[1]) 
gg_tsresiduals(models.forecast[2]) 
gg_tsresiduals(models.forecast[3])
gg_tsresiduals(models.forecast[4]) 


#accuary (precisión)
test.f <- list(
  "auto.arima" = forecast(models.forecast[1], h = 5),
  "arima" = forecast(models.forecast[2], new_data = ts.total.test), 
  "ETS" = forecast(models.forecast[3], h = 5),
  "linear" = forecast(models.forecast[4], new_data = ts.total.test)
)

accuracy(models.forecast)

accuracy(test.f[[1]],ts.total.test)
accuracy(test.f[[2]],ts.total.test)
accuracy(test.f[[3]],ts.total.test)
accuracy(test.f[[4]],ts.total.test)


#auto.arima RMSE: 3344,1500 MAE:2277,1256
#arima RMSE:3125,548  MAE:2166,469
#ETS RMSE:3731,1290  MAE:2665,883
#linear RMSE:3489,1686,  MAE:3106,1234

ts.plot <- tibble("Mes" = rep(test.f$auto.arima$Mes,4),
        "Desplazamientos" = c(test.f$auto.arima$.mean,test.f$arima$.mean,test.f$ETS$.mean,test.f$linear$.mean),
       "Model" = rep(c("auto.arima","arima","ETS","linear"),times = 1, each = 5))


bind_rows(tibble(ts.total[,c("Mes","Desplazamientos")]), tibble(ts.plot)) %>% 
  mutate(Model = ifelse(is.na(Model),"Original",Model),
         Mes = as.Date(Mes)) %>%
  ggplot() +
  geom_line(aes(Mes,Desplazamientos,col = Model),linewidth = 1.1) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %y",
               limits = c(ymd("2021-08-01"),ymd("2022-08-01"))) +
  scale_y_continuous(limits = c(60000,90000)) +
  labs(title = "Forecast Modelos") +
  xlab("Mes-Año") +
  theme(axis.text.x = element_text(angle=90, size =10),
        axis.text.y = element_text(size = 10))
  

#Por lo tanto se elije el modelo "Arima" (Con este se realizara la predicción final mediante 
#boostcrapping y bagging)
#El problema con este modelo es que es necesario un nuevo forecast para los inventarios.

#Predicción Inventarios
models.forecast.Existencias <- ts.total %>% 
  model("auto.arima" = ARIMA(Existencias ~ pdq() + PDQ() ,
                             approximation = FALSE, stepwise = FALSE),
        "ETS" = ETS(Existencias ~ error() + trend())
  )

models.forecast.Existencias[1] %>% report()
models.forecast.Existencias[2] %>% report()
models.forecast.Existencias %>% augment() %>% features(.innov, box_pierce, lag = 10) #Residuales no autocorrelacionados
models.forecast.Existencias %>% augment() %>% features(.innov, ljung_box, lag = 10) #Residuales no autocorrelacionados
accuracy(models.forecast.Existencias)

fc.Existencias <- forecast(models.forecast.Existencias[1], h = 6,bootstrap = TRUE,
                           times = 10000) %>% 
  tibble() %>% 
  select(Mes,`.mean`) %>% 
  rename(Existencias = `.mean`) %>% 
  tsibble(index = Mes)

#Forecast final
model.total.final <- ts.total %>% 
  model("arima" = ARIMA(Desplazamientos ~ pdq(1,1,1) + trend() + Existencias) )

tic()
fc.total.final <- forecast(model.total.final,new_data = fc.Existencias,bootstrap = TRUE,
         times = 10000) %>% 
  mutate(Mes = as.Date(Mes))
toc() #10.17 s

fc.total.final %>% 
  tibble() %>% 
  transmute(Mes,
            Desplazamientos = .mean,
            data = "Forecast") %>% 
  bind_rows(ts.total[,c(1,3)]) %>% 
  mutate(Mes = as.Date(Mes),
         data = ifelse(is.na(data),"Original",data)) %>% 
  arrange(Mes) %>% 
  ggplot() +
  geom_line(aes(Mes,Desplazamientos, col = data),linewidth = 1.1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y",
               limits = c(ymd("2021-08-01"),ymd("2023-03-01"))) +
  scale_y_continuous(limits = c(60000,98000)) +
  labs(title = "Forecast Total") +
  xlab("Mes-Año") +
  theme(axis.text.x = element_text(angle=90, size =10),
        axis.text.y = element_text(size = 10))

ts.total %>% 
  autoplot(Desplazamientos) +
  autolayer(forecast(model.total.final,new_data = fc.Existencias,bootstrap = TRUE,
                     times = 10000))



#----------------Forecast por Linea-------------------
lineas <- ts.linea$LINEA %>% unique
repalce_outliers<-function(x,removeNA=T){
  qrts<-quantile(x,probs = c(0.25,0.75),na.rm = removeNA)
  iqr<-qrts[2]-qrts[1]
  h<-1.5*iqr
  x[x<qrts[1]-h] <- NA
  x[x>qrts[2]+h] <- NA
  x <- imputeTS::na_kalman(x$Desplazamientos)
}#Función para remplazar los outliers por el percentil 10 o 85
resid_normal.test <- function(mb){
  
  modelos <- c("auto.arima","auto.arima.existencias","ETS","linear")
  r <- list()
  for (i in seq_along(modelos)) {
    r[[i]] <- mb %>% residuals() %>% 
      tibble() %>% 
      filter(.model == modelos[i]) %>% 
      select(.resid)
    r[[i]] <- tseries::jarque.bera.test(r[[i]]$.resid)
    r[[i]] <- ifelse(r[[i]]$p.value>0.05,
                     "Residuales con distribución Normal",
                     "Residuales con distribución NO Normal") %>% 
      data.frame()
  }
  
  print(tibble("modelo" = modelos,"Normal test" = data.table::rbindlist(r)[[1]]))
} #Prueba Jarquer Bera de normalidad para residuales en los modelos

ts.linea %>% 
  filter(LINEA == "Cocina") %>% 
  ggplot(aes(Mes,Desplazamientos, fill = LINEA), col = "black") +
  geom_boxplot()

ts.linea %>% 
  filter(LINEA != "Cocina") %>% 
  ggplot(aes(Mes,Desplazamientos, fill = LINEA), col = "black") +
  geom_boxplot()

ts.linea[ts.linea$LINEA=="Electrodoméstico","Desplazamientos"] <- ts.linea[ts.linea$LINEA=="Electrodoméstico","Desplazamientos"] %>% 
  repalce_outliers
ts.linea[ts.linea$LINEA=="Maquillaje","Desplazamientos"] <- ts.linea[ts.linea$LINEA=="Maquillaje","Desplazamientos"] %>% 
  repalce_outliers

#Datos entrenamiento y test
ts.linea.train <- ts.linea %>% 
  filter_index( ~ "2022-03")
ts.linea.test <- ts.linea %>% 
  filter_index("2022-04" ~ .)
  

models.forecast.linea <- list()
tic()
for (i in seq_along(lineas)) {
  models.forecast.linea[[i]] <- ts.linea.train %>% 
    filter(LINEA == lineas[i]) %>% 
  model("auto.arima" = ARIMA(Desplazamientos ~ pdq() + PDQ() + trend(),
                             approximation = FALSE, stepwise = FALSE),
        "auto.arima.existencias"=ARIMA(Desplazamientos ~ pdq() + PDQ() + trend() + Existencias,
                                       approximation = FALSE, stepwise = FALSE),
        "ETS" = ETS(Desplazamientos ~ error() + trend() + season()),
        "linear" = TSLM(Desplazamientos ~ trend() + Existencias )
  ) 
}
toc() #68.5 s
names(models.forecast.linea) <- lineas


#Evaluación por linea
resid_normal.test(models.forecast.linea$Cocina) #auto.arima(F)/auto.arima.existencias(F)/ETS(T)/linear(T)
resid_normal.test(models.forecast.linea$Electrodoméstico) #auto.arima(F)/auto.arima.existencias(F)/ETS(T)/linear(F)
resid_normal.test(models.forecast.linea$Maquillaje)#auto.arima(F)/auto.arima.existencias(F)/ETS(T)/linear(T)


#p<0.05 => autocorrelacionados
models.forecast.linea$Cocina %>% augment() %>% features(.innov, box_pierce, lag = 10)#linear correlacionado
models.forecast.linea$Cocina %>% augment() %>% features(.innov, box_pierce, lag = 10)#linear correlacionado
models.forecast.linea$Cocina %>% augment() %>% features(.innov, box_pierce, lag = 10)#linear correlacionado

#accuracy
accuracy(models.forecast.linea$Cocina)
accuracy(models.forecast.linea$Electrodoméstico)
accuracy(models.forecast.linea$Maquillaje)

#Gráficos
forecast.test <- list(
"Cocina" = models.forecast.linea$Cocina %>% forecast(new_data = ts.linea.test),
"Electrodoméstico" = models.forecast.linea$Electrodoméstico %>% forecast(new_data = ts.linea.test),
"Maquillaje" = models.forecast.linea$Maquillaje %>% forecast(new_data = ts.linea.test)
)

forecast.test$Cocina %>% 
  tibble() %>% 
  select(Mes,LINEA,.model,.mean) %>% 
  rename(Model = .model,
         Desplazamientos = .mean) %>% 
  bind_rows(ts.linea %>% tibble() %>% transmute(Mes,LINEA,Model = "Original", Desplazamientos)) %>% 
  filter(LINEA == "Cocina") %>% 
  mutate(Mes = as.Date(Mes)) %>% 
  ggplot() +
  geom_line(aes(Mes,Desplazamientos,col = Model),linewidth = 1.1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y",
               limits = c(ymd("2021-01-01"),ymd("2022-8-01")))

forecast.test$Electrodoméstico %>% 
  tibble() %>% 
  select(Mes,LINEA,.model,.mean) %>% 
  rename(Model = .model,
         Desplazamientos = .mean) %>% 
  bind_rows(ts.linea %>% tibble() %>% transmute(Mes,LINEA,Model = "Original", Desplazamientos)) %>% 
  filter(LINEA == "Electrodoméstico") %>% 
  mutate(Mes = as.Date(Mes)) %>% 
  ggplot() +
  geom_line(aes(Mes,Desplazamientos,col = Model),linewidth = 1.1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y",
               limits = c(ymd("2021-01-01"),ymd("2022-8-01")))

forecast.test$Maquillaje %>% 
  tibble() %>% 
  select(Mes,LINEA,.model,.mean) %>% 
  rename(Model = .model,
         Desplazamientos = .mean) %>% 
  bind_rows(ts.linea %>% tibble() %>% transmute(Mes,LINEA,Model = "Original", Desplazamientos)) %>% 
  filter(LINEA == "Maquillaje") %>% 
  mutate(Mes = as.Date(Mes)) %>% 
  ggplot() +
  geom_line(aes(Mes,Desplazamientos,col = Model),linewidth = 1.1) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b %y",
               limits = c(ymd("2021-01-01"),ymd("2022-8-01")))



#Forecast final para lineas
model.linea.final <- ts.linea %>% 
  model("ETS" = ETS(Desplazamientos ~ error() + trend() + season() ) )

fc.linea.final <- forecast(model.linea.final, h = 5,
                           bootstrap = TRUE,
                           times = 5000)

ts.linea %>% 
  filter(LINEA != "Cocina") %>% 
  autoplot(Desplazamientos) +
  autolayer(filter(fc.linea.final,LINEA != "Cocina"))
  

ts.linea %>% 
  filter(LINEA == "Cocina") %>% 
  autoplot(Desplazamientos) +
  autolayer(filter(fc.linea.final,LINEA == "Cocina"))

#save.image(file = "Lucky.RData")
load("Lucky.RData")


#------------------Data Informe-------------------------

REPORTE <- bind_rows(
  tibble(trend.total) %>% 
    transmute(Mes,
              LINEA = "Total",
              Data = "Tendencia",
              Desplazamientos = trend),
  
  trend.linea %>% 
    tibble() %>% 
    transmute(Mes,
              LINEA,
              Data = "Tendencia",
              Desplazamientos = trend),
  
  fc.total.final %>% 
    tibble() %>% 
    transmute(Mes,
              LINEA = "Total",
              Data = "Forecast",
              Desplazamientos = .mean),
  
  fc.linea.final %>% 
    tibble() %>% 
    transmute(Mes,
              LINEA,
              Data = "Forecast",
              Desplazamientos = .mean)
) %>% 
  arrange(LINEA,Data,Mes)



  
  
writexl::write_xlsx(REPORTE,"REPORTE.xlsx")








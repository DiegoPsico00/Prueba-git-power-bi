library(tidyverse)
library(openxlsx)
library(RODBC)

con<-odbcConnect(DBMSencoding = "UTF-8",
                 "SQL_RETAILERS",
                 uid="mcallejas",
                 pwd=rstudioapi::askForPassword("Database password")
)
odbcCloseAll()#CIERRA CONECCIÓN (IMPORANTE DESPUES DE IMPORTAR CERRAR LA CONEXIÓN)
rm(con)

#------------------Importación----------------

reporte_mensual <- sqlQuery(con,
"select * from Base_Reporte where mes in (
select mes from (
select *,ROW_NUMBER() over(order by mes desc) N from (select distinct mes from Base_Reporte) a 
) b
where N in (1,2,3)
)") %>% 
  tibble()


#----------------------------------------------Preparación Data----------------------------------------------

MESES <- paste0(
  rep(c("INV_","VTA_"),each = 3),
      sort(unique(tsibble::yearmonth(reporte_mensual$mes)))
  )

EQUIPOS <- inner_join(
reporte_mensual %>% 
  rename(NOM_SUC = NOMBRE) %>% 
  select(-VTA) %>% 
  filter(Equipo_Venta != "Distribuido por la Tienda") %>% 
  mutate(mes = tsibble::yearmonth(mes)) %>% 
  pivot_wider(names_from = mes,
              names_prefix = "INV_",
              values_from = INV, 
              values_fill = 0,
              names_sort = TRUE),
  
reporte_mensual %>% 
  rename(NOM_SUC = NOMBRE) %>% 
  select(-INV) %>% 
  filter(Equipo_Venta != "Distribuido por la Tienda") %>% 
  mutate(mes = tsibble::yearmonth(mes)) %>% 
  pivot_wider(names_from = mes, 
              names_prefix = "VTA_",
              values_from = VTA, 
              values_fill = 0,
              names_sort = TRUE)
) %>% 
  mutate(
    DDI = ifelse(
      !!as.symbol(MESES[6]) + !!as.symbol(MESES[5]) == 0 & !!as.symbol(MESES[4]) == 0,
      9999,
      (!!as.symbol(MESES[3]) / (!!as.symbol(MESES[6]) + !!as.symbol(MESES[5]) + !!as.symbol(MESES[4]) ))* 90
    ),
    DDI = round(DDI),
    Observación = case_when(
      between(DDI, 0,30) ~ "Necesita Piezas",
      between(DDI, 31,120) ~ "Correcto",
      between(DDI, 91,9998) ~ "Poca Venta",
      DDI == 9999 ~ "Sin vta en 3 meses/Prod Nuevo",
    )
  ) %>% 
  arrange(Equipo_Venta,TIENDA,desc(DDI)) %>% 
  split(.$Equipo_Venta)
  





#-------------------------Reportes---------------------------------------

#Formato
base1<-createStyle(textDecoration = "Bold",fgFill = "lightblue",halign = "center")#
base1.5<-createStyle(textDecoration = "Bold",fgFill = "lightblue",halign = "center",border = "right")#
base2<-createStyle(textDecoration = "Bold",fgFill = "lightblue",halign = "center",textRotation = 90)#90°
base2.5<-createStyle(textDecoration = "Bold",fgFill = "lightblue",halign = "center",textRotation = 90, border = "right")#90°
base3<-createStyle(textDecoration = "Bold",fgFill = "darkblue",halign = "center",fontColour ="white",wrapText=T)#azul oscuro
base4<-createStyle(textDecoration = "Bold",fgFill = "lightgreen",halign = "center",wrapText=T, border = "right")#azul oscuro
tabla1<-createStyle(bgFill = "white",fgFill = "white", border = NULL)#sin color
tabla2<-createStyle(bgFill = "white",fgFill = "white", border = "right")#sin color

sem1<-createStyle(bgFill ="red",fgFill ="red",border = "right")#sem rojo
sem2<-createStyle(bgFill = "orangered",fgFill = "orangered",border = "right")#sem rosa
sem3<-createStyle(bgFill = "yellow",fgFill = "yellow",border = "right")#sem amarillo
sem4<-createStyle(bgFill = "lightgreen",fgFill = "lightgreen",border = "right")#sem verde
sem5<-createStyle(bgFill = "white",fgFill = "white",border = "right")#sin color


#Generación de Archivos
for (i in seq_along(EQUIPOS)) {
  p1 <- EQUIPOS[[i]] %>% 
    group_by(Observación) %>% 
    tally() %>% 
    ggplot() +
    geom_col(aes(Observación, n, fill = Observación), colour  = "black", show.legend = FALSE) +
    geom_text(aes(Observación,n, label = n), vjust = -1) +
    labs(y = "Número de Casos", x = "Observación")
  
  p2 <- EQUIPOS[[i]] %>% 
    group_by(LINEA,PRODUCTO) %>% 
    summarise(VTA1 = sum(!!as.symbol(MESES[6])),
              VTA2 = sum(!!as.symbol(MESES[5])),
              .groups = "drop") %>% 
    ggplot() +
    geom_col(aes(VTA1,fct_reorder(PRODUCTO,VTA1),colour = "black", fill = PRODUCTO),show.legend = FALSE) +
    geom_point(aes(VTA2,fct_reorder(PRODUCTO,VTA2)),show.legend = FALSE) +
    labs(x = "Ventas", y = "Producto", subtitle = "El punto indica ventas del mes pasado")
  
  
  n <- length(EQUIPOS[[i]]$TIENDA) + 20
  
  wb<-createWorkbook()
  
  addWorksheet(wb,"RESUMEN")
  addWorksheet(wb,"REPORTE")
  
  addStyle(wb,1,tabla1,rows = 1:50,cols=1:50,gridExpand = TRUE)
  print(p2)
  insertPlot(wb,1,width = 19,height = 11,units = "cm", startCol = 2,startRow = 3)
  print(p1)
  insertPlot(wb,1,width = 15,height = 11,units = "cm", startCol = 12,startRow = 3)
  
  
  writeData(wb,2,startRow = 2,EQUIPOS[[i]],withFilter=T)
  setColWidths(wb, 2, cols = 1:16, widths = "auto")
  
  addStyle(wb,2,base1,rows = 2,cols=1:16,gridExpand = TRUE)
  addStyle(wb,2,base1.5,rows = 2,cols=8)
  addStyle(wb,2,base2,rows = 2,cols=9:14)
  addStyle(wb,2,base2.5,rows = 2,cols=c(11,14))
  addStyle(wb,2,base3,rows = 2,cols=15)
  addStyle(wb,2,base4,rows = 2,cols=16)
  addStyle(wb,2,tabla1,rows = 3:n,cols=1:25,gridExpand = TRUE)
  addStyle(wb,2,tabla1,rows = 2:n,cols=17:25,gridExpand = TRUE)
  addStyle(wb,2,tabla2,rows = 3:n,cols=c(8,11,14),gridExpand = TRUE)
  
  conditionalFormatting(wb,2,cols = 15,rows = 3:n, rule = "=9999", style = sem1)
  conditionalFormatting(wb,2,cols = 15,rows = 3:n, rule = "<=9998", style = sem2)
  conditionalFormatting(wb,2,cols = 15,rows = 3:n, rule = "<=120", style = sem3)
  conditionalFormatting(wb,2,cols = 15,rows = 3:n, rule = "<=30", style = sem4)
  conditionalFormatting(wb,2,cols = 15,rows = 3:n, rule = "<=0", style = sem5)
  
  url <- paste0("../Reportes/Reportes Equipos/",names(EQUIPOS)[i],".xlsx")
  saveWorkbook(wb,file=url,overwrite = T)
  
}

#save.image(file = "Reportes.RData")

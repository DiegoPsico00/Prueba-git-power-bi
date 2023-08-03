use RETAILERS

select top 10 * from cat_empx
select top 10 * from cat_empy

select top 10 * from dir_empx
select top 10 * from dir_empy

select top 10 * from base_empx
select top 10 * from base_empy

select top 10 * from id_direcc_zona
select top 10 * from EV_TP_Z

alter table base_empx alter column vta float
alter table base_empx alter column inv float

alter table base_empy alter column vta float
alter table base_empy alter column inv float


---------Define las fechas anuales respecto a al último mes en la base
select (SELECT dateadd(month,datediff(month,0,(select MAX(mes) from base_empx))-11,0)) F1, (select MAX(mes) from base_empx) F2 into #MAT

----------------------------------------------------------------------------------------CocinaBella (emp x)----------------------------

---Validación sucursales
select b.nombre, b.direccion from (select distinct id_tienda from base_empx) a
left join dir_empx b on a.id_tienda = b.id_tienda
where b.nombre is null

---Validación productos
select b.tipo_producto, b.producto from (select distinct id_prod from base_empx) a
left join cat_empx b on a.id_prod = b.id
where b.producto is null


-----Cruces de la base cat_empx
select 
format(mes,'yyyy-MM') mes, NOMBRE,ESTADO,CIUDAD,TIPO_PRODUCTO,PRODUCTO,cat_empx.LINEA,EV Equipo_Venta,
INV, VTA
into #cruce
from base_empx
inner join dir_empx on base_empx.id_tienda = dir_empx.id_tienda
inner join cat_empx on base_empx.id_prod = cat_empx.id
left join id_direcc_zona on dir_empx.direccion = id_direcc_zona.id_direccion
left join EV_TP_Z on EV_TP_Z.zona = id_direcc_zona.zona and EV_TP_Z.TP = cat_empx.TIPO_PRODUCTO
where mes between
(select F1 from #MAT)-----un año anterior al último mes en base
and
(select F2 from #MAT)-------último mes en base



----Validación del cruce cat_empx
select a.mes,
vta_base, vta_cruce,
IIF(vta_base-vta_cruce =0, 'ok','error') Val_vta,
inv_base,inv_cruce,
IIF(inv_base-inv_cruce =0, 'ok','error') Val_inv
from 
(
select format(mes, 'yyyy-MM') mes,SUM(vta) vta_base, SUM(inv) inv_base from base_empx 
where mes between (select F1 from #MAT) and (select F2 from #MAT)
group by mes
) a
inner join 
(
select mes,SUM(vta) vta_cruce, SUM(inv) inv_cruce from #cruce
group by mes
) b 
on a.mes=b.mes 
order by mes desc



----------------------------------------------------------------------------------------Cosmomex (emp y)----------------------------

---Validación sucursales
select b.nombre, b.direccion from (select distinct id_tienda from base_empy) a
left join dir_empy b on a.id_tienda = b.id_tienda
where b.nombre is null

---Validación sucursales
select b.tipo_producto, b.producto from (select distinct id_prod from base_empy) a
left join cat_empy b on a.id_prod = b.id
where b.producto is null


-----Cruces de la base cat_empy
select 
format(mes,'yyyy-MM') mes, NOMBRE,ESTADO,CIUDAD,TIPO_PRODUCTO,PRODUCTO,cat_empy.LINEA,EV Equipo_Venta,
INV, VTA
into #cruce2
from base_empy
inner join dir_empy on base_empy.id_tienda = dir_empy.id_tienda
inner join cat_empy on base_empy.id_prod = cat_empy.id
left join id_direcc_zona on dir_empy.direccion = id_direcc_zona.id_direccion
left join EV_TP_Z on EV_TP_Z.zona = id_direcc_zona.zona and EV_TP_Z.TP = cat_empy.TIPO_PRODUCTO
where mes between
(select F1 from #MAT)-----un año anterior al último mes en base
and
(select F2 from #MAT)-------último mes en base


----Validación del cruce cat_empy
select a.mes,
vta_base, vta_cruce,
IIF(vta_base-vta_cruce =0, 'ok','error') Val_vta,
inv_base,inv_cruce,
IIF(inv_base-inv_cruce =0, 'ok','error') Val_inv
from 
(
select format(mes, 'yyyy-MM') mes,SUM(vta) vta_base, SUM(inv) inv_base from base_empy 
where mes between (select F1 from #MAT) and (select F2 from #MAT)
group by mes
) a
inner join 
(
select mes,SUM(vta) vta_cruce, SUM(inv) inv_cruce from #cruce2
group by mes
) b 
on a.mes=b.mes 
order by mes desc


----------------------------------------------------------------------------------------Base_Reporte----------------------------
--drop table Base_Reporte
select * 
into Base_Reporte
from 
(
select 'CocinaBella' TIENDA,* from #cruce
union all
select 'Cosmomex' TIENDA,* from #cruce2
) a
order by TIENDA,mes,LINEA


update Base_Reporte set Equipo_Venta = 'Distribuido por la Tienda' where Equipo_Venta is null



























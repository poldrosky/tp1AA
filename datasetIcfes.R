library('DBI')
library('RPostgreSQL')

drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="db",host="localhost",port=5432,
                 user="usar",password="passwd")

query <- "
SELECT 
CASE
WHEN 
cast(db.mod_razona_cuantitativo_punt as double precision)>(select avg(cast(b1.mod_razona_cuantitativo_punt as 
double precision)) from db_icfes b1)
then 'SOBRE LA MEDIA'
else 'BAJO LA MEDIA'
END as mod_razona_cuantitativo_desem
,
CASE 
WHEN sf.estu_genero = 1 THEN 'M'
WHEN sf.estu_genero = 2 THEN 'F'
END as genero,		
sf.estu_edad_examen,	
estado_civil_nombre as estado_civil,	
hogar_actual_nombre as estu_hogar_actual,
CASE
WHEN sf.estu_sn_cabeza_fmlia=0 THEN 'NO'
WHEN sf.estu_sn_cabeza_fmlia=1 THEN 'SI'
END as estu_sn_cabeza_fmlia,	
CASE
WHEN estu_pers_cargo=0 THEN 'NO'
WHEN estu_pers_cargo>0 THEN 'SI'
END as estu_pers_cargo, 
nivel_educativo_nombre as fami_nivel_educa_padres,
p.ocupacion_nombre as fami_ocup_padre, 
m.ocupacion_nombre as fami_ocup_madre, 
clasificacion as inst_tipo,
ca.caracter_academico_nombre as inst_caracter_academico,
CASE
WHEN 
(select count(*) from instituciones_acreditadas_colombia where institucion=db.inst_nombre_institucion)>=1
then 'INSTITUCION ACREDITADA'
else 'INSTITUCION NO ACREDITADA'
END as inst_acreditada,
CASE
WHEN
(select count(*) from prog_acreditados where institucion=db.inst_nombre_institucion 
and programa=db.estu_prgm_academico)>=1 then 'PROGRAMA ACREDITADO'
else
'PROGRAMA NO ACREDITADO'
END as prog_acreditado,
mun.municipio_nombre as inst_programa_zona, 
metodo_programa_nombre as estu_metodo_prgm, 
area_conocimiento_nombre as estu_area_conoc, 
grupo_referencia_nombre as area_grupo_referencia,
CASE 
WHEN sb.estu_pje_creditos ='0' then 'NO SIGUE SISTEMA DE CREDITOS'
WHEN sb.estu_pje_creditos ='1' then 'MENOS DEL 75%'
WHEN sb.estu_pje_creditos ='2' then 'ENTRE 75% Y 80%'
WHEN sb.estu_pje_creditos ='3' then 'ENTRE 81% Y 90%'
WHEN sb.estu_pje_creditos ='4' then 'MAS DE 90%'
END as estu_pje_creditos ,
ca.caracter_academico_nombre as estu_titulo_bto,
tipo_financiacion_nombre as estu_financiacion_matricula,
estrato_nombre as estu_estrato,
nivel_sisben_nombre as fami_nivel_sisben,
CASE
when material_pisos_codigo in(1,2) then 'MALA'
when material_pisos_codigo =3 then 'REGULAR'
ELSE
'BUENA'
END as econ_condicion_vivienda,
CASE
WHEN    cast (sb.econ_sn_dvd as smallint)+cast(sb.econ_sn_lavadora as smallint)+
cast(sb.econ_sn_microondas as smallint)+cast(sb.econ_sn_horno as smallint)+
cast(sb.econ_sn_nevera as smallint) in (0,1) then 'CONDICION VIVIENDA MALA'
WHEN 	cast (sb.econ_sn_dvd as smallint)+cast(sb.econ_sn_lavadora as smallint)+
cast(sb.econ_sn_microondas as smallint)+cast(sb.econ_sn_horno as smallint)+
cast(sb.econ_sn_nevera as smallint) in(2,3) then 'CONDICION VIVIENDA REGULAR'
WHEN	cast (sb.econ_sn_dvd as smallint)+cast(sb.econ_sn_lavadora as smallint)+
cast(sb.econ_sn_microondas as smallint)+cast(sb.econ_sn_horno as smallint)+
cast(sb.econ_sn_nevera as smallint) in(4,5) then 'CONDICION VIVIENDA BUENA'
END as eco_condicion_hogar,
CASE
WHEN
sb.econ_sn_automovil='0' then 'CONDICION TRANSPORTE PUBLICO'
WHEN
sb.econ_sn_automovil='1' then 'CONDICION TRANSPORTE PARTICULAR'
END as eco_condicion_transporte,
CASE
WHEN    cast(sb.econ_sn_internet as smallint)+cast(sb.econ_sn_servicio_tv as smallint)+
cast(sb.econ_sn_telefonia as smallint)+cast(sb.econ_sn_celular as smallint)+
cast(sb.econ_sn_computador as smallint ) in(0,1) then 'CONDICION HOGAR MALA'
WHEN    cast(sb.econ_sn_internet as  smallint)+cast(sb.econ_sn_servicio_tv as  smallint)+
cast(sb.econ_sn_telefonia as smallint)+cast(sb.econ_sn_celular as smallint)+
cast(sb.econ_sn_computador as smallint) in(2,3) then 'CONDICION HOGAR REGULAR'
WHEN	cast(sb.econ_sn_internet as  smallint)+cast(sb.econ_sn_servicio_tv as  smallint)+
cast(sb.econ_sn_telefonia as  smallint)+cast(sb.econ_sn_celular as  smallint)+
cast(sb.econ_sn_computador as smallint) >=4 then  'CONDICION HOGAR BUENA'
END as eco_condicion_tic,
CASE
WHEN cast(sb.fami_num_pers_grup_fam as integer)/cast(sb.infa_dormitorios as integer) <2.5 then 'SIN HACINAMIENTO'
WHEN cast(sb.fami_num_pers_grup_fam as integer)/cast(sb.infa_dormitorios as integer)>=2.5 
and cast(sb.fami_num_pers_grup_fam as integer)/cast(sb.infa_dormitorios as integer)<5 then 'HACINAMIENTO MEDIO'
WHEN cast(sb.fami_num_pers_grup_fam as integer)/cast(sb.infa_dormitorios as integer)>=5 then 'HACINAMIENTO CRITICO'
END as eco_condicion_vive,
CASE
WHEN sb.fami_ing_fmliar_mensual='1' Then 'UN SALARIO'
WHEN sb.fami_ing_fmliar_mensual='2' Then 'DOS SALARIOS'
WHEN sb.fami_ing_fmliar_mensual='3' Then 'TRES SALARIOS'
WHEN sb.fami_ing_fmliar_mensual='4' Then 'CUATRO SALARIOS'
WHEN sb.fami_ing_fmliar_mensual='5' Then 'CINCO SALARIOS'
WHEN sb.fami_ing_fmliar_mensual='6' Then 'SEIS SALARIOS'
WHEN sb.fami_ing_fmliar_mensual='7' Then 'SIETE SALARIOS'
END as fami_ing_fmliar_mensual,
trabaja_actualmente_nombre as estu_trabaja,
zon.num_estu_zona,
zon.num_instituciones_zona
from 
saber_pro_finales sf 
join 
estado_civil
on 
(estu_estado_civil=estado_civil_id)
join
hogar_actual
on
(estu_hogar_actual=hogar_actual_codigo)
join
nivel_educativo
on
(fami_educa_padres=nivel_educativo_codigo)
join
ocupaciones p
on
(fami_cod_ocup_padre=p.ocupacion_codigo)
join 	
ocupaciones m
on
(fami_cod_ocup_madre=m.ocupacion_codigo)
join
tipo_institucion
on
(tipo_institucion_codigo=inst_tipo)
join
caracter_academico
on
(caracter_academico_codigo=inst_caracter_academico)
join
municipios mun
on
(inst_mpio_programa=mun.municipio_codigo)
join
metodo_programa
on
(estu_metodo_prgm=metodo_programa_codigo)
join
areas_conocimiento
on
(estu_area_conoc=area_conocimiento_codigo)
join
grupos_referencia
on(grupo_referencia_codigo=estu_grupo_referencia)
join
saberpro_temp sb
on
(sb.estuconsecutivo=sf.estu_consecutivo)
join
caracter_academico ca
on
(sf.estu_titulo_bto=ca.caracter_academico_codigo)
join
tipos_financiacion
on
(estu_financiacion_matricula=tipo_financiacion_codigo)
join
estratos
on
(sf.estu_estrato=estrato_codigo)
join
niveles_sisben
on
(nivel_sisben_codigo=sf.estu_nivel_sisben)
join
material_pisos
on
(material_pisos_codigo=sf.econ_material_pisos)
join
trabaja_actualmente
on
(trabaja_actualmente_codigo=cast(sb.estu_trabaja as smallint))
join 
departamentos dep
on
(dep.departamento_codigo=mun.departamento)
join
zonas zon
on
(zon.zona_codigo=dep.departamento_zona) 
join
db_icfes db
on(sf.estu_consecutivo=db.estuconsecutivo);"

icfes <- dbGetQuery(con, query)

icfes$num_estu_zona <- cut(
  icfes$num_estu_zona,
  breaks = c(0, 10000, 20001, max(icfes$num_estu_zona) +
               1),
  labels = c('Baja', 'Media', 'Alta')
)

icfes$num_instituciones_zona <- cut(
  icfes$num_instituciones_zona,
  breaks = c(0, 60, 71, max(icfes$num_instituciones_zona) +
               1),
  labels = c('Baja', 'Media', 'Alta')
)

icfes <- icfes[complete.cases(icfes)==TRUE,]
write.csv2(icfes, "datasetOriginal.csv", row.names=FALSE)

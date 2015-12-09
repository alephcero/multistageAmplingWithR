#PRIMERA ETAPA

#los 64 radios se dividen en: 26 en estrato 1, 30 en el estrato 2 y 8 en el estrato 3

#6 conglomerados en el estrato 1
#9 en el estrato 2
#3 en el estrato 3. 
#Como consecuencia la muestra final de primer etapa a 18 conglomerados de los 64.

rm(list=ls())

library(sampling)
library(survey)
library(dplyr)

# a) probabilidades de primer etapa
data = read.csv('radiosACT2.csv')
data = data[order(data$estrato),] 



m1 = 26
m2 = 30
m3 = 8

m1 = 6
m2 = 9
m3 = 3

pii1 = n1/m1
pii2 = n2/m2
pii3 = n3/m3


# Las de segundo orden: pagina 9 MPF 3FEB 2015 MEstratificado.pdf. 
#Calculo solo para dentro de cada estrato, 

piij1 = pii1 * (n1-1) / (m1-1)
piij2 = pii2 * (n2-1) / (m2-1)
piij3 = pii3 * (n3-1) / (m3-1)



#b) Seleccionar la muestra estratificada de 1er etapa
muestraResumen = strata(data = data, stratanames = 'estrato',size = c(n1,n2,n3), method = 'srswor', description = T)
muestraResumen = getdata(data, muestraResumen)


#SEGUNDA ETAPA
#90 viviendas estrato 1
#50 viviendas estrato 2
#80 viviendas estrato 3

# Muestra final de viviendas de tamaño igual a 1230 para la localidad:
# 540 (6*90), 450 (9*50) y 240 (3*80) viviendas por estrato respectivamente

#c) 

muestraResumen$vivSel = ifelse(muestraResumen$estrato==1, 90,
                       ifelse(muestraResumen$estrato==2,50,80)
                       )

#Probabilidad de las viviendas de ser seleccionadas dentro de cada conglomerado
muestraResumen$pviv = muestraResumen$vivSel / muestraResumen$tviv

#Probabilidad final de ser incluida esa vivienda en la muestra
muestraResumen$pikhi = muestraResumen$pviv * muestraResumen$Prob

#Pesos Inversa de la Probabilidad final de ser incluida esa vivienda en la muestra
muestraResumen$w = 1/muestraResumen$pikhi

muestraResumen$tstra = ifelse(muestraResumen$estrato==1, 26,
                              ifelse(muestraResumen$estrato==2,30,8)
)


#d) probabilidades de una vivienda del estrato 3
# pertenecen al mismo conglomerado o no? 
# son 3 nada mas asi que se puede armar una tabla con todas las combinaciones
# Las probabilidades de inclusion (de primer y segundo orden) de una vivienda del estrato 3 dependen
# del conglomerado en el que la vivienda se incluya.
# Tomamos la muestra seleccionada solo para el estrato 3

muestraResumenEst3 = muestraResumen[muestraResumen$estrato == 3,]

# De esta manera la probabilidad de inclusion de primer orden final de una vivienda en el estrato 3 
# queda  indicada en la columna pikhi


#Para la primera etapa, dado que se seleccionan siempre 3 conglomerados de 8 posibles

m3 = 8

n3 = 3

pii3 = n3/m3


# Para la segunda etapa para las de primer orden, es necesario tener en cuenta el total de viviendas 
# en cada conglomerado. 

muestraResumenEst3[,1:4]

# De este modo, las probabilidades de inclusion de primer orden para la segunda etapa
# para una vivienda del conglomerado 1 sera igual a 
# la cantidad de viviendas a seleccionar sobre el total de viviendas en el conglomerado 1
# esta operacion puede repetirse de manera vectorial y obtener ese vector
muestraResumenEst3$vivSel / muestraResumenEst3$tviv

# De este modo la probabilidad de inclusion final de primer orden se obtiene vetorialmente
# calculando la probabilidad conjunta y queda resumida en el vector pikhi


#Para las probabilidades de inclusion de segundo orden, tambien es necesario saber
# el conglomerado al que pertenecen cada vivienda

# Para las de primer tapa, en la medida en que siempre se selecciona 3 conglomerados de 8
# esto no varía y las probabilidades de segundo orden etsan dadas por:

piij3 = pii3 * (n3-1) / (m3-1)

# Pero para las de segunda etapa es necesario considerar el conglomerado con el cual 
# se empareja la vivienda del. estrato 3. Por haber solo 3 conglomerados en el estrato 3
# las combinaciones pueden ser solo 
# Para el primer caso

congComb = combn(3,2)
congComb = t(cbind(congComb,rep(1,2),rep(2,2),rep(3,2)))
congComb = as.data.frame(congComb)
names(congComb) = c('i','j')


congComb$tviv.i = NA
congComb$tviv.j = NA
for(i in 1:nrow(congComb)){
  congComb$tviv.i[i]=muestraResumenEst3$tviv[congComb$i[i]]
  if(congComb$j[i]==congComb$i[i]){
    congComb$tviv.j[i]=congComb$tviv.i[i]-1
  }else{
    congComb$tviv.j[i]=muestraResumenEst3$tviv[congComb$j[i]]
  }
}

#segundo orden Primer etapa
congComb$pSegOrd.PrimEtap = muestraResumenEst3$Prob[1]

#segunda etapa
congComb$pSegOrd.SegEtap = 80/congComb$tviv.i * 80 / congComb$tviv.j

#finales
congComb$pSegOrd = congComb$pSegOrd.PrimEtap * congComb$pSegOrd.SegEtap

#2

#Adapto resumen para poder mergear con la otra tabla con individuos
muestraResumen = 
  muestraResumen %>% 
  group_by(estrato) %>% 
  mutate(Cong = row_number()) %>%
  select(estrato,Cong,radio,tviv,tstra,Prob,vivSel,pviv,pikhi,w) 

muestra = read.csv('muestraACT2.csv')
names(muestra)[1]='estrato'


muestra = merge(muestra,muestraResumen,all.x = T,by = c('estrato','Cong'))


muestraEstim = svydesign(id=~Cong,#vector de clusters
                         strata = ~estrato,
                         probs = ~Prob + pviv,
                         #weights = ~w,
                         data=muestra, #tabla de datos
                         nest=TRUE) #el id de los cluster no es unico, anidados para cada estrato


#Tengo que usar los probs como parametros. peor cuales? Prob, pviv o pikhi
## Prob es probabilidad de primer orden de la PRIMERA etapa
## pviv es probabilidad de primer orden de la SEGUNDA etapa

#2.A
#la proporción de individuos con cobertura en salud por sexo, un coeficiente de
#variación (CV) para cada estimación y el efecto de diseño,

tablaCobSalud = svyby(~Salud,~Sexo, muestraEstim, svymean, keep.var=F,deff = TRUE)
tablaCobSalud$CV = round(cv(svyby(~Salud,~Sexo, muestraEstim, svymean))*100,2)

muestraEstim$variables$Salud2 = muestraEstim$variables$Salud * (muestraEstim$variables$Edad>55)

tablaCobSalud2 = svyby(~Salud2,~Sexo, muestraEstim, svymean, keep.var=F,deff = TRUE)
tablaCobSalud2$CV = round(cv(svyby(~Salud2,~Sexo, muestraEstim, svymean))*100,2)

muestraEstim$variables$Desocup = as.numeric(muestraEstim$variables$Ocup != 1)

svytotal(~PEA,muestraEstim,deff=TRUE)
svytotal(~Desocup,muestraEstim,deff=TRUE)

svyratio(~Desocup,denominator = ~ PEA == 1,muestraEstim,deff=TRUE)

surtq <-
  svyby(
    ~ Desocup ,
    by = ~ Sexo ,
    denominator = ~ PEA == 1 ,
    design = muestraEstim ,
    na.rm = TRUE ,
    svyratio
  )
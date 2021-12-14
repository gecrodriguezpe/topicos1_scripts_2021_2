# Laboratorio tópicos de econometría 1: Modelos multinomiales ----

# Modelos a tratar: 
## Multinomiales 
## Multinomiales ordenados
## Modelos logit condicionados 

# Paquetes para usar: 
library(nnet) # Paquete base para calcular modelos multinomiales en R
library(MCMCpack) # Paquete para calcular modelos logit condicionados y modelos logit ordenados
# Paquetes adicionales: 
library(car)
library(broom)
library(tidyverse)
library(readxl)
library(lmtest)
library(sandwich)
library(forecast)
library(knitr)    # Para utilizar la función kable

# Defino el directorio de trabajo:
setwd("~/GitHub/Additional_topics/topicos1_scripts_2021_2/modelos_multinomiales")

# Nota: Otros paquetes que pueden ser utilizados para realizar 
#       modelos multinomiales en R son: 
### survival 
### mclogit: Provee estimadores para modelos logit multinomiales 
### Zelig: Paquete muchísimo más general que depende del paquete survival
###        e importa entre muchos paquetes el paquete MCMCpack

# Nota: Se recomienda que revisen CRAN para tener mayor familiaridad
#       de cada uno de los paquetes que les acabamos de mencionar 


# Ejemplo 1: Logit multinomial usando la función multinom del paquete nnet ----

# Nota: La función multinom del paquete nnet permite estimar logit
#       multinomiales

# Para el presente ejemplo se va a utilizar la base de datos
# nels_small del libro Pinciple of economics 

# Nota: Para mayor referencia sobre el 
#       libro Principles of Econometrics de Hill, Griffiths y Lim
#       (El cual es una excelente alternativa al Wooldridge y al Stock y Watson)
#       consultar: http://www.principlesofeconometrics.com/poe4/poe4.htm

# La base de datos a emplea se obtuvo directamente de: http://www.principlesofeconometrics.com/poe4/poe4excel.htm

# Variables de interés: 
## grades: Es un índice de notas de secundaria, donde menores valores
##         indican mejores notas (variable numérica)
## psechoice: Variable que indica la elección de educación 
##            post-secundaria (variable categórica con 3 categorias)
## Categorias de psechoice (Cada individuo escoge solo 1 de ´las 3 categorias)
### psechoice = 1:  Ningún tipo de educación post-secundaria
### psechoice = 2:  2 años de educación post-secundaria (nivel técnico)
### psechoice = 3:  4 años de educación post-secundaria (nivel universitario)

# Importación y visualización de la base de datos
nels_small = read_xlsx("nels_small.xlsx")
glimpse(nels_small)

# Para ilustrar como utilizar el comando multinom realizaremos un 
# modelo sencillo que explique la elección de educación 
# post-secundaria de las personas basada en las notas de 
# secundaria

# Estimación de un modelo logit multinomial 
nels.multinom = multinom(PSECHOICE ~ GRADES, data = nels_small)

summary(nels.multinom)

# Como se puede observar del summary anterior, los resultados 
# de la función multinom da los coeficientes estimados
# para cada nivel de la variable de respuesta psechoice 
# A excepción de la primer categoría (1: Ningún tipo de educación post-secundaria) 
# que se toma como nivel de referencia y es la categoría base con la cuál
# se analizan los coeficientes del modelo 
# (Nota: la categoria base se puede modificar)

# Ahora bien, el siguiente código ilustra como encontrar 
# la probabilidad predicha por el modelo para cada categoría 
# utilizando las notas de secundaria de dos individuos diferentes: 
## 1. Para el primer estudiante sus notas de secundaria se encuentran en la mediana de las notas de la muestra
## 2. Para el segundo estudiante sus notas de secundaria se encuentran en el 5 % más alto

notas_estudiante1 = median(nels_small$GRADES) # Estudiante cuyas notas de secundaria se encuentran sobre la media de notas
notas_estudiante2 = quantile(nels_small$GRADES, 0.05) # Estudiante cuyas notas de secundaria se encuentra en el 5 % más alto de la distribución de notas

notas_estudiantes = data.frame(GRADES=c(notas_estudiante1, notas_estudiante2))
prediccion_notas = predict(nels.multinom, notas_estudiantes, "probs"); prediccion_notas

# Se puede observar lo siguiente:
## Para el estudiante 1 cuyas notas de secundaria se encuentran sobre
## la media de las notas de la muestra con una probabilidad apróximandamente
## del 20 % no cursara ninguna carrera ni técnica ni universitaria
## y con una probabilidad del 30 % cursara una carrera técnica

## Para el estudiante 2 cuyas notas de secundaria se encuentran sobre
## el quantil del 5 % de las notas más altas con una probabilidad casi 
## del 90% dicho estudiante cursara una carrera universitaria
## y con una probabilidad del 10 % cursara una carrera técnica

# Lo anterior se podría repetir para cualquier estudiante que 
# haga parte de la muestra y conocer basado en sus notas de 
# secundaria cuál es la probabilidad de

# De igual forma, el anterior modelo se puede extender para incluir
# otras variables de la base de datos como lo son: FEMALE o BLACK 
# Nota: Se sugiere que como ejercicio de aprendizaje lo hagan 

# Ejemplo 2: Modelo Logit condicional usando la función MCMCmnl del paquete MCMCpack ----

# Para estimar modelos logit condicionales en R se pueden utilizar
# 3 funciones distintas: 

# La función MCMCmnl del paquete MCMCpack
# La función clogit del paquete survival
# La función mclogit del paquete mclogit

# En nuestro caso, vamos a utilizar la función MCMCmnl

# Ahora bien, nosotros sabemos que en un modelo logit multinomial
# todos los individuos enfrentan las mismas condiciones externas
# y la elección de cada individuo se determina exclusivamente por
# las preferencias de cada individuo. Por el contario, un modelo
# logit condicional permite a los individuos enfrentar condiciones
# externas específicas a ellos. Es decir, en un logit multinomial 
# solo se observa para todos los individuos un solo valor de la 
# variable independiente para todas las categorias mientras 
# que en un logit condicional, para cada individuo se observa 
# valores diferentes de la variable independiente para cada
# categoría de la variable dependiente

# Nota: A diferencia de un modelo logit multinomial, el coeficiente 
#       de la variable independiente es el mismo para todas las 
#       categorias de la variable dependiente, pero para cada
#       individuo va a haber un valor diferente de la variable
#       independiente por cada categoria de la variable dependiente

# Nota: Sobre la base de datos para un modelo logit condicional

# La base de datos para un modelo logit condicional se tiene
# que establecer de una manera muy particular. Por cada individuo
# se tiene que colocar en filas adyacentes el valor que asume éste 
# para cada categoria de la variable categórica dependiente
# (ésto, es muy similar a como se estructura una base de datos tipo 
# panel donde se colocan en filas adyacentes las observaciones de 
# todos los años para cada individuo, solo que acá en lugar de años
# tenemos los valores que asuimiria el individuo i para cada categoria
# j). Adicionalmente, en la variable categórica dependiente se le 
# Asigna un valor de 1 en la la fila del individuo i donde se encuentra
# la categoría j que selecionó. 

# En esta etapa, por favor ejecute el comando glimpse(colas)
# y visualice la base de datos mientras vuelve a leer la parrafo 
# anterior para que tenga más sentido lo que se acaba de mencionar
# y se comprenda mejor la estructura de una base de datos para
# ejecutar un modelo logit condicional

# Importación y visualización de la base de datos
cola = read_xlsx("cola.xlsx")
glimpse(cola)

# La variable categórica dependiente tiene las siguientes
# categorias para cada individuo: (Cada individuo escoge solo 
# una de éstas categorias)

# 1. pepsi
# 2. sevenup
# 3. coke


# Para entener las base de datos, cada individuo tiene 3 filas
# adyacentes: 
# En la primera fila de cada individuo está el valor que asumiria
# en la categoría pepsi, en la segunda fila para dicho individuo
# el valor que asuiría en la categoría sevenup y en la tercera
# fila pra dicho individuo el valor que asuiria en la categoría coke

# Nota: La variable ID se utiliza para identificar a cada individuo 

# Para este ejercicio se busca establecer que gaseosa 
# escogería cada individuo dado el precio al que éste se enfrenta
# (recordar que cada individuo podría estar enfrentando un precio
# de gaseosa diferente para cada tipo de gaseosa, cosa que no puede
# pasar en un logit multinomial donde todos los individuos, enfrentan
# el mismo precio para cada una de las categorias de la variable dependiente)

# Lo primero es aislar los precios para cada categoria de gasesoa
N <- nrow(cola) # Tamaño del Data Frame
N3 <- N/3       # Número de individuos diferentes
price1 = cola$PRICE[seq(1,N,by=3)] # Extraigo los precios de pepsi para todos los individuos
price2 = cola$PRICE[seq(2,N,by=3)] # Extraigo los precios de sevenup para todos los individuos
price3 = cola$PRICE[seq(3,N,by=3)] # Extraigo los precios de coke para todos los individuos

# Se construye la variable dependiente que se va a utilizar para 
# modelar el logit condicional

# bchoice es la variable categórica dependiente que asume los valores:
## 1. pepsi
## 2. sevenup
## 3. coke

# Se inicializa la variable son solo 1 (para denotar la categoría 1)
bchoice <- rep("1", N3)
# No obstante, se utiliza la el ciclo y el condicional 
# para colocar la categoría correcta para cada individuo. 
# (Por ejemplo si el individuo i escogió la categoría 2
# entonces el if statemente asociada a esa condición lógica se
# activa y bchoice pasa de tener un valor de 1 a un valor de 2)
for (i in 1:N3){
  if(cola$CHOICE[3*i-1]==1) bchoice[i] = "2"
  if(cola$CHOICE[3*i]==1) bchoice[i] = "3"
}

# Se estima el logit multinomial 
cola.clogit <- MCMCmnl(bchoice ~
                         choicevar(price1, "b2", "1")+
                         choicevar(price2, "b2", "2")+
                         choicevar(price3, "b2", "3"),
                       baseline="3", mcmc.method="IndMH")

# Nota: Sobre el comando

# choicevar(var, varname, choicelevel)
## var: La varaible que se va a usar
## varname: El nombre de la nueva variable que se va a crear
## choicelevel: La categoría asociada de la variable categórica dependiente

# MCMCmnl(formula, baseline, mcmc.method): 
## formula: formula para la estimación
## baseline: Categoria base 
## mcmc.method: metodo de integración/estimación por MCMC

# Nota: El paquete MCMC estima los modelos multinomiales lógisticos
#       utilizando Monte Carlo Markov Chain (MCMC). Les recomiendo
#       que le hechen una ojeada a qué es eso

# Nota: La siguiente tabla muestra los coeficientes estimados
#       del logit condicional. 
#       Tener en cuenta que b2 es el parámetro que acompaña a la
#       variable independiente, mientras que b11 es el intercepto
#       asociada a la categoría 1, b12 es el intercepto asociado
#       a la cateogria 2 y b13 = 0 porque se tomo la categoría 3
#       como categoría base. 

sclogit <- summary(cola.clogit); sclogit
tabMCMC <- as.data.frame(sclogit$statistics)[,1:2]
row.names(tabMCMC)<- c("b2","b11","b12")
kable(tabMCMC, digits=4, align="c",
      caption="Coeficientes estimados para el logit condicional")

# Finalmente se quiere ver las probabilidades de que un individuo
# escoga una determianda gaseosa dado un conjunto de precios de las
# gaseosas que éste individuo tiene que enfrentar

# Precios de las gasesosas que enfrenta el individuo i: 
pPepsi = 1
pSevenup = 1.25
pCoke = 1.10

# Coeficientes estimados para el logit condicional: 
b13 = 0
b2  = tabMCMC$Mean[1]
b11 = tabMCMC$Mean[2]
b12 = tabMCMC$Mean[3]

# Usando la definición de probabilidad correspondiente a 
# un modelo logit condicional se obtiene que la probabilidad
# asociada a cada categoria es: 

# Probabilidad que el individuo i escoga pespi: 
PiPepsi = exp(b11+b2*pPepsi)/
  (exp(b11+b2*pPepsi)+exp(b12+b2*pSevenup)+
     exp(b13+b2*pCoke))

# Probabilidad que el individuo i escoga sevenup: 
PiSevenup = exp(b12+b2*pSevenup)/
  (exp(b11+b2*pPepsi)+exp(b12+b2*pSevenup)+
     exp(b13+b2*pCoke))

# Probabilidad que el individuo i escoga coke: 
PiCoke = 1-PiPepsi-PiSevenup

# Probabilidades son: 
PiPepsi
PiSevenup
PiCoke

# Las probabilidades van a cambiar para cada individuo, dado que 
# cada individuo tendrá que enfrentar posiblemente precios 
# diferentes para cada tipo de gaseosa

# Adicionalmente, es posible extener el modelo incluyendo otros
# regresores además del precio, por ejemplo, regresores que 
# reflejen características individuales 

# Ejemplo 3: Modelo Probit ordenado usando la función MCMCoprobit del paquete MCMCpack ----

# En un modelo probit ordenado el orden de las elecciones es importante
# a diferencia de un modelo logit multinomial o un modelo logit
# condicional. 

# Para un modelo ordenado es posible estimar: 
### probit ordenado: Los errores siguen una distribución normal estándar 
### logit ordenado: Los errores siguen una distribución logit estándar 

# Para el siguiente ejemplo se utilizara la base de datos "nesl_small"
# la misma que se utilizó en el ejemplo 1, y lo que se busca es 
# estimar un modelo probit ordenado. 

# Al ser un modelo probit ordenado, las categorias de la variable
# categórica dependiente están ordenadas. 

## grades: Es un índice de notas de secundaria, donde menores valores
##         indican mejores notas (variable numérica)
## psechoice: Variable que indica la elección de educación 
##            post-secundaria (variable categórica con 3 categorias)
## Categorias de psechoice (Cada individuo escoge solo 1 de ´las 3 categorias)
### psechoice = 1:  Ningún tipo de educación post-secundaria
### psechoice = 2:  2 años de educación post-secundaria (nivel técnico)
### psechoice = 3:  4 años de educación post-secundaria (nivel universitario)

# Para un modelo probit ordenado, siempre se parte de una ecuación latente: 

# y_i* = \beta GRADES_i + e_i
# Donde la variable y_i (variable categórica discreta con 3 categorias
# asume una determianda categoria si la variable latente supera
# un nivel determinado nivel o threshold)

# y_i = 3 (4 años de educación post-secundaria) si y_i* > mu_2
# y_i = 2 (2 años de educación post-secundaria) si mu_2 > y_i* > mu_1
# y_i = 1 (1 sin edudación post-secundaria) si mu_1 > y_i*

# Donde mu_1 es el threshold para pasar de no tener educación post-secundaria
# a tener 2 años de educación post-secundaria y mu_2 es el threshold para pasar
# de tener 2 años de educación secundaria a tener 4 años de educación secundaria

# Se estima el modelo probit ordenado
# Un modelo probit ordenado se estima maximizando una función de 
# maxima verosimilitud numéricamente (para ello el comando emplea
# MCMC)
nels.oprobit <- MCMCoprobit(PSECHOICE ~ GRADES, 
                            data=nels_small, mcmc=10000) #mcmc es el número de itreaciones MCMC 

sOprobit <- summary(nels.oprobit) 
tabOprobit <- sOprobit$statistics[, 1:2]

# La siguiente tabla da los coeficientes asociados al modelo
# probit ordenado estimado
kable(tabOprobit, digits=4, align="c",
      caption="Ordered probit estimates for the 'nels' problem")

# No obstante, dichos parámetros no se pueden interpretar 
# directamente como los parámetros mu_1, mu_2 y \beta descritos
# anteriormente. Para ello, hay que hacer una transformación
# a los coeficientes de la tabal anterior para encontrar
# los valores de los umbrales (mu_1 y mu_2) y del parámetros \beta
# que acompaña a la variable GRAES en la ecuación latente

intercept = tabOprobit[1,1]
GRADES = tabOprobit[2,1]
gamma2 = tabOprobit[3,1]
  
# Los valores para mu_1, mu_2 y beta transformando los coeficientes
# encontrados por el modelo: 

mu1 = - intercept
beta = GRADES
mu2 = gamma2 - intercept
  
# Ahora bien, se van a extraer dos estudiantes de la muestra. 
# Un estudiante, cuyas notas de secundaria están en la media de la
# muestra y otro estudiante cuyas notas están en el cuántil más alto
# de la muestra

xGrade = c(mean(nels_small$GRADES), 
           quantile(nels_small$GRADES, 0.05))

# Las probabilidades de que cada uno de los estudiantes esté 
# en una determinado categoría de educación post-secundaria son:

# Categoría1: Sin educación post-secundaria
prob1 = pnorm(mu1 - beta * xGrade); prob1
# Categoría2: 2 años de educación post-secundaria
prob2 = pnorm(mu2 - beta * xGrade)-pnorm(mu1 - beta * xGrade); prob2
# Categoría3: 4 años de educación post-secundaria
prob3 = 1 - pnorm(mu2 - beta * xGrade); prob3

# Los efectos marginales de que cada uno de los estudiantes
# para cada categoría son: 

# Categoría1: Sin educación post-secundaria
Dp1DGrades = -pnorm(mu1 - beta * xGrade) * beta; Dp1DGrades
# Categoría2: 2 años de educación post-secundaria
Dp2DGrades = (pnorm(mu1- beta * xGrade)-pnorm(mu2 - beta * xGrade)) * beta; Dp2DGrades
# Categoría3: 4 años de educación post-secundaria
Dp3DGrades = pnorm(mu2 - beta * xGrade) * beta; Dp3DGrades

# Por tanto, el efecto marginal de las notas de secundaria
# en la probabiliad de atender 4 años de educación post-secundaria es: 
# −0.143  para el estudiante con notas en la media y −0.031 para
# el estudiante con las notas en el 5 % más alto de la muestra

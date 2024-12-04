source("TDecisionCodigo/teoriadecision_funciones_multicriterio.R")
source("TDecisionCodigo/teoriadecision_funciones_multicriterio_diagram.R")
source("TDecisionCodigo/teoriadecision_funciones_multicriterio_utiles.R")

## Datos del Problema
  
  # | **Fornitore** | **Costo (€ per tonnellata)** | **Affidabilità (% ordini puntuali)** | **Tempi di consegna (giorni)** | **Impatto ambientale (kg CO₂/ton)** |
  # |---------------|-----------------------------|--------------------------------------|--------------------------------|-------------------------------------|
  # | **A**         | 850                         | 95%                                  | 7                              | 180                                 |
  # | **B**         | 820                         | 92%                                  | 10                             | 200                                 |
  # | **C**         | 880                         | 98%                                  | 5                              | 160                                 |
  # | **D**         | 900                         | 90%                                  | 8                              | 150                                 |
  

## Fonti dei Dati
  
#1. Procurement Resource: Dettagli sui costi e sostenibilità ambientale (https://www.procurementresource.com/)
#2. MEPS International: Prezzi aggiornati dell'acciaio in Italia (https://mepsinternational.com/)
#3. European Steel Review: Analisi logistiche e dati di settore (https://mepsinternational.com/)

tab<-multicriterio.crea.matrizdecision(c(850,0.95,7,180,820,0.92,10,200,880,0.98,5,160,900,0.9,8,150),
                                       4,4,c("A","B","C","D"),c("Costo","Affidabilità","Tempi di consegna","Impatto ambientale"))
tab
tab2<-multicriterio.homogeneizacion.nadir(tab)
tab2
#-----Metodo AHP-----

#comparaciones:
#* Costo<-2 por Affidabilità; Costo<-3 por Tempi di consegna; Costo<-4 por Impatto Ambientale
#* Affidabilità<-1.5 por Tempi di consegna; Affidabilità<- 2 por Impatto Ambientale
#* Tempi di consegna<- 4/3 por Impatto Ambientale
tab3<-multicriterio.crea.matrizvaloraciones_mej(c(2,3,4,1.5,2,4/3),4,c("C1","C2","C3","C4"))
tab3
inc<-multicriterio.metodoAHP.coef.inconsistencia(tab3)

pesos<-multicriterio.metodoAHP.variante3.basico(tab3)
pesos_criterios<-pesos$valoraciones.ahp
rank<-tab2%*%pesos_criterios
rank
#El metodo AHP nos sugiere el provedoor C

##Metodo ELECTREE

#Como pesos utilizo los siguientes:
#* Costo<-0.4
#* Affidabilita<-0.3
#* Tempo di consegna<-0.2
#* Impatto Ambientale<-0.1

install.packages("qgraph")
library(qgraph)
pesos<-c(0.4,0.3,0.2,0.1)
alpha<-0.7
no_se_compensan<-c(20,0.03,2,10)
el1<-multicriterio.metodoELECTRE_I(tab2,pesos,alpha,no_se_compensan)
el<-func_ELECTRE_Completo(el1)
qgraph(el$Grafo)
alpha<-0.55
el2<-multicriterio.metodoELECTRE_I(tab2,pesos,alpha,no_se_compensan)
el<-func_ELECTRE_Completo(el2)
qgraph(el$Grafo)

#El metodo Electree I nos sugiere el provedoor C, como con el primer metodo

##Metodo Promethee

pesos<-c(0.4,0.3,0.2,0.1)
tab_pref<-matrix(c(3,10,15,0,
                   5,0.02,0.05,0,
                   3,1,3,0,
                   3,5,10,0),ncol=4,byrow=T)
promethee_1<-multicriterio.metodo.promethee_i(tab2,pesos,tab_pref)
library(qgraph)
qgraph::qgraph(promethee_1$tablarelacionsupera)

multicriterio.metodo.promethee_ii(tab2,pesos,tab_pref)

#Ambos metodos Promethee (I y II) nos sugieren que el provedoor mejor es el C
#En conclusion, se puede decir que el provedoor C es lo que elegimos,
#porquè resulta el mejor en todos los metodos multicriterios utilizados
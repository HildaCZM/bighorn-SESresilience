# bighorn-SESresilience
#install.packages('deSolve')
library("deSolve")

management.bighorn <- function(t, state, parameters) {
  with(as.list(c(state,parameters)), {
# Auxiliary Variables
    Observation.Rate.Bighorn.BCS<-Base.Observation.Rate.Bighorn.BCS # [bighorn sheep/year]ø
    Pop.Observed.Males.Bighorn.BCS<-Observation.Rate.Bighorn.BCS*Male.Proportion*Pop.Bighorn.BCS #[bighorn sheep]ø
    General.Use.Rate.BCS<-(Pop.Observed.Males.Bighorn.BCS/Base.Observation.Rate.Bighorn.BCS)*Percentage.Use.BCS #[bighorn sheep/year]ø
    Presence.Bighorn.Ejido<-Pop.Bighorn.Ejido/Area.Ejido # [bighorn sheep/km2x1000]ø
    Presence.Change.Bighorn.Ejido<-Presence.Bighorn.Ejido/Presence.Bighorn.Ejido.Initial#[%]
    Effect.Bighorn.Presence.Change.Ejido.at.Sighting<-approx(c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,100),
                                                              c(0.0,0.2,0.4,0.6,0.8,1.0,1.2,1.4,1.6,1.8,2.0,2.0), xout =Presence.Change.Bighorn.Ejido)$y # [%]
                                                              # x= presence change, where 1 is the original presence or base presence
                                                              # y= probability of sighting. The effect is lineal
    Sighting.Bighorn.Ejido<-Sighting.Bighorn.Ejido.Base*Effect.Bighorn.Presence.Change.Ejido.at.Sighting #[bighorn sheep/hr.flight]
#Model Permits Demand
    Price.Other.Permits<-90
    Price.Permit.Relative<-Price.Other.Permits/Price.Permit.Ejido #ø
    Demand.Permit.Ejido<-Beta0+Beta1*Sustainable.Management+Beta2*Status.Protection+Beta3*Sighting.Bighorn.Ejido+Beta4*Price.Permit.Relative # [bighorn sheep/year]ø
#Variables de flujo del ejido
    No.Permit.Ejido<-ifelse(Pop.Bighorn.Ejido>General.Use.Rate.BCS*(Proportion.Permits.Ejido),General.Use.Rate.BCS*(Proportion.Permits.Ejido),Pop.Bighorn.Ejido) #[bighorn sheep/year]ø
   #Supply.Permit.Ejido<-Funcion(Precio.Cintillo*No.Permit.Ejido-Costos.Operacion)
    Supply.Permit.Ejido<-No.Permit.Ejido # [bighorn sheep/year]
    Permisos.Ejercidos.Ejido<-min(Demand.Permit.Ejido,Supply.Permit.Ejido) # [bighorn sheep/year]
    Ingresos.por.Permisos<-Permisos.Ejercidos.Ejido*Price.Permit.Ejido  # [usdx1000/year]
    Costo.Manejo.del.Habitat<-Gasto.UMA.Guia+Gasto.UMA.Vigilancia+Gasto.UMA.Manteminto.Habitat #[usdx1000/year]
    Costos.Operacion.UMA<-Gasto.Asistencia.Convencion+Costo.Manejo.del.Habitat-Tasa.Subsido  #[usdx1000/year]
    Rentabilidad.UMA<-Ingresos.por.Permisos-Costos.Operacion.UMA # [usdx1000/año]
#EXPERIMENTOS CON EFECTO DE CAMBIO HÍDRICO DESPUÉS DE AGOSTO DE 2017
    #CEH_1_1
      tendencia<--0.035
      Amplitud.cambio.hidrico<-0.4
      Periodo.cambio.hidrico<-0.19
#cambio.hidrico<-1.0
  cambio.hidrico<-max(tendencia*t+(1.0-Amplitud.cambio.hidrico*sin(Periodo.cambio.hidrico*t))+runif(1,-0.0,0.2),0) # Precip/Precip.Promedio.historica
    Effect.Rainfall.Change.on.Emigration.Ejido<-approx(c(0.000,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0,100),
                                                         c(100.0,50.0,20.0,10.0,5.00,4.00,3.50,3.00,1.50,1.10,1.0,1.0), xout = cambio.hidrico)$y
    Effect.Rainfall.Change.on.Immigration.Ejido<-approx(c(0.000,0.10,0.20,0.300,0.40,0.50,0.60,0.70,0.80,0.90,1.0,1.10,1.2,1.30,1.4,1.50,1.60,1.70,1.8,1.9,2.0,100),
                                                          c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.70,0.90,1.0,1.15,1.3,1.35,1.4,1.45,1.48,1.49,1.5,1.5,1.5,1.5), xout = cambio.hidrico)$y
#
    Effect.Rainfall.Change.on.Deaths.BCS<-approx(c(0.000,0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.0,100),
                                                     c(100.0,50.0,20.0,10.0,5.00,4.00,3.50,3.00,1.50,1.10,1.0,1.0), xout = cambio.hidrico)$y
    Effect.Rainfall.Change.on.Births.BCS<-approx(c(0.000,0.10,0.20,0.300,0.40,0.50,0.60,0.70,0.80,0.90,1.0,1.10,1.2,1.30,1.4,1.50,1.60,1.70,1.8,1.9,2.0,100),
                                                      c(0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.50,0.70,0.90,1.0,1.15,1.3,1.35,1.4,1.45,1.48,1.49,1.5,1.5,1.5,1.5), xout = cambio.hidrico)$y
#En Ejido
    Immigration.Rate<-Immigration.Rate.Base*Effect.Rainfall.Change.on.Immigration.Ejido # [%/year] ø
    Emigration.Rate<-Emigration.Rate.Base*Effect.Rainfall.Change.on.Emigration.Ejido # [%/year]ø
    Immigration.Bighorn.Ejido<-Pop.Bighorn.BCS*Immigration.Rate # [bighorn sheep/year]ø
    Emigration.Bighorn.Ejido<-Pop.Bighorn.Ejido*Emigration.Rate # [bighorn sheep/year]ø

#Variables de flujo de BCS
    No.Permits.BCS <-max(General.Use.Rate.BCS*(1-Proportion.Permits.Ejido),0) # [bighorn sheep/year]?????
    Death.Rate<-Death.Rate.base*Effect.Rainfall.Change.on.Deaths.BCS #ø
    Birth.Rate<-Birth.Rate.base*Effect.Rainfall.Change.on.Births.BCS #hasta el estresor
    Deaths.BCS<-max(Pop.Bighorn.BCS*Death.Rate,0) # [bighorn sheep/year]ø
    Births.BCS<-max(Pop.Bighorn.BCS*Birth.Rate,0) # [bighorn sheep/year]ø
#Variables de Estado
    dPop.Bighorn.Ejido <- Immigration.Bighorn.Ejido-No.Permit.Ejido-Emigration.Bighorn.Ejido # bighorn sheep/year]ø
    dPop.Bighorn.BCS <- Births.BCS-Deaths.BCS-No.Permits.BCS #[bighorn sheep/year]ø
    dPrice.Permit.Ejido<-Demand.Permit.Ejido/Demand.Elasticity.Permit.Price-Supply.Permit.Ejido/Supply.Elasticity.Permit.Price ø
    list(c(dPop.Bighorn.BCS,dPop.Bighorn.Ejido,dPrice.Permit.Ejido),
           cambio.hidrico = cambio.hidrico,
           #Immigration.Bighorn.Ejido = Immigration.Bighorn.Ejido,
           #Emigration.Bighorn.Ejido = Emigration.Bighorn.Ejido,
           #Rentabilidad.UMA = Rentabilidad.UMA,
           Demand.Permit.Ejido = Demand.Permit.Ejido,
           #Supply.Permit.Ejido = Supply.Permit.Ejido,
           #Demanda.Relativa.Cintillo = Demand.Permit.Ejido/Supply.Permit.Ejido,
           Ingresos.por.Permisos = Ingresos.por.Permisos,
           No.Permit.Ejido = No.Permit.Ejido,
           Price.Permit.Relative = Price.Permit.Relative,
           No.Permits.BCS = No.Permits.BCS)
  })
}

parametros<-c(Birth.Rate.base = 0.047215, # [%/año] estimando poblacional estable durante los ultimos años con permisos
              Death.Rate.base = 0.01, # [%/año]  estimando poblacional estable durante los ultimos años con permisos
              Male.Proportion = 0.32 , # [%]  aerial survey 2016 (citation)
              Percentage.Use.BCS = 0.10, # [%], Mexican General Direction of Wildlife
              Base.Observation.Rate.Bighorn.BCS = 0.30, # [%], (observation rate =0.30,Lee,2003--,)
              Proportion.Permits.Ejido = 0.173, # [%], estimado considerando que ya se asignan 7.0 permisos por año
              Emigration.Rate.Base =0.01, # [%] exploratorio -por definir con modelo de nicho-
              Immigration.Rate.Base = 0.008385, # [%] estimando poblacional estable durante los ultimos años con permisos
              Area.Ejido = 5.5, # [km2x1000]
              Presence.Bighorn.Ejido.Initial = ((38/176)*(176/0.30))/5.5 ,#[borregos/km2x1000]38=the number of observed sheep at the ejido Bonfil,176=the total observed sheep in BC ø
              Sighting.Bighorn.Ejido.Base = 7.9, # [borregos/hora de vuelo], aerial survey 2016 (citation)
              Beta0 = 2.9715 , # [borregos/año]los parámetros Beta se ajustaron para que la demenda sea igual a 7 (número de permisos disponibles)
              Beta1 = 1.5 , # [borregos/año]
              Beta2 = 1.5,  # [borregos/año]
              Beta3= 0.20 ,# [(borregos/año)/(borregos/hr.vuelo)]
              Beta4= 0.011 ,# [(borregos/año)/(usdx1000)]
              Sustainable.Management = 1, # [1]
              Status.Protection = 1, # [1]
              Demand.Elasticity.Permit.Price = 0.2, # [%]
              Supply.Elasticity.Permit.Price = 0.175, # [%]
              Gasto.Asistencia.Convencion = 7.0, # [usdx1000/año] , ajustado a 70,000 usd para inversion, y 70,000 usd de ingreso de uma
              Tasa.Subsido = 100 , # [usdx1000/año] , ajustado a 70,000 usd para inversion, y 70,000 usd de ingreso de uma
              Gasto.UMA.Guia = 50.0 , # [usdx1000/año] , ajustado a 70,000 usd para inversion, y 70,000 usd de ingreso de uma
              Gasto.UMA.Vigilancia = 300 , # [usdx1000/año] , ajustado a 70,000 usd para inversion, y 70,000 usd de ingreso de uma
              Gasto.UMA.Manteminto.Habitat = 250 ) # [usdx1000/año] , ajustado a 70,000 usd para inversion, y 70,000 usd de ingreso de uma


CondicionesIniciales <- c(Pop.Bighorn.BCS = 586.0 , # [bighorn sheep] Estimated as:176/0.30 Total.Pop=Observed.Pop/Observation.Rate; data (observation rate=0.30,Lee,2003--,), (Pop.Observed=176, aerial survey 2016 (citation needed))
                       Pop.Bighorn.Ejido = 126.00, # [bighorn sheep] ;(38/176)*(176/0.30) Pop.Observada.Ejido/Pop.Observada (Pop.Observada.Ejido = poblacion de ejido+Población La Purisima [aerial survey 2016])
                       Price.Permit.Ejido = 70) # [usdx1000/bighorn sheep] revisar info de entrevista

times <- seq(0, #initial time # "seq": genera secuencias regulares
             30, # durate 30 años
             0.5)#time step -cada mes-

intg.method<-c("rk4") #algoritmo de integración. By Euler's Method or Classical Runge-Kutta 4th Order Integration.
#Simular modelo
set.seed(99999)
out <- ode(y = CondicionesIniciales, #ode: resuelve ecuaciones diferenciales. General Solver for Ordinary Differential Equations
           times = times,
           func = management.bighorn ,
           parms = parametros,
           method =intg.method )

plot(out) #plot: genera las gráficas


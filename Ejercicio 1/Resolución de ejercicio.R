###Ejercicio 1
   
dta<- read.table("dta_veg.txt", header=T)
str(dta)


#Pregunta 1.

   #Primero es necesario definir como vamos a comparar entre las dos localidades, puesto que tenemos 
   #6 parcelas en cada localidad.  Deberíamos definir primero si juntaremos todas las parcelas para 
   #obtener un valor global en cada localidad.
   
   locC<<-colSums(dta[dta$Localidad=="Chinchas", 3:42])
   locA<- colSums(dta[dta$Localidad=="Alamala",3:42])

   locC.RA <- locC/sum(locC)
   locA.RA <- locA/sum(locA)
   
   #ponemos en un solo vector
   
  loc.RA<- cbind(locA.RA, locC.RA)

##Porcentaje de similitud es igual a la suma 
##de los porcentajes mínimos de cada especie en la comunidad

   mins <- apply(loc.RA, 1, min)
   mins
   
   sum(mins)*100
   
    #########
    ###42.90
    ########

#######################
   ### Sorensen: es igual a 2C(especies comunes)/A+B (especies en el sitio A y en el sitioB)

##Definimos las especies que no son compartidas

   comp<- apply(loc.RA, 1, function(abuns) all(abuns != 0))
   comp
   
##Sumamos las especies compartidas

   Rs <- apply(loc.RA, 2, function(x) sum(x > 0))
   Rs

#aplicamos la formula de sorensen
   
   2*sum(comp)/sum(Rs)
   
   ##73%
   
   
   
###################
#Pregunta 2: Similitud entre altitudes
 

#Comaparamos altitud de 1600 (parcela 3 en Alamala y 11 en ls Chinchas)
#y hacemos los calculos

#Sumamos las parcelas a 1600 en cada zona

a<- apply(dta[3:4,3:42], 2, sum)
b<- apply(dta[11:12,3:42], 2, sum)
ab<-rbind(a,b) 
ab<- ab[,c(apply(dta[c(3,4,11,12),3:42], 2, sum)!=0)]

#Sacamos la abundancia relativa

A16.RA <- t(apply(ab, 1,
                  function(sp.abun) sp.abun/sum(sp.abun)))
A16.RA

#Sacamos el índice de similitud
mins16 <- apply(A16.RA[1:2, ], 2, min)
mins16
A16_S<- sum(mins16)*100

#El índice de sorensen
comp16<- apply(A16.RA, 2, function(abuns) all(abuns != 0))
comp16

Rs16 <- apply(A16.RA, 1, function(x) sum(x > 0))
Rs16
A16_Sor<-2*sum(comp16)/sum(Rs16)

#Sumamos las parcelas a 1900 en cada zona

a9<- apply(dta[7:8,3:42], 2, sum)
b9<- apply(dta[13:14,3:42], 2, sum)
ab9<-rbind(a9,b9) 
ab9<- ab9[,c(apply(dta[c(7,8,13,14),3:42], 2, sum)!=0)]

#Sacamos la abundancia relativa

A19.RA <- t(apply(ab9, 1,
                  function(sp.abun) sp.abun/sum(sp.abun)))
A19.RA

#Sacamos el índice de similitud
mins19 <- apply(A19.RA[1:2, ], 2, min)
mins19
A19_S<- sum(mins19)*100

#El índice de sorensen
comp19<- apply(A19.RA, 2, function(abuns) all(abuns != 0))
comp19

Rs19 <- apply(A19.RA, 1, function(x) sum(x > 0))
Rs19
A19_Sor<-2*sum(comp19)/sum(Rs19)

#Sumamos las parcelas a 1500 en cada zona

a5<- apply(dta[1:2,3:42], 2, sum)
b5<- apply(dta[9:10,3:42], 2, sum)
ab5<-rbind(a5,b5) 
ab5<- ab5[,c(apply(dta[c(1,2,9,10),3:42], 2, sum)!=0)]

#Sacamos la abundancia relativa

A15.RA <- t(apply(ab5, 1,
                  function(sp.abun) sp.abun/sum(sp.abun)))
A15.RA

#Sacamos el índice de similitud
mins15 <- apply(A15.RA[1:2, ], 2, min)
mins15
A15_S<- sum(mins15)*100

#El índice de sorensen
comp15<- apply(A15.RA, 2, function(abuns) all(abuns != 0))
comp15

Rs15 <- apply(A15.RA, 1, function(x) sum(x > 0))
Rs15
A15_Sor<-2*sum(comp15)/sum(Rs15)

#hacemos una matriz para poner los datos

comp_tot<- matrix(0,3,2)
colnames(comp_tot)<- c("simil", "Soren")
rownames(comp_tot)<- c("A1500", "A1600", "A1900")

comp_tot[1,1]<- A15_S
comp_tot[1,2]<- A15_Sor
comp_tot[2,1]<- A16_S
comp_tot[2,2]<- A16_Sor
comp_tot[3,1]<- A19_S
comp_tot[3,2]<- A19_Sor

comp_tot[,2]<- comp_tot[,2]*100

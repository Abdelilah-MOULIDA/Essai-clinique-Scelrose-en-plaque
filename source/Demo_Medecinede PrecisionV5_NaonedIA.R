## Copyright Methodomics 

 library(ggplot2)

### je veux generer un exemple de donnees d essai clinique  qui me permetterait de montrer ce qu'est la medecine personalisee ###

 #Le DFG est environ de 0,12 L/min (120mL/min, soit 170 L/j). 
#Niveau 1 : ??? 90mL/min/1.73m2 , DFG normal ou augmenté ;
#Niveau 2 : 60-89mL/min/1.73m2, DFG légèrement diminué ;
#Niveau 3 : 30-59mL/min/1.73m2, insuffisance rénale chronique modérée ;
#Niveau 4 : 15-29mL/min/1.73m2, insuffisance rénale chronique sévère ;
#Niveau 5 : < 15mL/min/1.73m2, insuffisance rénale chronique terminale.
set.seed(2017)

#SCENARIO le medicament A permet 90 sd10  au bout d un an  -->55 sd 10-15
 Baseline= rnorm(700,90,10)
 Treatment=c(rep("A",350),rep("B",350))
 AARM=c(rnorm(250,55,15), rnorm(50,55,10), rnorm(50,55,10)) # marche a peu pres mieux pour tout le monde avec des varaibliité differentes
 BARM=c(rnorm(240,43,15), rnorm(60,77,10), rnorm(50,60,10)) # march mieux pour certains et moins bien pour les autres  - en moyenne moins bon 
 gender=c(ifelse(runif(350)<0.4,0,1),ifelse(runif(250)<0.25,0,1),ifelse(runif(50)<0.65,0,1)) # l 'efficacité est liée au genre
 smoke=c(ifelse(runif(350)<0.2,0,1),ifelse(runif(300)<0.15,0,1),ifelse(runif(50)<0.5,0,1)) # l 'efficacité est liée au genre

 # Matrixing the data  
 DATA=cbind(Baseline, c(AARM, BARM))
  rownames(DATA)=Treatment
  colnames(DATA)=c("Baseline","Result")
  # transfo en EDSS [compris entre 0 et 10 par demi points]
  hist(10+(-1*round(c(DATA[,1],DATA[,2])/6,0)/2))
  table(round(c(DATA[,1],DATA[,2])/6,0)/2)
  
  dfg2edss= function(vec) { 10+(-1*round(vec/6,0)/2)}
  
  Baseline <- dfg2edss(Baseline)
  DATA[,1] <- dfg2edss(DATA[,1]) #baseline
  DATA[,2] <- dfg2edss(DATA[,2]) #treated
  

##TEST 1 en moyenne A mieux que B ###
summary(lm(DATA[,2]~rownames(DATA)))
summary(lm(DATA[,2]-DATA[,1]~rownames(DATA)))
t.test(DATA[1:350,2]-DATA[1:350,1],DATA[351:700,2]-DATA[351:700,1]) # A fait meiux que B 
t.test(DATA[1:350,2],DATA[351:700,2]) # A fait meiux que B 

##TEST 1 en variance A mieux que B ###
bartlett.test(c(DATA[1:350,2]-DATA[1:350,1],DATA[351:700,2]-DATA[351:700,1])~rownames(DATA)) # A fait meiux que B 
bartlett.test(c(DATA[1:350,2],DATA[351:700,2])~rownames(DATA)) # A fait meiux que B 

## effet Sexe NS 
t.test(DATA[gender==1,2]-DATA[gender==1,1],DATA[gender==0,2]-DATA[gender==0,1]) # effet sexe
t.test(DATA[gender==1,2],DATA[gender==0,2]) # effet sexe

## Modele lineaire ? 
plot(DATA[,1],DATA[,2],col=c(rep("green",300),rep("blue",300)))
 abline(lm(DATA[1:300,2]~DATA[1:300,1]),col="green")  # effet additif moyen de A
 abline(lm(DATA[301:600,2]~DATA[301:600,1]),col="blue") # effet additif moyen de B
 
#FIG1 Boxplot 
boxplot(DATA[1:300,2]-DATA[1:300,1],(DATA[301:600,2]-DATA[301:600,1]),col="light gray", main="Traditional RCT A vs B with borderline gender effect", names=c("Treat A", "Treat B"), outline = F)
 points(jitter(rep(1,300),9), DATA[1:300,2]-DATA[1:300,1], col="light green")
 points(jitter(rep(2,300),5), DATA[301:600,2]-DATA[301:600,1], col="light blue")
 abline(h=mean(DATA[1:300,2]-DATA[1:300,1]), col="red")
 text(1.5,7, paste("P_AvsB=",formatC(t.test(DATA[1:300,2]-DATA[1:300,1],DATA[301:600,2]-DATA[301:600,1])$p.value,digits=2,format="e"),sep=""))
 text(1.5,6, paste("P_gender=",formatC(t.test(DATA[gender==1,2]-DATA[gender==1,1],DATA[gender==0,2]-DATA[gender==0,1])$p.value,digits=2,format="e"),sep=""))

# FIG S1 Density 
#Data framing the data 
dat <- data.frame(dens = c(DATA[,1], DATA[1:350,2],DATA[351:700,2])
                , lines = c(rep("0",700), rep(c("A","B"),each = 350)))
# Plotting the scenario 
ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)

#Data framing the data 
dat2 <- data.frame(dens = c(DATA[1:350,2],DATA[351:700,2])
                  , lines = c( rep(c("Drug A"," Drug B"),each = 350)))
# Plotting the scenario 
ggplot(dat2, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)

################################################
## je chercher des criteres de resemblance qui me feraient donner B plutot que A a ceux qui ont plus beneifice de B que de A 
################################################

# Comment trouver une regle qui me permettent de montrer en choississant B plutot que A pour ceux pour qui B a bien marche 
# je fais "mieux" que en donnant A a tous....: Mieux = "Mieux" en moyenne  ou pas vriament pire // 

# Sur quelle base je rapproche vers les gens de A et les gens de B ? 

# Dois je conserver comme reference de B ceux qui ont fait mieux sous B que la moyenne de A ? la moyenne plus 1 sd?  
# Sol 1:   je compare mon POI a A et B-faisant-"mieux"-que-A (par hasard? )
# Sol 2:   je lui donne B si il resssemble a plus de "B de reference ( B > moy A)" que de A de reference  parmi les X plus proches
 
# Step 1 je fais un groupe de patient reference A ou B si B afait mieux que A en moyenne 
Ref =  Treatment== "A" | (DATA[351:700,2]-DATA[351:700,1])<mean(DATA[1:350,2]-DATA[1:350,1]) # discutable
Ref1 =  Treatment== "A" | (DATA[351:700,2]-DATA[351:700,1])< mean(DATA[1:350,2]-DATA[1:350,1]) # discutable PAG dirait tout le monde compte pb variabilite


 ## 1000 pqtients suivants ( mem base line - 40 d homme )
POI= dfg2edss(rnorm(1000,90,10)) ; GOI=ifelse(runif(1000)<0.4,0,1)  # 1000 patients suivant
POIA= POI + mean(DATA[1:350,2]-DATA[1:350,1]) # je modelise l'effet du traitement a partir de mes donnees  discutable pas de varaibilite
POIB= POI + mean(DATA[351:700,2]-DATA[351:700,1]) # je modelise l'effet du traitement a partir de mes donnees  discutable
#POIRuleBfemale= POI + mean(DATA[Ref,2]-DATA[Ref,1]) #( A sauf si femme B car peut etre effet dans RCT)

#POIA= POI + rnorm(1000,mean(DATA[1:300,2]-DATA[1:300,1]),sd(DATA[1:300,2]-DATA[1:300,1])) # je modelise l'effet du traitement a partir de mes donnees  discutable pas de varaibilite
#POIB= POI + rnorm(1000,mean(DATA[301:600,2]-DATA[301:600,1]),sd(DATA[301:600,2]-DATA[301:600,1])) # je modelise l'effet du traitement a partir de mes donnees  discutable
# DFG2EDSS needed ? 

POINN51=vector()
DIST=vector()
#K NN
for (i in 1:1000) {
  #i=2
  k=51 # pourquoi 51 ? A cause du pastis ...  # distance a faire dans PLS-DA  euclidienen pondere entre PLS DA te AFC discrinante 
  DIST=cbind(DIST,sqrt((Baseline-POI[i])/dfg2edss(90)*(Baseline-POI[i])/dfg2edss(90)+(gender-GOI[i])*(gender-GOI[i])))
  select=rank(sqrt((Baseline-POI[i])/dfg2edss(90)*(Baseline-POI[i])/dfg2edss(90)+(gender-GOI[i])*(gender-GOI[i])))<=k #51 neighbors euclidean dist ( on pourrait chercher le premier point d inflexion dans la deniste de similarite )
  table(select)
  POINN51[i]=ifelse(mean(DATA[select & Treatment=="A",2]-DATA[select & Treatment=="A",1]) < mean(DATA[select & Treatment=="B",2]-DATA[select & Treatment=="B",1])  
                      # CLE je donne le traitement qui marche le mieux parmi K voisins en moyenne ##
                     ,POI[i] + mean(DATA[select & Treatment=="A",2]-DATA[select & Treatment=="A",1])
                      ,POI[i] + mean(DATA[select & Treatment=="B",2]-DATA[select & Treatment=="B",1]))
  POINN51[i]=ifelse(is.na(POINN51[i]), POI[i] + mean(DATA[Treatment=="A",2]-DATA[Treatment=="A",1]),POINN51[i])
                    
  #POINN51[i]=ifelse(sum(Treatment[select]=="A")>k/2  # je donne le traitement des X plus porches voisins que je modelise en local 
  #                  ,POI[i] + mean(DATA[select & Treatment=="A",2]-DATA[select & Treatment=="A",1])
  #                  ,POI[i] + mean(DATA[select & Treatment=="B",2]-DATA[select & Treatment=="B",1]))
  #POINN51[i]=ifelse(sum(Treatment[select]=="A")>k/2,  # je donne le traitement des X plus porches voisins que je modelise en local 
  #                  , POI[i] + mean(DATA[1:300,2]-DATA[1:300,1])
  #                   ,POI[i] + mean(DATA[301:600,2]-DATA[301:600,1]))
}

# graphical view
for (i in 1:10) {
  k=51 # pourquoi 51 ? A cause du pastis ... 
  select=rank(sqrt((Baseline-POI[i])/dfg2edss(90)*(Baseline-POI[i])/dfg2edss(90)+(gender-GOI[i])*(gender-GOI[i])), ties.method="random")<=k #51 neighbors euclidean dist
  plot(DIST[,i],jitter(rep(0,nrow(DIST)),4),col=ifelse(rank(DIST[,i])<=51,"black", "grey")
        ,pch=ifelse(Treatment=="A","A","B"),xlim=c(0,1.05), ylim=c(-0.2,0.2),cex=0.5
       ,xlab="Baseline Distance from POI", ylab ="")
  title(main=paste(" POI #", i))
  text(0.2,0.18,paste("#A in NN =",sum(Treatment[select]=="A")),col="red")
  text(0.2,0.14,paste("#B in NN =",sum(Treatment[select]=="B")),col="blue")
  text(0.5,0.16,paste("Let 's give",ifelse(sum(Treatment[select]=="B")>sum(Treatment[select]=="A"),"B","A"))
       ,col=ifelse(sum(Treatment[select]=="B")>sum(Treatment[select]=="A"),"blue","red"))
  
  plot(DIST[,i],jitter(rep(0,nrow(DIST)),4),col=ifelse(rank(DIST[,i])<=51,ifelse(Treatment=="A","red","blue"), "grey")
       ,pch=ifelse(Treatment=="A","A","B"),xlim=c(0,0.08), ylim=c(-0.2,0.2), cex=1
       ,xlab="Baseline Distance from POI", ylab ="")
  title(main=paste("Zoomed NN of  POI #", i))  
  text(0.01,0.18,paste("#A in NN =",sum(Treatment[select]=="A")),col="red")
  text(0.01,0.14,paste("#B in NN =",sum(Treatment[select]=="B")),col="blue")
  text(0.025,0.16,paste("Let 's give",ifelse(sum(Treatment[select]=="B")>sum(Treatment[select]=="A"),"B","A"))
                        ,col=ifelse(sum(Treatment[select]=="B")>sum(Treatment[select]=="A"),"blue","red"))
                        
  hist(DIST[,i], breaks=100,main=paste(" POI= Mister or Miss", i))
  
}


#FIg 2
boxplot(POIA, POINN51, POIB, names=c("Treat A","51NN","Treat B"),col="light grey", outline=F, main="Fig.2: Personalised medicine based on A vs.B RCT")
 points(jitter(rep(1,1000),11), POIA, col="light green")
 points(jitter(rep(2,1000),6), POINN51, col="orange")
 points(jitter(rep(3,1000),3.5), POIB, col="light blue")
 abline(h=median(POIA), col="green")
 abline(h=median(POINN51), col="orange")
 abline(h=median(POIB), col="blue")
 text(1.5,7, paste("p=",formatC(t.test(POIA,POINN51)$p.value,digits=2,format="e"),sep=""))
 text(2.5,7, paste("p=",formatC(t.test(POIB,POINN51)$p.value,digits=2,format="e"),sep=""))
 text(2,7, paste("p=",formatC(t.test(POIA,POIB)$p.value,digits=2,format="e"),sep=""))

#Fig S2
boxplot(POI, POIA, POINN51, POIB, names=c("baseline","Treat A","51NN","Treat B"), main="Fig S2 with baseline ",col="light grey",outline=F)
text(1.7,3, paste("p=",formatC(t.test(POIA,POINN51)$p.value,digits=3,format="e"),sep=""))
text(2.7,3, paste("p=",formatC(t.test(POIB,POINN51)$p.value,digits=3,format="e"),sep=""))
text(3.7,3, paste("p=",formatC(t.test(POIB,POIA)$p.value,digits=3,format="e"),sep=""))
points(jitter(rep(2,1000),9), POIA, col="light green")
points(jitter(rep(3,1000),5), POINN51, col="orange")
points(jitter(rep(4,1000),3), POIB, col="light blue")
abline(h=mean(POIA), col="green")
abline(h=mean(POINN51), col="orange")
abline(h=mean(POIB), col="blue")
# mon critere de jugement c est de diminuer la sd entre patient traiter a essai non inferieur a marge large.


# faire simulation 
 # ici faibleasse je na i pas beaucoup de de donnees et les donnees en T=0 baseline sont independante de l efficacite du traitement 
 # par contre B est peut eter un peu plus efficace chez les femmes 

# demontrer l innocuite du traitement  ( non infeririote) 
# demontrer l efficacite de la nouvelle procedure 
# ML : J ai defini un trining a parti de l essai clinique  - jai un modele de lefet de A et de B


## je cherche une regle de ressemblance de mes X prochain patinets ( A ou B ) qui fasse mieux que A 

## FIG 3  montrer les segments  et faire des scenarios par patients en global Moy de A Moyenne de B , Moyenne des voisins A Moyenne des voisins B. 
 
## Pui CHoisir quelques patients en exemples 
 ## montrer l imprescisiond e la prediction
 
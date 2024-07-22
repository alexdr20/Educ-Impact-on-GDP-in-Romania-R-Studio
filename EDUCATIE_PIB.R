# Draghicescu Alexandra 
# 1073A 
# Csie 2022-2023 
# 
# Impactul performanței academice a populației asupra PIB-ului


summary(educ_PIB_RO)
#consideram,in primul rand,existenta unei legaturi directe intre nivelul de educatie(nr mediu de ani de scoala al populatiei rezidente si nivelul PIB-ului tarii)
#pentru inceput,consideram un sistem de coordonate(x,y) unde x este variabila independenta(nr de ani de scoala),iar y este variabila dependenta

x=educ_PIB_RO$ani_scoala
y=educ_PIB_RO$PIB
plot(x,y)
text(x,y,educ_PIB_RO$an)
library(ggplot2)

ggplot(educ_PIB_RO, aes(x=x, y=y)) +geom_point(colour="orange",size=3) +geom_text(aes(label=an,size=3))+ geom_smooth()
#din grafic,vizual, se poate observa cum ca,odata cu cresterea nr de ani de scoala,exista o creste si a pib-ului 
#insa,nu suntem siguri de aceasta legatura directa

#in mod similar,putem considera si o legatura directa intre nr de ani de scoala si rata de ocupare 

x=educ_PIB_RO$ani_scoala
z=educ_PIB_RO$rata_ocupare
plot(x,z)
lines(predict(lm(z~x)),col='green')
text(x,z,educ_PIB_RO$an)
educ_PIB_RO$an
ggplot(educ_PIB_RO, aes(x=x, y=z)) +geom_point(colour="orange",size=3) +geom_text(aes(label=an,size=3))+ geom_smooth()

#in mod evident,se poate observa tendinta de descrestere brusca in perioada 2020-2021 de aprox 3%,fapt datorat perioadei de pandemie si a restrictiilor impuse in ceea ce priveste limitarea de miscare

#in urmatorul pas,vom considera modelul de regresie simpla dintre PIB si nr de ani de scoala
library(dplyr)

reg1_ed_p=lm(PIB~ani_scoala,data=educ_PIB_RO)
summary(reg1_ed_p)
 #exact cum am anticipat,rezultatele abtinute sunt unele valiude si importante din punct de vedere statistic 
#valorile obtinute ar avea urmatoarea interpretare 
#in medie,Romania are o valore negativa a PIB-ului,in jurul valorii de 1697214 mil euro,insa nivelul de edeucatie al populatiei are un efect pozitiv
#cu fiecare an de scoala in plus,valoare PIB-ului creste cu 154900 mil euro 
#astfel,nivelul de cunostiinte academice al populatie contribuie in mod direct la dinamica economica a tarii,respectiv la cresterea nivelului PIB-ului(calculat la pretul pietei)
#vizual ,putem observa rezultatul regresiei prin intermediul coeficientului beta1(panta)

x=educ_PIB_RO$ani_scoala
y=educ_PIB_RO$PIB
plot(x,y)
abline(reg1_ed_p)
 #in mod asteptat,valorile se afla in jurul pantei functiei(mai mult sau mai putin exact pe panta) 
#luand in considerare rezultatele obtinute in partea de summary,putem interpreta urmatoarele:
#Multiple R-squared:  0.9026,	Adjusted R-squared:  0.8905 - ambele au valori asemanatoare,ceea ce ne ajuta sa afirmam faptul ca
#peste 89% din valorile variabilei independente influenteaa in mod direct rezultatul modelului,respectiv evolutia variabilei dependente
#mai mult,0.89 este foarte aproape de 1,limita mnaxima admisa pentru R-squared,ceea ce inseamna ca exista o legatura directa puternica intre nivelul de educatie si PIB-ul tarii

#insa,in realitate,dinamica PIB-ului unei tari,din prisma nivelului de educatie,nu poate fi justificat doar prin influenta nr de ani de studiu ai populatie

#consider ca,o alta variabila independeta care ajuta la descrierea dinamicii PIB-ului,este si rata ocuparii de pe piata muncii(nr de persoane active pe piata muncii )
#conform definitiei oficiale,asa cum este prezentata pe platforma tempo insee
# Populatia ocupata cuprinde, conform metodologiei Anchetei statistice asupra fortei de munca in gospodarii, persoanele cu varste cuprinse intre 15 si 89 de ani (in ani impliniti, la sfarsitul saptamanii de referinta) care, in cursul saptamanii de referinta, se incadrau in una dintre urmatoarele categorii:
# (a) persoane care, in cursul saptamanii de referinta, au lucrat cel putin 1 ora contra plata sau pentru obtinerea unui profit, inclusiv lucratorii familiali care colaboreaza la intreprinderea familiala ;
# (b) persoane care au un loc de munca sau o intreprindere si care au fost temporar absente de la lucru in cursul saptamanii de referinta, dar au avut o legatura formala cu locul de munca; 
# (c) persoane care produc bunuri agricole destinate in cea mai mare parte vanzarii sau schimbului in natura.
# Incepand cu anul 2021, persoanele care produc bunuri agricole destinate exclusiv sau majoritar auto-consumului nu mai fac parte din populatia ocupata.Anterior anului 2021, aceste persoane erau considerate ocupate daca:
# a)    productia agricola era destinata, fie si macar in parte, vanzarii sau schimbului in natura (troc);
# b)    productia agricola era destinata exclusiv consumului propriu si reprezenta o parte substantiala  a consumului total al gospodariei.
# Periodicitate
# 
# Anuala, Trimestriala
# Surse de date
# 
# Cercetarea statistica asupra fortei de munca in gospodarii - AMIGO  Detalii
# Metodologie
# 
# POPULATIA OCUPATA (PO)= Populatia activa (PA) - Somerii BIM (So)
# Populatia ocupata se clasifica in categorii de persoane ocupate in functie de statutul profesional. Statutul profesional reprezinta situatia detinuta de o persoana in functie de modul de obtinere a veniturilor prin activitatea exercitata si anume:
#   - Salariat - este considerata persoana care-si exercita activitatea pe baza unui contract de munca intr-o unitate economica sau sociala - indiferent de forma ei de proprietate - sau la persoane particulare, in schimbul unei remuneratii sub forma de salariu.
# - Patron - este persoana care-si exercita ocupatia (meseria) in propria sa unitate (intreprindere, agentie, atelier, magazin, birou, ferma etc.), pentru a carei activitate are angajati unul sau mai multi salariati.
# -Lucrator pe cont propriu - este persoana care-si exercita activitatea in unitatea proprie sau intr-o afacere individuala, fara a angaja nici un salariat, fiind ajutat sau nu de membrii familiei neremunerati. Sunt incadrati la acest statut intreprinzatorii independenti (vanzatorii ambulanti, meditatorii, femeile care ingrijesc copii, carausii, taximetristii particulari etc.), liber profesionistii (muzicantii ambulanti, artistii plastici, avocatii), zilierii ocazionali, titularii de contracte de gestiune sau de concesiune care nu folosesc salariati, agricultorii individuali sau care lucreaza in asociatii agricole.
# - Lucrator familial neremunerat - este persoana care-si exercita activitatea intr-o unitate economica familiala condusa de un membru al familiei sau o ruda, pentru care nu primeste remuneratie sub forma de salariu sau plata in natura.
# - Membru al unei societati agricole sau al unei cooperative neagricole - este considerata persoana care a lucrat fie ca proprietar de teren agricol intr-o societate agricola constituita conform Legii 36/1991, fie ca membru al unei cooperative mestesugaresti.

#astfel vom considera modelul de regresie multipla,format din variabilele independente nr de ani de scoala si rata ocuparii(pentru a usura calculele vom considera procentul,deoarece efectiv nr de persoane ocupate pe piata muncii nu este relevant din cauza unor factori externi,precum ar fi migratia)
#si in mo evident,variabila dependenta,PIB-ul

#in mod similar,vom urma pasii ca la regresia simpla

reg2_ed_p=lm(PIB~ani_scoala+rata_ocupare,data=educ_PIB_RO) #folosind rata de ocupare
summary(reg2_ed_p)

reg3_ed_p=lm(PIB~ani_scoala+pop_ocupata,data=educ_PIB_RO) #folosind nr de pers ocupatre(milioane)
summary(reg3_ed_p) #pentru un rezultat mai bun,mil persoane ar tebui transformate in mii persoane

#vom interpreta rezultatele din prima regresie multipla reg2
#in urma rezultatelor obtinute in seectiunea summary,putem trage urmatoarele concluzii:
# in primul rand,rezultatele obtinute sunt semnificative din punct de vdere statistic
# dupa cum sugereaza cele 3 ***,respectiv si p-value foarte aproape de 0
# valorile Multiple R-squared:  0.9825,	Adjusted R-squared:  0.9775 sunt mai mari si mult mai aproape de 1
# ceea ce inseamna ca variabila dependenta este mult mai bine "explicata" de cele doua variabile independente
# si mai mult,efectul celei de-a doua variabile si impactul este unul pozitiv
# in esenta,cu fiecare an de scoala al populatiei tarii,valoarea PIB-ului creste cu 165095 mil euro,
# iar pentru fiecare procent in plus la rata ocuparii pietei muncii(0,01) PIB-ul creste cu 7680 mil euro
# in mod comparativ,rezultatl celei de-a doua regresii multiple ar putea fi interpretat astfel:
#   pentru fiecare an in plus de scoala,PIB-ul va creste cu 196 600  euro,
# in timp ce pentru fiecare persoana angajata,PIB-ul va creste cu 50.71 euro

library(car)
avPlots(reg5_ed_p,layout=c(1,3),col="orange",main="Regresie Liniara Multipla",pch=20,cex=5)

#PRODUCTIVITATEA MUNCII 

# #asa cum era de asteptat,in urma regresiei si a rezultatelor semnificative dpdv statistic
# si graficele arata acest efect dpdv vizual
# atat pentru nr de ani de scoala,cat si pentru rata de ocuparae,valorile PIB-ului se plimba in jurul pantei(cu mici exceptii,in mod evident care sunt influentate de factori externi)

# In mod evident,in viata reala,variatia PIB-ului nu sta doar in acesti doi factori independeti-respectiv nr de ani de scoala si rata de ocupare
# Pentru a face modelul si mai complwt,putem apela si la un al 3-lea factor mai mult sau mai putin independent,ci anume investitiile facute in educatie din bani publici.
# am considerat aceasta variabila in unitate de masura % ca fiind procent din PIB,la final anului 

#vom considera in mod experimental,un nou model de egresie cu 3 variabile independente
reg4_ed_p=lm(PIB~ani_scoala+rata_ocupare+ch_educ,data=educ_PIB_RO) #folosind rata de ocupare
summary(reg4_ed_p) 
#asa cum am anticipat,ch nu este o variabila importanta,ci nici independenta,ci mai degraba una dependenta de valoarea PIB-ului,astefl aceasta nu poate fi folosita in modelul de regresie 

#un alt factor considerat important ar puta fi,valoarea consumului final(evident masurat lap retul pietei)
#ca o relatie cauzala ce ar putea fi explicata astfel:odata ce un individ acumuleaza mai multi ani de scoala,sansele sa obtina un loc de munca bine platit
#(proportional cu efoprtul depus in timpul anilor de studiu) sunt mai mari,astfel odata cu un salariu suficient acestia vor alege sa consumje fie mai mult,fie produse sau servicii mai scumpe,de calitate superioara
#astfel,acestia vor contribui la cresterea economica a tarii,in mod indirect,prin achizitiile facute

#consum fincal,conform definitiei preluate de pe EUROSTAT 
#Private consumption expenditure consists of expenditure incurred for the direct satisfaction of individual or collective needs by private households or non-profit institutions serving households (such as religious societies, sports and other clubs, political parties, etc.).													

reg5_ed_p=lm(PIB~ani_scoala+rata_ocupare+consum_final,data=educ_PIB_RO) #folosind rata de ocupare
summary(reg5_ed_p) 

# In mod surprinzator,in urma regresiei multiple cu cei 3 termeni independenti,doar consumul final este semnificativ dpdv statistic,resul neffind luati in considerare.
# Astfel,putem interpreta acest lucru astfel: de fapt,consul final nu este conditionat de nivelul acadmeic al unei persoane sau de valoarea baneasca a salariului,
# ci mai degraba de necesitatea de a consuma si de a-si satisface nevoile.
# Astfel,dinamica PIB-ului va lua in considerare toti indivizii tarii care participa la consum prin achizita diferitelor servcii si produse,care astfel contribuie la cresterea ecoomica in sens larg a tarii.
# 
# In concluzie,... 


#Posibilitate de prognoza pe termen scurt...? 
#RETEA NEURONALA ARTIFICIALA ?!?!?!? 
library(tsibble)
library(readxl)
library(fpp3)
library(fpp2)

#putem incerca forecats cu seria de tiump a Romaniei
ro_ts=ts(educ_PIB_RO)
View(ro_ts)

# transform data to stationarity
diffed = diff(ro_ts, differences = 1)

# #scalling data 
# hist(educ_PIB_RO$ani_scoala)
# hist(educ_PIB_RO$rata_ocupare)
# #normalize
# educ_PIB_RO$ani_scoala=(educ_PIB_RO$ani_scoala-min(educ_PIB_RO$ani_scoala))/(max(educ_PIB_RO$ani_scoala)-min(educ_PIB_RO$ani_scoala))
# hist(educ_PIB_RO$ani_scoala)
# 
# hist(educ_PIB_RO$ani_scoala)
# hist(educ_PIB_RO$rata_ocupare)
# hist(educ_PIB_RO$consum_final)
# #normalize
# educ_PIB_RO$rata_ocupare=(educ_PIB_RO$rata_ocupare-min(educ_PIB_RO$rata_ocupare))/(max(educ_PIB_RO$rata_ocupare)-min(educ_PIB_RO$rata_ocupare))
# hist(educ_PIB_RO$rata_ocupare)
# 
# #normalize
# educ_PIB_RO$consum_final=(educ_PIB_RO$consum_final-min(educ_PIB_RO$consum_final))/(max(educ_PIB_RO$consum_final)-min(educ_PIB_RO$consum_final))
# hist(educ_PIB_RO$consum_final)


#SAMPLING DATA
set.seed(12345)
inp=sample(2,nrow(educ_PIB_RO),replace=TRUE,prob=c(0.7,0.3))
training=educ_PIB_RO[inp==1,]
test=educ_PIB_RO[inp==2,]

#fitting neural network 
library(neuralnet)
n=neuralnet(PIB~ani_scoala+rata_ocupare+pop_rez+consum_final,data=training,hidden=3,err.fct = "sse",
            linear.output = FALSE,
            lifesign = 'full',
            rep = 2,
            algorithm = "rprop+",
            stepmax = 100)

plot(n,rep=1) #ambele output-uri au aceeasi valoare a erorii 

#generarea erorilor in cadrul retelei neuronale
n$result.matrix

#predictie 
pred=compute(n,rep=1,training[,-1])
pred$net.result
head(training[1,])

#Then, we round up our results using compute() method and 
#create a confusion matrix to compare the number of true/false positives and negatives. We will form a confusion matrix with training data

output=compute(n,rep=1,training[,-1])
p1=output$net.result
pred1=ifelse(p1>0.5,1,0)
tab1=table(pred1,training$PIB)
tab1

1 - sum(diag(tab1)) / sum(tab1) #missclasification error de 75%,inseamna ca trebuie sa umblam la nr de starturi 

#pentru un mai bun rezultat,se poate sa folosim log(PIB) 
lgPIB=log(educ_PIB_RO$PIB)
lgPIB
educ_PIB_RO=cbind(educ_PIB_RO,lgPIB)
educ_PIB_RO

#aceleasi modele de regresie liniara,folosind variabila log(PIB)
#putem observa diferente,daca exista evident

reg1_ed_plog=lm(lgPIB~ani_scoala,data=educ_PIB_RO)
summary(reg1_ed_p) #in mod evident,rezultatele sunt identice,valoarea PIB-ului fiind aceeasi,indiferent daca este logaritmata sau nu 


#RNA IN REGRESIE PT PIB
date=educ_PIB_RO
set.seed(123)
install.packages("caTools")	
library(caTools)
#eliminam var calitative
date1=date[,-1]
split = sample.split(date$PIB, SplitRatio = 0.75)
setantrenare<-subset(date, split==TRUE)
View(setantrenare)
settestare<-subset(date, split==FALSE)
View(settestare)
maxim=apply(date1,2,max)
minim=apply(date1,2,min)
maxim
minim

#standardizare max-min
datestand<-as.data.frame(scale(date1, center=minim, scale=maxim-minim))
View(datestand)
 library(neuralnet)
antrenareretea<-subset(datestand, split==TRUE)
antrenareretea
testareretea<-subset(datestand, split==FALSE)
testareretea

#consideram o retea cu inputurile(var independente):ani_scoala,rata_ocupare,consum_final , iar output-ul evident PIB-ul 
RNA_PIB=neuralnet(PIB~ani_scoala+rata_ocupare+pop_rez+consum_final,antrenareretea,hidden=4,linear.output = T)
plot(RNA_PIB)
#in plot se poate observa ca ponerile sunt calculate conform algoritmului de "backpropagation" - iar cu albastru este reprezentat bias-ul 
#ca explicatie pentru existanta bias-ului pot fi factorii externic care compun dinamica pib-ului si pe care nu i-am luat in considerare in compunerea acestui model 

#mai departe,consideram o predictie pentru dinamica pib-ului tarii
#folosind setul de tastare

predictie<-compute(RNA_PIB, testareretea)
predictie$net.result
head((predictie$net.result))
 #aceste valori obtinute in urma predictiei vor fi aduse la cele normale,pentru a putea fi comparate in termeni reali(in repsectivele unitati de masura)
predictie=(predictie$net.result* (max(date$PIB)-min(date$PIB)))+min(date$PIB)
predictie
#am obtinut cele 25% din valori previzonate in mil euro(care pot fi mai mult sau mai putin aproape de realitate)

#vizualizare valori reale vs valori previzionate
plot(settestare$PIB, predictie, col= "red", pch=12, ylab= "Valori previzionate de retea ", xlab= "Valori reale ")
abline(0,1)
#DEOARECE nr de valori este foarte mic,un astfel e grafic nu este foarte specifi si relevant in termeni reali,ci poate fi considerat doar un model pur demonstrativ
#in viata reala,nu este realista o prognoza a PIB-ului pentru o perioada mai mare de 1 an,intrucat apar factori esterni sau situatii extraordinare care vor avea un impact fantastic asupra dinamicii cresterii economice
#acesti factori si situatii extraordinare,de obicei,nu pot fi prevazute sau amploarea lor nu poate fi anticipata cu exactitate(vezi evenimentele din ultimii ani:Pandemia COVID,razboiul de la granita)

#tot in mod experimental,putem deetermina eroarea de predictie RMSE (Root Mean Square Error) ca fiind deviatia standard a reziduurilor
#putem observa in ce masura cat de imprastiate sunt aceste valori fata de dreapta regresiei 

eroare=(sum(settestare$PIB-predictie)^2/nrow(settestare))^0.5 
eroare
#vizual,putem compara valorile din predictie cu cel reale
df_pred=data.frame(settestare$PIB, predictie) #rezultatele obtinute pot reflecta mai mult sau mai putin apropierea lor fata de valorile reale
df_pred


dane <- read.csv("bmt.csv")
head(dane)

dane.red <- data.frame(dane, event=ifelse(dane$first_e==0,0,1),
                       odrzut=ifelse(dane$first_e==1,1,0), 
                       nawrot=ifelse(dane$first_e==2,1,0),
                       zgon=ifelse(dane$first_e==3,1,0))

install.packages("cmprsk")
library(cmprsk)

# Najpierw użyjemy metody K-M dla poszczegolnych typów zdarzeń. 
odrzut_KM <- survfit(Surv(first_t, first_e==1)~1, data = dane.red)
nawrot_KM <- survfit(Surv(first_t, first_e==2)~1, data = dane.red)
zgon_KM <- survfit(Surv(first_t, first_e==3)~1, data = dane.red)

plot(odrzut_KM, mark.time=FALSE, conf.int=FALSE, col=2, xscale=1,
     xlab=" ")
lines(nawrot_KM, mark.time=FALSE, conf.int=FALSE, col=3)
lines(zgon_KM, mark.time=FALSE, conf.int=FALSE, col=4)
#Krzywe K-M wyglądaja nieciekawie,
# ponieważ ten estymator zdarzenia innych typów 
# traktuje jako cenzurowania.
#liczności poszczególnych zdarzeń
sum(dane.red$nawrot)
sum(dane.red$zgon)
sum(dane.red$odrzut)
#Mało zdarzeń typów 2 i 3, 86% zdarzeń to odrzut.

#Szacowanie funkcji sub-dystrybuanty 
ci <- survfit(Surv(first_t, first_e, type="mstate")~1,
              data = dane.red)

plot(ci, mark.time=FALSE, conf.int=FALSE,
     col=c(2,3,4) , xscale=1,
     xlab="Funkcje sub-dystrybunaty")

#oszacowanie sub-dystrybuant przy pomocy testu Gray'a.
#Zilustrujemy tę możliwość rozważająć grupy zdefiniowane
#dwiema metodami pobierania komórek (z krwi obwodowej oraz miednicy).





attach(dane.red)
ci.trt <- cuminc(first_t, first_e, group=trt)
print.cuminc(ci.trt)
print(ci.trt, ntp=2) #to samo tylko skrócona wersja
#Wyniki testu wskazują na brak istotnych różnic między 
#sub-dystrybuantami dla metody leczenia 
#dla kolejnych typów zdarzeń. 
plot(ci.trt, curvlab=c("Tradycyjna,Odrzut", 
                       "Nowa,Odrzut",
                       "Tradycyjna,Nawrot",
                       "Nowa,Nawrot",
                       "Tradycyjna,Zgon",
                       "Nowa,Zgon"))

     #legend("bottomright")
#Marcin zrób ładnie ten wykres :D 

str(ci.trt)

odrzut.trt <- list(ci.trt$"0 1", ci.trt$"1 1")
plot.cuminc(odrzut.trt, curvlab=c("Tradycyjna", "Nowa"), lty=1:2, xlab="ładny napis", ylab="CIF")

nawrot.trt <- list(ci.trt$"0 2", ci.trt$"1 2")
plot.cuminc(nawrot.trt, curvlab=c("Tradycyjna", "Nowa"), lty=1:2, xlab="ładny napis", ylab="CIF")

zgon.trt <- list(ci.trt$"0 3", ci.trt$"1 3")
plot.cuminc(zgon.trt, curvlab=c("Tradycyjna", "Nowa"), lty=1:2, xlab="ładny napis", ylab="CIF")
# a teraz patrz - będzie prościej! :D 

#Szacujemy sub-dystrybuanty:
ci.sfit <- survfit(Surv(first_t, event)~trt, etype=first_e, data=dane.red)
plot(ci.sfit, lty=c(1,0,0,2,0,0), col=1:2, mark.time=F, conf.int=F, xscale=1, xlab="Ładny napis")
text(2, 0.7,"Tradycyjna, Odrzut", col=1)
text(0.3, 0.85,"Nowa, Odrzut", col=2)

plot(ci.sfit, lty=c(0,1,0,0,2,0), col=1:2, mark.time=F, conf.int=F, xscale=1, xlab="Ładny napis")
text(1.3, 0,"Tradycyjna, Nawrot", col=1)
text(0.3, 0.15,"Nowa, Nawrot", col=2)

plot(ci.sfit, lty=c(0,0,1,0,0,2), col=1:2, mark.time=F, conf.int=F, xscale=1, xlab="Ładny napis")
text(0.6, 0.18,"Tradycyjna, Zgon", col=1)
text(1.7, 0,"Nowa, Zgon", col=2)


# punkt 10
summary(coxph.odrzut <- 
          coxph(Surv(first_t, first_e == 1) ~ diag+trt+age,
                            data = dane.red))
summary(coxph.nawrot <- 
          coxph(Surv(first_t, first_e == 2) ~ diag+trt+age,
                          data = dane.red))
summary(coxph.zgon <- 
          coxph(Surv(first_t, first_e == 3) ~ diag+trt+age,
                          data = dane.red))


# to samo dla zmiennej DIAG co dla zmiennej TRT
ci.diag <- cuminc(first_t, first_e, group=diag)
print.cuminc(ci.diag)
print(ci.diag, ntp=2) #to samo tylko skrócona wersja
#Wyniki testu wskazują na wystepowanie istotnych statystycznie
# różnic między 
#sub-dystrybuantami dla metody typu choroby (ostra i przewlekla)
#dla 1 typu zdarzen (nawrotu).  + intepreteacja wspolczynnika
plot(ci.diag, curvlab=c("Ostra,Odrzut", 
                       "Przewlekla,Odrzut",
                       "Ostra,Nawrot",
                       "Przewlekla,Nawrot",
                       "Ostra,Zgon",
                       "Przewlekla,Zgon"))



# punkt 11

model.matrix(~ diag+trt+age)[,-1] -> m.dane.red

summary(mod.odrzut <- crr(ftime=first_t, 
                        fstatus=first_e,
                        cov1= m.dane.red,
                        #cov2 = trt,
                        failcode=1))
#istotny wsolczynnik przy diag
summary(mod.nawrot <- crr(ftime=first_t, 
                          fstatus=first_e,
                          cov1= m.dane.red,
                          #cov2 = trt,
                          failcode=2))
#nieisitoene wszyststko
summary(mod.zgon <- crr(ftime=first_t, 
                          fstatus=first_e,
                          cov1= m.dane.red,
                          #cov2 = trt,
                          failcode=3))
#nieisitoene wszyststko:
# Marcinku pamietaj: wspolczynniki sa nieistoent
# w modelowaniu zgonu jako pierwszego zdarzenia
# Kocham Cie :*:*:*:*:*:*


# skoro odrzuty cos tam istotne, to moze wykresik
# pokazujacy ro\nice dla odrzutow
#12. Konstruujemy oszacowania funkcji skumulowanych częstości odpowiadających modelowi PH dla
#funkcji hazardu sub-dystrybuanty dla odrzutow

odrzut.pred <- predict(mod.odrzut,cov1=rbind(0,1))
plot(odrzut.pred,lty=1:2,color=1:2)
text(0.6, .275, "WW, AIDS", col=1)
text(0.8, .15, "WM, AIDS", col=2)




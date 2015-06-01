dane <- read.csv("bmt.csv")


dane.red <- data.frame(dane, event=ifelse(dane$first_e==0,0,1), odrzut=ifelse(dane$first_e==1,1,0), 
                       nawrot=ifelse(dane$first_e==2,1,0), zgon=ifelse(dane$first_e==3,1,0))

#install.packages("cmprsk")
library(cmprsk)

# Najpierw użyjemy metody K-M dla poszczegolnych typów zdarzeń. 
odrzut_KM <- survfit(Surv(first_t, first_e==1)~1, data = dane.red)
nawrot_KM <- survfit(Surv(first_t, first_e==2)~1, data = dane.red)
zgon_KM <- survfit(Surv(first_t, first_e==3)~1, data = dane.red)

plot(odrzut_KM, mark.time=FALSE, conf.int=FALSE, col=2, xscale=1, xlab="Marcinek zrób piękny tytuł :) <3")
lines(nawrot_KM, mark.time=FALSE, conf.int=FALSE, col=3)
lines(zgon_KM, mark.time=FALSE, conf.int=FALSE, col=4)
#Krzywe K-M wyglądaja nieciekawie, ponieważ ten estymator zdarzenia innych typów traktuje jako cenzurowania.
#liczności poszczególnych zdarzeń
sum(dane.red$nawrot)
sum(dane.red$zgon)
sum(dane.red$odrzut)
#Mało zdarzeń typów 2 i 3, 86% zdarzeń to odrzut.

#Szacowanie funkcji sub-dystrybuanty 
ci <- survfit(Surv(first_t, first_e, type="mstate")~1, data = dane.red)

plot(ci, mark.time=FALSE, conf.int=FALSE, col=c(2,3,4) , xscale=1, xlab="Marcinek zrób piękny tytuł :) <3")

#oszacowanie sub-dystrybuant przy pomocy testu Gray'a. Zilustrujemy tę możliwość rozważająć grupy zdefiniowane
#dwiema metodami pobierania komórek (z krwi obwodowej oraz miednicy).
attach(dane.red)
ci.trt <- cuminc(first_t, first_e, group=trt)
print.cuminc(ci.trt)
print(ci.trt, ntp=2) #to samo tylko skrócona wersja
#Wyniki testu wskazują na brak istotnych różnic między sub-dystrybuantami dla metody leczenia 
#dla kolejnych typów zdarzeń. 

plot(ci.trt, curvlab=c("Tradycyjna,Odrzut", "Nowa,Odrzut", "Tradycyjna,Nawrot", "Nowa,Nawrot", "Tradycyjna,Zgon", "Nowa,Zgon"))
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


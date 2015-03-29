install.packages("foreign")
library(foreign)
dane <- read.dta("gbcs_short.dta")


library(survival)
names(dane)




# dla ciaglych zmiennych pamietac o wykresie pusty model(reszty) vs zmienna ciagla
# sugestia logarytmow


plot(survfit(Surv(rectime,censrec)~meno, data = dane), col=1:2) # model warstwowy
plot(survfit(Surv(rectime,censrec)~horm, data = dane), col=1:2)
plot(survfit(Surv(rectime,censrec)~prog, data = dane), col=1:2)
plot(survfit(Surv(rectime,censrec)~estr, data = dane), col=1:2)
 # pierwsza zmienna - przecinaja sie krzywe przezycia (niespelnione zalozenia ph), proponuje model warstwowy
# kolejne 3 zmienne nie maja przecinajacych sie zmiennych, co nie jest sprzeczne z zalozeniami modelu ph
# ale dobrze popatrzec na transformacje log(-log)

# transformacje log(-log) dla krzywych przezycia
plot(survfit(Surv(rectime,censrec)~meno, data = dane), col=1:2, fun=function(x) log(-log(x)), log="x", firstx=1) # model warstwowy
plot(survfit(Surv(rectime,censrec)~horm, data = dane), col=1:2, fun=function(x) log(-log(x)), log="x", firstx=1) # model warstwowy
plot(survfit(Surv(rectime,censrec)~prog, data = dane), col=1:2, fun=function(x) log(-log(x)), log="x", firstx=1) # model warstwowy
plot(survfit(Surv(rectime,censrec)~estr, data = dane), col=1:2, fun=function(x) log(-log(x)), log="x", firstx=1) # model warstwowy
plot(survfit(Surv(rectime,censrec)~grade, data = dane), col=1:3, fun=function(x) log(-log(x)), log="x", firstx=1) # model warstwowy



+ dodac testy logrank

plot(survfit(Surv(rectime,censrec)~grade, data = dane), col=1:3)
# nie przecinaja sie za bardzo wiec nie jest to sprzeczne z zalozeniami ph - proponuje test logrank dla trendu
# z racji na naturalne uporzadkowanie zmiennej - w celu sprawdzenia zalozen ph


# + testy shenfelda dla sprawdzenia czy sa 


# testy logrank

survdiff(Surv(rectime,censrec)~meno, data = dane)  # p= 0.597 - z poprzednich wnioskow 
survdiff(Surv(rectime,censrec)~horm, data = dane)  # p= 0.00343 
survdiff(Surv(rectime,censrec)~prog, data = dane)  # p= 2.17e-12 
survdiff(Surv(rectime,censrec)~estr, data = dane) # p= 0.000158 

### test log rank dla trendu dla zmiennej grade !!! SASIK



# test na stalosc wspolczynnika w czasie

model <- coxph(Surv(rectime,censrec)~horm+prog+estr+grade+strata(meno)+log(size)+log(nodes), data = dane) 

cox.zph(model, transform = "identity")
# horm       -0.00797 0.0190 0.891
# prog        0.03575 0.3757 0.540
# estr        0.07071 1.4827 0.223
# grade      -0.08326 1.8263 0.177
# log(size)   0.00840 0.0198 0.888
# log(nodes) -0.04936 0.7986 0.372
# GLOBAL           NA 9.5986 0.143

#wskazane jest sprawdzenie wykresu skalowanych reszt schoenfelda, odpowiadajacego testowi

pdf( file = "skal_res_shen.pdf", onefile=FALSE)

par(mfrow=c(2,3))
plot(cox.zph(model, transform = "identity"), df=4,nsmo=10, se=TRUE)
dev.off()

# dolaczone granice dla 95% obszaru ufnosci nie daja podstawy do odrzuceni hipotezy,ze krzywa
# rozni sie od horyzontalnej 

# wykres i wynik tetsu nie sugreuja odstepst od zalozenia ph

# p-wartosci wieksze od 0,05 tym bardziej globalne, co mowi, ze nie ma podstaw do odrzucenia
# hipotezy zerowej o tym, ze wspolczynniki sa stale w czasie


summary(model)
# testy likelihood, wald test, score daja p-wartosc mniejsza od 0,05
# co swiadczy o tym, ze model jest lepszy od modelu pustego
# istotne zmienne majace wplyw na czas przezycia to: horm, prog, log(nodes)

# sprawdzenie dopasowania:
# reszty
par(mfrow=c(1,1))
library(ggplot2)
library(ggthemes)

# reszty dewiancji a liniowa kombinacja zmiennych
plot(predict(model, type="lp"),residuals(model, type="deviance" ))

# reszty martyngalowe a liniowa kombinacja zmiennych
plot(predict(model, type="lp"),residuals(model))

# reszty martyngalowe a indeks
plot(1:686,residuals(model))

library(ggthemes)
#reszty dewiancji a indeks
qplot(1:686,residuals(model, type="deviance"))+theme_tufte()+xlab("index")
qplot(1:686,residuals(model))+theme_tufte()+xlab("index")

qplot(predict(model, type="lp"),residuals(model, type="deviance" ))+theme_tufte()

qplot(predict(model, type="lp"),residuals(model))+theme_tufte()

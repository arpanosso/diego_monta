# An?lise de Lodo com o experimento de Diego - Monta e Thiago
require(lattice)
require(latticeExtra)
require(agricolae)
require(ExpDes.pt)

dd<-read.table("https://raw.githubusercontent.com/arpanosso/diego_monta/main/dados/LodoDiegoMonta.txt",h=T,sep="\t")
dp<-read.table("https://raw.githubusercontent.com/arpanosso/diego_monta/main/dados/LodoDiegoMontaPR.txt",h=T,sep="\t")
dET<-read.table("https://raw.githubusercontent.com/arpanosso/diego_monta/main/dados/LodoDiegoMontaET.txt",h=T,sep="\t")
names(dp)
# dET$ET[dET$ET>6]<-c(2.5,2)
# EMiss?o total (MG ha-1)

tx<-xtabs(ET~Dose,data=dET)
tm<-tx/(4*3)
tm
tm<-tm[c(6,1,4,5,2,3)]
cores="gray"
barchart(tm,sta=F,
         horizontal=F,
         ylab=expression(paste("Emiss?o Total (",Mg," ",ha^-1,")")),
         xlab="Doses + Tratamentos adicionais",col="gray")+
  layer({
    panel.abline(h=seq(0,4,by=1),lty=2,col="gray50")
  },under=TRUE)

plot(dET$ET~dET$Dose)

# Produtividade
tx<-xtabs(Produtividade~Dose,data=dp)
tm<-tx/4
tm
tm<-tm[c(6,1,4,5,2,3)]
cores="gray"
barchart(tm,sta=F,
         horizontal=F,origin=3000,
         ylab=expression(paste("Produtividade de gr?os (",kg," ",ha^-1,")")),
         xlab="Doses + Tratamentos adicionais",col="gray")+
  layer({
    panel.abline(h=seq(3000,5000,by=500),lty=2,col="gray50")
  },under=TRUE)



# MACRO
tx<-xtabs(MACRO~DOSE+Prof,data=dd)
tm<-tx/4
tm<-tm[c(6,1,4,5,2,3),]
cores<-gray.colors(3)
barchart(tm,sta=F,auto.key=list(title="Profundidade (m)",column=3),
         horizontal=F,
         ylab=expression(paste("Macroporosidade (",m^3,m^-3,")")),
         xlab="Doses + Tratamentos adicionais",
         par.settings=list(superpose.polygon=list(col=cores)))+
  layer({
    panel.abline(h=seq(0,.2,by=0.05),lty=2,col="gray50")
  },under=TRUE)

# MICRO
tx<-xtabs(MICRO~DOSE+Prof,data=dd)
tm<-tx/4
tm<-tm[c(6,1,4,5,2,3),]
cores<-gray.colors(3)
barchart(tm,sta=F,auto.key=list(title="Profundidade (m)",column=3),
         horizontal=F,origin=0.2,
         ylab=expression(paste("Microporosidade (",m^3,m^-3,")")),
         xlab="Doses + Tratamentos adicionais",
         par.settings=list(superpose.polygon=list(col=cores))) +
  layer({
    panel.abline(h=seq(0.2,.32,by=0.02),lty=2,col="gray50")
  },under=TRUE)


# DS
tx<-xtabs(DS~DOSE+Prof,data=dd)
tm<-tx/4
tm<-tm[c(6,1,4,5,2,3),]
cores<-gray.colors(3)
barchart(tm,sta=F,auto.key=list(title="Profundidade (m)",column=3),
         horizontal=F,origin=.9,
         ylab=expression(paste("Densidade do solo (",g," ",cm^-3,")")),
         xlab="Doses + Tratamentos adicionais",
         par.settings=list(superpose.polygon=list(col=cores))) +
  layer({
    panel.abline(h=seq(1,1.6,by=0.2),lty=2,col="gray50")
  },under=TRUE)


# PT
tx<-xtabs(PT~DOSE+Prof,data=dd)
tm<-tx/4
tm<-tm[c(6,1,4,5,2,3),]
cores<-gray.colors(3)
barchart(tm,sta=F,auto.key=list(title="Profundidade (m)",column=3),
         horizontal=F,origin = .2,
         ylab=expression(paste("Porosidade Total (",m^3,m^-3,")")),
         xlab="Doses + Tratamentos adicionais",
         par.settings=list(superpose.polygon=list(col=cores))) +
  layer({
    panel.abline(h=seq(0.2,.45,by=0.05),lty=2,col="gray50")
  },under=TRUE)


# An?lise de Vari?ncia
prof<-levels(dd$Prof)
names(dd)

for(j in 4:length(dd)){
for(i in 1:3){
  print("========================================")
  print(paste("Vari?vel",names(dd[j]),"Prof =",prof[i]))
  print("=========================================")
  f<-dd$Prof==prof[i]
  trat<-dd$DOSE[f]
  bloco<-dd$REP[f]
  y<-dd[f,j]
  dbc(trat,bloco,y)
  
  mc<-matrix(c(0,4,0,0,0,
               -1,-1,1,-1,-3,
               -1,-1,3,1,1,
               -1,-1,-3,1,-1,
               -1,-1,-1,-1,3,
               4,0,0,0,0),ncol=5,byrow = T)
  colnames(mc)<-c("ADxDO","TESTxDO","Lin","Qua","Cub")
  mc
  tratC<-trat
  contrasts(tratC)<-mc
  mod<-aov(y~tratC)
  print("------------- An?lise de Contrastes --------------")
  print(summary.lm(mod))
  }
}

print("=========================================")
print("An?lise para a Produtividade de Gr?os")
print("=========================================")

trat<-dp$Dose
bloco<-dp$Bloco
y<-dp$Produtividade
dbc(trat,bloco,y)
mc<-matrix(c(0,4,0,0,0,
             -1,-1,1,-1,-3,
             -1,-1,3,1,1,
             -1,-1,-3,1,-1,
             -1,-1,-1,-1,3,
             4,0,0,0,0),ncol=5,byrow = T)
colnames(mc)<-c("ADxDO","TESTxDO","Lin","Qua","Cub")
mc
tratC<-trat
contrasts(tratC)<-mc
mod<-aov(y~tratC)
print(summary.lm(mod))

print("=========================================")
print("An?lise para a Emiss?o total")
print("=========================================")

trat<-dET$Dose
bloco<-dET$Bloco
y<-dET$ET
dbc(trat,bloco,y,sigT = .3)
mc<-matrix(c(0,4,0,0,0,
             -1,-1,1,-1,-3,
             -1,-1,3,1,1,
             -1,-1,-3,1,-1,
             -1,-1,-1,-1,3,
             4,0,0,0,0),ncol=5,byrow = T)
colnames(mc)<-c("ADxDO","TESTxDO","Lin","Qua","Cub")
mc
tratC<-trat
contrasts(tratC)<-mc
mod<-aov(y~tratC)
print(summary.lm(mod))




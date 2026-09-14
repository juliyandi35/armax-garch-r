# Uji Kormogorov-smirnov
#ARMAX1
resBBCA1=residuals(garch.fit.BBCA1)
resBBRI1=residuals(garch.fit.BBRI1)
resBMRI1=residuals(garch.fit.BMRI1)
resBBNI1=residuals(garch.fit.BBNI1)
#ARMAX2
resBBCA2=residuals(garch.fit.BBCA2)
resBBRI2=residuals(garch.fit.BBRI2)
resBMRI2=residuals(garch.fit.BMRI2)
resBBNI2=residuals(garch.fit.BBNI2)
#ARMAX3
resBBCA3=residuals(garch.fit.BBCA3)
resBBRI3=residuals(garch.fit.BBRI3)
resBMRI3=residuals(garch.fit.BMRI3)
resBBNI3=residuals(garch.fit.BBNI3)
ks.test(resBBCA1,"pnorm",alternative=c("two.sided"))
ks.test(resBBCA2,"pnorm",alternative=c("two.sided"))
ks.test(resBBCA3,"pnorm",alternative=c("two.sided"))
ks.test(resBBRI1,"pnorm",alternative=c("two.sided"))
ks.test(resBBRI2,"pnorm",alternative=c("two.sided"))
ks.test(resBBRI3,"pnorm",alternative=c("two.sided"))
ks.test(resBMRI1,"pnorm",alternative=c("two.sided"))
ks.test(resBMRI2,"pnorm",alternative=c("two.sided"))
ks.test(resBMRI3,"pnorm",alternative=c("two.sided"))
ks.test(resBBNI1,"pnorm",alternative=c("two.sided"))
ks.test(resBBNI2,"pnorm",alternative=c("two.sided"))
ks.test(resBBNI3,"pnorm",alternative=c("two.sided"))

# Uji Lagrange Multiplier Residual
resBBCA=resBBCA1
resBBRI=resBBRI3
resBBNI=resBBNI3
resBMRI=resBMRI3
library(FinTS)
hasil.BBCA=matrix(0,10,2)
colnames(hasil.BBCA)=c('chi-sq','p-value')
for (i in 1:10){
  LM.BBCA=ArchTest(resBBCA,lags=i)
  hasil.BBCA[i,1]=LM.BBCA$statistic
  hasil.BBCA[i,2]=LM.BBCA$p.value
}
hasil.BBRI=matrix(0,10,2)
colnames(hasil.BBRI)=c('chi-sq','p-value')
for (i in 1:10){
  LM.BBRI=ArchTest(resBBRI,lags=i)
  hasil.BBRI[i,1]=LM.BBRI$statistic
  hasil.BBRI[i,2]=LM.BBRI$p.value
}
hasil.BMRI=matrix(0,10,2)
colnames(hasil.BMRI)=c('chi-sq','p-value')
for (i in 1:10){
  LM.BMRI=ArchTest(resBMRI,lags=i)
  hasil.BMRI[i,1]=LM.BMRI$statistic
  hasil.BMRI[i,2]=LM.BMRI$p.value
}
hasil.BBNI=matrix(0,10,2)
colnames(hasil.BBNI)=c('chi-sq','p-value')
for (i in 1:10){
  LM.BBNI=ArchTest(resBBNI,lags=i)
  hasil.BBNI[i,1]=LM.BBNI$statistic
  hasil.BBNI[i,2]=LM.BBNI$p.value
}
hasil.BBCA
hasil.BBRI
hasil.BMRI
hasil.BBNI



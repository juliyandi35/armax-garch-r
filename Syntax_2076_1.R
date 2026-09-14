library(readxl)
d=read_excel('Return.xlsx')
Y1=d[,10]
Y2=d[,11]
Y3=d[,12]
Y4=d[,13]
X=d[,14]
alfa=0.05
n=length(as.matrix(Y1))
window=c(250,375,500)
beta1=0
beta2=0
beta3=0
beta4=0
pval1=0
pval2=0
pval3=0
pval4=0
a=0
b=0
c=0
d=0
for (window in window){
  for (i in window:n){
    Y1a=Y1$BBCA_RT[(i-window+1):i]
    Y2a=Y2$BBRI_RT[(i-window+1):i]
    Y3a=Y3$BMRI_RT[(i-window+1):i]
    Y4a=Y4$BBNI_RT[(i-window+1):i]
    Xa=X$IHSG_RT[(i-window+1):i]

    reg1=lm(Y1a~0+Xa)
    beta1[(i-window+1):(n-window+1)]=reg1$coefficients
    pval1[(i-window+1):(n-window+1)]=summary(reg1)$coefficients[,4]
    a[(i-window+1):(n-window+1)]=if(pval1[(i-window+1)] < alfa) "Signifikan" else "Tidak Signifikan"
    reg2=lm(Y2a~0+Xa)
    beta2[(i-window+1):(n-window+1)]=reg2$coefficients
    pval2[(i-window+1):(n-window+1)]=summary(reg2)$coefficients[,4]
    b[(i-window+1):(n-window+1)]=if(pval2[(i-window+1)] < alfa) "Signifikan" else "Tidak
Signifikan"
    reg3=lm(Y3a~0+Xa)
    beta3[(i-window+1):(n-window+1)]=reg3$coefficients
    pval3[(i-window+1):(n-window+1)]=summary(reg3)$coefficients[,4]
    c[(i-window+1):(n-window+1)]=if(pval3[(i-window+1)] < alfa) "Signifikan" else "Tidak
Signifikan"
    reg4=lm(Y4a~0+Xa)
    beta4[(i-window+1):(n-window+1)]=reg4$coefficients
    pval4[(i-window+1):(n-window+1)]=summary(reg4)$coefficients[,4]
    d[(i-window+1):(n-window+1)]=if(pval4[(i-window+1)] < alfa) "Signifikan" else "Tidak Signifikan"
  }

  plot(beta1,col="blue", type="l",lwd=2, ylim=c(0.5,2.5),
       ylab="Beta",xlab="Time")
  lines(beta2,col="red", type="l",lwd=2)
  lines(beta3,col="yellow", type="l",lwd=2)
  lines(beta4,col="black", type="l",lwd=2)
  legend("topright",c("BBCA","BBRI","BMRI","BBNI"),lwd=2,
         col=c("blue","red","yellow","black","green"),bg="white"
  )
}

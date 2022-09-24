log.mod<-function(psi,id,xidep) {
  age<-xidep[,1]
  int<-psi[id,1]
  sl1<-psi[id,2]
  sl2<-psi[id,3]
  return(int+sl1*age+sl2*log(age))
}

linear.mod<-function(psi,id,xidep) {
  age<-xidep[,1]
  int<-psi[id,1]
  sl1<-psi[id,2]
  return(int+sl1*age)
}

poly2.mod<-function(psi,id,xidep) {
  age<-xidep[,1]
  int<-psi[id,1]
  sl1<-psi[id,2]
  sl2<-psi[id,3]
  return(int+sl1*age+sl2*(age**2))
}

exp.mod<-function(psi,id,xidep) {
  age<-xidep[,1]
  int<-psi[id,1]
  bp<-psi[id,2]
  k<-psi[id,3]
  return(int+bp*(1-exp(-k*age)))
}

kouchi.mod<-function(psi,id,xidep) {
  age<-xidep[,1]
  int<-psi[id,1]
  bp<-psi[id,2]
  cp<-psi[id,3]
  return(int+bp*(age**cp))
}

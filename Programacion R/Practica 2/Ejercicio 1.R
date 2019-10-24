
fibonacci <- function(n) {
    if (n < 1) {
        stop("Proporcione valores enteros positivos para 'n'") #Ya que la serie Fibonacci comienza con el cero y no puede tomar valores negativos   
    }
    
    if (n < 3) {
        return(c(0, 1)[1:n])
    } else {
        
        fibonacciSeq <- numeric(n)
        fibonacciSeq[2] <- 1
        for (i in 3:n) {
            fibonacciSeq[i] <- fibonacciSeq[i - 2] + fibonacciSeq[i - 1]
        }
        return(fibonacciSeq)
    }
}
fibonacci(15)


#Ahora, buscaremos el generador de Ternas con la siguiente funcion


ternas<-function(n){
    x<-c(fibonacci(n+4))
    x<-x[-1]
    m<-matrix(nrow=n,ncol=3) 
    for (i in 1:n) {
        v1<-x[i]
        v2<-x[i+1]
        v3<-x[i+2]
        v4<-x[i+3]
        a<- v1*v4
        b<-2*v2*v3
        h<-v2*v2+v3*v3
        m[i,]<-c(a,b,h) 
        
    }
    print(m) 
}
ternas(20)


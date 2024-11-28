ozone_data <- read.table("ozone.txt", header = TRUE)


x <- ozone_data$maxO3v
y <- ozone_data$maxO3
n<-size(x)

#définition de la fonction de knn
knn_predict <- function(x_train, y_train, x_test, k) {
  sapply(x_test, function(x0) {
    distances <- abs(x_train - x0)
    neighbors <- order(distances)[1:k]
    mean(y_train[neighbors])
  })
}
par(mfrow = c(2, 2))
k_values_exemple=c(1,3,10,49)
for (k in k_values_exemple) {
  x_sorted<-sort(x)
  predictions <- knn_predict(x, y_sampled, x_sorted, k)
  plot(x, y, main = paste("k =", k), xlab = "maxO3v", ylab = "maxO3", pch = 1, col = "blue")
  lines(x_sorted, predictions, col = "orange", lwd = 2)
}



#Etape de cross validation pour trouver le k_optimal

crossval=function(x_train,y_train,k){
  n=length(x_train)
  indices=rep(1:n)
  Rh=0
  for (i in indices) {
    new_data_x_t=x_train[-i]
    new_data_y_t=y_train[-i]
    new_data_x_test=x_train[i]
    
    psi_without_i=knn_predict(new_data_x_t, new_data_y_t, new_data_x_test, k)
    
    Rh=Rh+ (y_train[i]-psi_without_i)^2
  }
  return(Rh/n)
}

k_values <- rep(1:40)
RMSE <- k_values*0;
Biais<-k_values*0;
var<-k_values*0;
y_sampled<-y + rnorm(n, sd = 1)
for (k in k_values) {
  
  x_sorted<-sort(x)
  predictions <- knn_predict(x, y_sampled, x_sorted, k)
  RMSE[k]=crossval(x,y,k)
  Biais[k] <- mean((predictions - y)^2)
  var[k] <- mean(var(predictions))
}
par(mfrow = c(1,1))
#Cross-validation pour k_optimal
plot(k_values,RMSE,xlab = "Nombe de plus proches voisins k",ylab=expression(hat(R)(k)))
lines(k_values,RMSE)

#Comparaison modèle linaire / KNN

K<-5
x_sorted<-sort(x)
lineaire_model=lm(y~x,data=ozone_data)

model_lin=predict(lineaire_model)
modele_knn=knn_predict(x,y,x_sorted,K)

plot(x, y,  xlab = "maxO3v", ylab = "maxO3", pch = 1, col = "blue",
     title("Comparaison entre KNN et la régression linéaire"))
lines(x_sorted, modele_knn, col = "orange", lwd = 2)
lines(x,model_lin,col="red",lwd=2)
legend("topleft", legend = c("KNN results, k=5", "Linear Regression results"), col = c("orange", "red"), lty = 1, lwd = 2)



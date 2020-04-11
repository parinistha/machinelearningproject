library("plot3D")
library("dml")
library("scatterplot3d")
library("MASS")

rnames<-c("Age","Year","pos_aux_nodes","survived")
data <- read.csv("haberman.DATA",header=FALSE,col.names = rnames)

l=nrow(data)

alive <- which(grepl(1,data$survived))
dead <- which(grepl(2,data$survived))

simi <- rbind(t(combn(alive, 2)), t(combn(dead, 2)))

temp <-  as.data.frame(t(simi))
tol <- as.data.frame(combn(1:l, 2))

dism <- t(as.matrix(tol[!tol %in% temp]))

result <- GdmDiag(data, simi, dism)
newData <- result$newData

color <- c(1:l)

for (value in color){ 
    if  (data$survived[value]==1){
        color[value] <- "red"
    }
    else {
        color[value] <- "blue"
    }
}
    


par(mfrow = c(2, 1), mar = rep(0, 4) + 0.1)
scatterplot3d(data, color = color, cex.symbols = 0.6,
              xlim = range(data[, 1], newData[, 1]),
              ylim = range(data[, 2], newData[, 2]),
              zlim = range(data[, 3], newData[, 3]),
              main = "Original Data")
# plot GdmDiag transformed data
scatterplot3d(newData, color = color, cex.symbols = 0.6,
              xlim = range(data[, 1], newData[, 1]),
              ylim = range(data[, 2], newData[, 2]),
              zlim = range(data[, 3], newData[, 3]),
              main = "Transformed Data")

plot(newData[,2],newData[,3],col=color)

set.seed(123)

red_data <- data.frame(V1=newData[,2],V2=newData[,3],V3=newData[,4])

smp_size <- floor(0.75 * nrow(red_data))

train_ind <- sample(seq_len(nrow(red_data)), size = smp_size)

train <- red_data[train_ind, ]
test <- red_data[-train_ind, ]

lda_out <- lda(V3 ~.,data=train)

lda_pred<- predict(lda_out,test,type="class")

table(lda_pred$class,test$V3)

confusionMatrix(table(lda_pred$class,test$V3))



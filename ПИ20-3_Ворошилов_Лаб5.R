# Ворошилов Владислав, ПИ20-3

# Task 1
generate.supply <- function(n, min, max){
  return(sample(x = min:max, size = n, replace = T))
}

# Task 2
res <- generate.supply(10, 0, 1000)
plot(x = res, 
     col = "#033b11",
     cex = 1, 
     pch = 16, 
     type = "b", 
     lty = 1, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")


plot(x = res, 
     col = "#06661e",
     cex = 2, 
     pch = 3, 
     type = "o", 
     lty = 4, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")


plot(x = res, 
     col = "#078526",
     cex = 2, 
     pch = 18, 
     type = "s", 
     lty = 5, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")

# Task 3

shop.1 <- generate.supply(30, 0, 10000)
shop.2 <- generate.supply(30, 0, 10000)

# График 1
plot(x = shop.1, 
     col = "#078526",
     cex = 2, 
     pch = 2, 
     type = "o", 
     lty = 5, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")

points(x = shop.2,
       col = "#0432d9",
       cex =2,
       pch = 6,
       )
lines(x = shop.2,
      col = "#0432d9",
      lty = 4,
      lwd = 2)
legend(x = 0,
       y = 10000,
       legend = c("Shop 1", "Shop 2"),
       title = "Shop lines",
       col = c("#078526", "#0432d9"),
       lty = 1:2,
       cex = 0.8,
       bg = "#dce3b1")


# График 2
plot(x = shop.1, 
     col = "black",
     cex = 2, 
     type = "b", 
     lty = 1, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")

points(x = shop.2,
       col = "red",
       cex =2,
)
lines(x = shop.2,
      col = "red",
      lty = 1,
      lwd = 2)
legend(x = 0,
       y = 10000,
       legend = c("Shop 1", "Shop 2"),
       title = "Shop lines",
       col = c("black", "red"),
       lty = 1:2,
       cex = 0.8,
       box.lty = 0)

# Task 4 

shop.1 <- generate.supply(30, 0, 10000)
shop.2 <- generate.supply(30, 0, 10000)
shop.3 <- generate.supply(30, 0, 10000)
shop.4 <- generate.supply(30, 0, 10000)
shop.5 <- generate.supply(30, 0, 10000)

# Грфик 1
plot(x = shop.1, 
     col = "#078526",
     cex = 2, 
     pch = 2, 
     type = "o", 
     lty = 5, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")


points(x = shop.2,
       col = "#0432d9",
       cex =2,
       pch = 6,
)
lines(x = shop.2,
      col = "#0432d9",
      lty = 4,
      lwd = 2)

points(x = shop.3,
       col = "black",
       cex =3,
       pch = 2,
)
lines(x = shop.3,
      col = "black",
      lty = 2,
      lwd = 4)


points(x = shop.4,
       col = "lightblue",
       cex =1,
       pch = 4,
)
lines(x = shop.4,
      col = "lightblue",
      lty = 4,
      lwd = 3)

points(x = shop.5,
       col = "yellow",
       cex = 5,
       pch = 8,
)
lines(x = shop.5,
      col = "yellow",
      lty = 2,
      lwd = 4)


legend(x = 0,
       y = 10000,
       legend = c("Shop 1", "Shop 2", "Shop 3", "Shop 4", "Shop 5"),
       title = "Shop lines",
       col = c("#078526", "#0432d9", "black", "lightblue", "yellow"),
       lty = 1:5,
       cex = 0.5,
       bg = "#dce3b1")


# Грфик 2
plot(x = shop.1, 
     col = "black",
     cex = 2, 
     type = "b", 
     lty = 1, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")

points(x = shop.2,
       col = "red",
       cex =2,
)
lines(x = shop.2,
      col = "red",
      lty = 1,
      lwd = 2)

points(x = shop.3,
       col = "blue",
       cex =3,
       pch = 2,
)
lines(x = shop.3,
      col = "blue",
      lty = 2,
      lwd = 4)


points(x = shop.4,
       col = "lightblue",
       cex =1,
       pch = 4,
)
lines(x = shop.4,
      col = "lightblue",
      lty = 4,
      lwd = 3)

points(x = shop.5,
       col = "yellow",
       cex = 5,
       pch = 8,
)
lines(x = shop.5,
      col = "yellow",
      lty = 2,
      lwd = 4)


legend(x = 0,
       y = 10000,
       legend = c("Shop 1", "Shop 2", "Shop 3", "Shop 4", "Shop 5"),
       title = "Shop lines",
       col = c("black", "red", "blue", "lightblue", "yellow"),
       lty = 1:5,
       cex = 0.5,
       box.lty = 0)

# Task 5
shop.1.sum <- sum(shop.1)
shop.2.sum <- sum(shop.2)
shop.3.sum <- sum(shop.3)
shop.4.sum <- sum(shop.4)
shop.5.sum <- sum(shop.5)

tab <- matrix(c(shop.1.sum, shop.2.sum, shop.3.sum, shop.4.sum, shop.5.sum),
               ncol=5)
colnames(tab) <- c("Shop 1", "Shop 2", "Shop 3", "Shop 4", "Shop 5")
rownames(tab) <- c("Supply")
tab <- as.table(tab)
tab

barplot(tab,
        main = "Поставки за месяц",
        xlab = "Магазины",
        ylab = "Количество, шт.", 
        col=c("darkblue", "red", "black", "#078526", "yellow"),
        beside = TRUE)

# Task 6
generate.sale <- function(in.){
  vec <- unlist(lapply(in., sample, size = 1, replace = F))
  return(vec)
}

# Task  7

shop.1.sales <- generate.sale(shop.1)

plot(x = shop.1, 
     col = "#078526",
     cex = 2, 
     pch = 2, 
     type = "o", 
     lty = 5, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")

points(x = shop.1.sales,
       col = "#0432d9",
       cex =2,
       pch = 6,
)
lines(x = shop.1.sales,
      col = "#0432d9",
      lty = 4,
      lwd = 2)
legend(x = 0,
       y = 10000,
       legend = c("Поставки", "Продажи"),
       title = "Легенда",
       col = c("#078526", "#0432d9"),
       lty = 1:2,
       cex = 0.8,
       bg = "#dce3b1")

# Task 8
shop.2.sales <- generate.sale(shop.2)
shop.3.sales <- generate.sale(shop.3)

plot(x = shop.1, 
     ylim=c(0,15000),
     col = "#078526",
     cex = 2, 
     pch = 2, 
     type = "o", 
     lty = 5, 
     lwd = 2, 
     xlab = "День",
     ylab = "Количество, шт.",
     main = "Ёль искусственная")


points(x = shop.1.sales,
       col = "#0432d9",
       cex =2,
       pch = 6,
)
lines(x = shop.1.sales,
      col = "#0432d9",
      lty = 4,
      lwd = 2)


points(x = shop.2,
       col = "black",
       cex = 3,
       pch = 2,
)
lines(x = shop.2,
      col = "black",
      lty = 5,
      lwd = 2)

points(x = shop.2.sales,
       col = "#red",
       cex = 4,
       pch = 9,
)
lines(x = shop.2.sales,
      col = "red",
      lty = 1,
      lwd = 2)

points(x = shop.3,
       col = "lightblue",
       cex =2,
       pch = 6,
)
lines(x = shop.3,
      col = "lightblue",
      lty = 4,
      lwd = 2)


points(x = shop.3.sales,
       col = "yellow",
       cex = 2,
       pch = 17,
)
lines(x = shop.3.sales,
      col = "yellow",
      lty = 3,
      lwd = 2)



legend(x = 0,
       y = 15000,
       legend = c("Поставки в магазин 1", "Продажи в магазине 1",
                  "Поставки в магазин 2", "Продажи в магазине 2",
                  "Поставки в магазин 3", "Продажи в магазине 3"),
       title = "Легенда",
       col = c("#078526", "#0432d9", "black", "red", "lightblue", "yellow"),
       lty = 1:6,
       cex = 0.8,
       bg = "#dce3b1")

# Task 9 
shop.1.unsaled = shop.1 - shop.1.sales
shop.2.unsaled = shop.2 - shop.2.sales
shop.3.unsaled = shop.3 - shop.3.sales


unsaled <- matrix(c(shop.1.unsaled, shop.2.unsaled, shop.3.unsaled),
              ncol=30, nrow =3, byrow= TRUE)
rownames(unsaled) <- c("Shop 1", "Shop 2", "Shop 3")
# Делам подписи для каждого дня в матрицу
names <- c()
for (i in 1:30){
  names <- append(names, paste("Day ", i))
}
colnames(unsaled) <- names
unsaled <- as.table(unsaled)
unsaled

barplot(unsaled,
        main = "Непроданные товары по дням",
        xlab = "День",
        ylab = "Непроданные товары, шт.",
        col = c("red","green", "blue")
)
legend("topleft",
       c("Shop 1","Shop 2", "Shop 3"),
       fill = c("red","green", "blue")
)


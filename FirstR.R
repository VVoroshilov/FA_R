# Константы
G_MAX <- 10000000 # количество товаров 
M_MAX <- 1000000 #0 количество клиентов 
PRICE_MIN <- 20 # минимальная цена 
PRICE_MAX <- 4000 # максимальная цена 
BUY_MIN <- 1 # количество покупок минимум
BUY_MAX <- 20 # количесво покупок максимум

# Решение в стиле R 
t0 <- unclass(Sys.time()) # Начало

price <- runif(min = PRICE_MIN, max = PRICE_MAX, n = G_MAX)
price <- round(price, 2)
amount.m <- sample(x = BUY_MIN:BUY_MAX, size = M_MAX, replace = T)
payCach.m <- sample(c(T, F, T), size = M_MAX, replace = T)
price.m <- sample(x = price, size = M_MAX, replace = T)
vol <- sum(amount.m*price.m) # Рассчёт общей выручки
vol <- round(vol/1000000, 1) # Перевод в миллионы рублей
vol.cach <- sum(amount.m*price.m*payCach.m) # Рассчёт выручки в наличке
vol.cach <- round(vol.cach/1000000, 1) # Перевод в миллионы рублей
perc <- round(vol.cach/vol*100, 1) # Подсчёт доли наличных платежей


t1 <- unclass(Sys.time()) # Конец

print(paste('Выручка за день, всего = ', vol, ' млн. рублей'))
print(paste('Выручка за день, наличные = ', vol.cach, ' млн. рублей'))
print(paste('Доля наличных в общей выручке', perc, ' %'))
print(paste('Время выполнения программы = ', round(t1-t0, 2), ' сек.'))

{# Ворошилов Владислав ПИ20-3
  #Task 5
  13 %/% 3 %% 3 # 13 div 3 mod 3 = 1 => Сначала целочисленное деление, потом по остатку
  13 %% 3 %/% 3 # 13 mod 3 div 3 = 0 => Сначала по остатку, потом целочисленое деление
  # Операторы имеют одинаковый приоритет
  # Приоритет операций
  # 1. Возведение в степень
  # 2. Деление по остатку и деление по целому
  # 3. Деление и умножение
  # 4. Сложение и вычитание

  
  
  2^5-3*(100 %/% 10) + 12*30/3
  2^6 %/% 3 + 228 %% 13 
  11*3^2 - 7/3*6 + 9%/%4
  123%%11 * 12 + 13^4/1024
  82%/%8*7-2^12%%5
  
  #Task 6
  #Inf
  1/0
  
  #Nan
  0/0
  
  #NA
  x <- 3
  x[2]
  
  #NULL
  arr <- NULL
  arr
  #Task 7 
  var.1 = as.integer(readline('Введите первое число: '))
  var.2 = as.integer(readline('Введите второе число: '))
  answer.1 = var.1^var.2
  answer.2 = var.2^var.1
  answer.3 = var.1 / var.2
  answer.4 = var.1 / 0
  print(paste0('Первое число в степени, равной второму числу: ', answer.1))
  print((paste0('Второе число в степени, равной первому числу: ', answer.2)))
  print((paste0('Результат деления первого числа на второе: ', answer.3)))
  print(paste0('Деление на ноль: ', answer.4))
}
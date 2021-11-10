# Ворошилов Владислав ПИ20-3

  # Task 1
  # Такая громоздкая проверка условия необходима, потому что при использовании 
  #as.double() для логических переменных произойдёт преобразование
pow.and.div <- function(x=1, y=1, z=1){
  if ( !( (is.integer(x) || is.double(x)) && (is.integer(y) || is.double(y)) && (is.integer(z) || is.double(z)) ) ){
    return("Функция ожидает целые или действительные числа")
  }
  else {
    if (z == 0){
      return("Ошибка деления на ноль")
    }
    else{
      result <- (x^y)/z
      return(result)
    }
  }
}

pow.and.div(3,5,9)
pow.and.div(2, T, 10)

# Task 2, 3, 4, 5

week.rus <- function(N = 0, lang = "RU", full = TRUE, isDebug=FALSE){
  
  # N - число или вектор
  # lang - язык вывода
  # full - логическая переменная, хранящая длину вывода. TRUE - полный вывод, FALSE - сокращённый
  
  
  ENG <- c("Eng", "eng", "English", "english","англ", "Англ", "анг")
  result <- ""
  
  # Если N является вектором, то вызываем функцию lapply, которая применяет функцию к каждому элементу вектора
  # иначе просто выполняем функцию (именно так какждый элемент вектора или одиночное значение будет входить в программу)
  # Т.к. is.vector() возвращает TRUE не только для векторов, но и для их значений, то проверяем, чтобы их длина не была равна 1  
  if (is.vector(N) && length(N) != 1){
    return(unlist(lapply(N, week.rus, lang=lang, full=full, isDebug=isDebug)))
  }
  else{
  N <-  as.integer(N)
  flag <- TRUE 
  # Переменная flag понадобится для проверки корректности преобразования числа к целочисленному виду
  
  if (lang %in% ENG){
    lang = "EN"
  }  
  else{
    lang = "RU"
  }
  
  
  if (full){
    week <- switch(lang, 
                 "RU" = c("Понедельник", "Вторник", "Среда", "Четверг", "Пятница", "Суббота", "Воскресенье"),
                 "EN" = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  }
  else{
    week <- switch(lang, 
                     "RU" = c("Пн", "Вт", "Ср", "Чт", "Пт", "Сб", "Вс"),
                     "EN" = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
    }
    
    
    if (is.na(N) || is.nan(N)){
      result <- "Некорректный ввод. Введите число!"
      flag <- FALSE
    }
    
    
    if (1 <= N && flag){
      N <- N %% 7
      result <- week[N]
    }
    else{
      if (result == ""){
      result <- " "
      }
    }
    
    
    if (isDebug){
      return(print(paste0("N = ", N, "result = ", result)))
    }
    else{
      return(result)
    }
  }
}
# Проверка корректности работы с разными вхоными данными
week.rus(5, "рус")
week.rus(12312, "Eng", FALSE)
week.rus(0, "RU")
week.rus(-23)
week.rus("15", 'Deutch', TRUE)
week.rus(N = "5 ", full = FALSE)
week.rus(T)
week.rus(2i)
week.rus(" ")
week.rus("NJ")
week.rus("213-23")
week.rus(c(1, 2, 3, 4))
week.rus(c(-1, 0, "12", 12332, 10.5, "T", "ff"), "Eng", FALSE)




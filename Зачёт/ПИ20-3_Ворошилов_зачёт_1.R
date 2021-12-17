# Ворошилов Владислав ПИ20-3
# Почта: vvsvoroshilov@gmail.com


# Константы
# PRODUCTS - содержит название товаров 
# MAX.SUPPLY и MIN.SUPPLY- значения по умолчанию для функции генерации данных
# Можно сделать несколько значений в виде вектора, 
# а при генерации значений - выбирать любое из них. 
# Для демонстрации в этой работе я буду использовать константное значение 
# MIN.SUPPLY = 0
# MAX.SUPPLY = c(1000, 5000, 10000)
# P_supply, P_sale, P_util из условия


PRODUCTS <- c("Молоко", "Хлеб", "Яблоки")
MAX.SUPPLY <- c(1000, 5000, 10000)
MIN.SUPPLY <- 0
SHOPS.COUNT <- 10
P_supply <- 5500
P_sale <- 8000
P_util <- 400



# Создаём папку с именем студента. В ней создаём папку торговой сети
path.to.script <- getwd()

my.name <- "Ворошилов_Владислав_ПИ20-3"
path.to.dir <- file.path(path.to.script, my.name)
dir.create(path.to.dir)

setwd(path.to.dir)

company.name <- "Микси"
company.dir <- file.path(path.to.dir, company.name)
dir.create(company.dir)

setwd(company.dir)

products.dir.name <- "Товары"
products.dir <- file.path(company.dir, products.dir.name)
dir.create(products.dir)

# Генерация данных

# Функции для генерации данных
# Директории будут иметь формат: ~/Ворошилов_Владислав_ПИ20-3/Микси/имя_товара/Магазин_n

generate.files <- function(dir.name, period = c(1:7), sale.level = -1, file.name = company.name, minimum = MIN.SUPPLY, maximum = MAX.SUPPLY){
  # dir.name - имя директории, для которой создаются файлы. Ожидается путь до папки магазина
  # period - ветор дней, для которых генерируются данные. По умолчанию это вектор от 1 до 7
  # sale.level - уровень продаж. Если его не указывать, то будут сгенерированы рандомные продажи
  # maximum и minimum - максимум и минимум поставок. 
  # По умолчанию имеют значение заданных в начале программы констант. 
  # file.name - имя для файлов. По умолчанию - имя компании, указанное в программе.
  supply <- as.integer(runif(n = length(period), min = minimum, max = maximum))

  if(sale.level == -1){
    # sale.level не указан, поэтому генерируем рандомные продажи
    # умножаем поставки на рандомное число от 0 до 1, что исключает случай, когда продажи > поставки
    sales <- as.integer(supply * runif(n = length(supply), min = 0, max = 1))
  }
  
  else{
    
    if(sale.level <= 100 && 0 <= sale.level){
      # указан sale.level
      # То есть если мы зададим sale.level = 50, то это не значит, что продажа
      # в каждый день будет равна половине поставки. 
      # Это значит, что сумма продаж будет равна половине поставки, а сами продажи 
      # могут принимать рандомное значение от 0 до поставки в конкретный день.
      
      # Считаем сумму поставок и высчитываем ожидаемую сумму продаж
      supply.sum <- sum(supply)
      sales.sum <- as.integer(supply.sum * sale.level / 100)
      sales <- rep(0, length(supply))
      # Для генерации рандомных продаж воспользуемся следующим алгоритмом
      # В цикле (решения без цикла, пока что, я придумать не смог.) будем генерировать значения продаж для дня 
      # Когда сумма продаж станет равной ожидаемому числу, цикл прекратится. 
      # Данный способ, на самом деле, даёт нам плохое распределение, так как в конце вектора, скорее всего будут очень маленькие значения и нули
      # У меня была идея насчёт другого способа, который позволял бы сделать хорошие, "реалистичные" (в рамках задачи составить случайные векторы поставок и продаж)
      # значения для вектора продаж, но этот способ не защищён от ошибки, когда продажи превышают поставки. (На защите работы могу рассказать, в чём он заключался)
      j = 1
      
      while (sales.sum != 0){
        
        if(is.na(supply[j])){
          j <- j - length(supply)
        }
        rand.int <- as.integer((supply[j] - sales[j]) * runif(n = 1, min = 0, max = 1))
        if(rand.int < sales.sum){
          sales[j] <- sales[j] + rand.int
        }else{
          rand.int <- sales.sum
          sales[j] <- sales[j] + sales.sum
        }
        sales.sum <- sales.sum - rand.int
        j <- j + 1

      }
    }
    
    else{
      # Если указан неверный sale.level, то возвращаем ошибку и прекращаем выполнеие функции
      return("Неверный параметр sale.level !")
      
    }
  }
  # Создаём датафреймы со сгенерированной информацией
  DataF.supply <- data.frame(
    "День" = period,
    "Поставка" = supply
  )
  
  DataF.sales <- data.frame(
    "День" = period, 
    "Продажа" = sales
  )
  
  # Задаём разрешение файлов 
  supply.file.format <- ".in"
  sales.file.format <- ".out"
  # Создаём строки с именами файлов поставок и продаж
  file.name.supply <- paste0(file.name, supply.file.format)
  file.name.sales <- paste0(file.name, sales.file.format)
  # Записываем датафреймы в файлы
  write.table(x = DataF.supply, 
              file = file.path(dir.name, file.name.supply),
              sep = " ", 
              row.names = FALSE)
  
  write.table(x = DataF.sales, 
              file = file.path(dir.name, file.name.sales),
              sep = " ",
              row.names = FALSE)
  
}

generate.product.dir <- function(product.name, n, sale.level = -1, minimum = 0, maximum = 10000){
  # Функция генерации папок с товарами
  # product.name - название товара
  # n - количество магазинов
  # В директории компании создаётся папка с названием товара
  # В папке товара создаётся n папок магазинов
  # Далее создаём файлы с продажами и поставками. Из всех аргументов по умолчанию
  # Передадим только sale.level
  product.dir <- file.path(products.dir, product.name)
  dir.create(product.dir)
  shops <- sprintf("Магазин_%s",seq(1:n))
  shops <- file.path(product.dir, shops)
  lapply(shops, dir.create)
  lapply(shops, generate.files, sale.level = sale.level, minimum = minimum, maximum = maximum)
}

# Функция для генерации данных в одну строчку, но только с одинаковыми ограничениями по поставкам для каждого товара. 
generate.data <- function(n = 10, sale.level = -1, products = PRODUCTS, minimum = 0, maximum = 10000){
  # n - количество магазинов. По умолчанию 10
  lapply(products, 
         generate.product.dir, 
         n = n, 
         sale.level = sale.level, 
         minimum = minimum,
         maximum = maximum)
}

# Буду генерировать уровень продаж 67%
generate.product.dir(n = SHOPS.COUNT, sale.level = 67, product.name = PRODUCTS[1], minimum = 0, maximum = MAX.SUPPLY[1])
generate.product.dir(n = SHOPS.COUNT, sale.level = 67, product.name = PRODUCTS[2], minimum = 0, maximum = MAX.SUPPLY[2])
generate.product.dir(n = SHOPS.COUNT, sale.level = 67, product.name = PRODUCTS[3], minimum = 0, maximum = MAX.SUPPLY[3])


# Данные успешно сгенерированы.

# Создание папки "Анализ"
analyze.dir.name <- "Анализ"
analyze.dir <- file.path(company.dir, analyze.dir.name)
dir.create(analyze.dir.name)

# Создадим в директории "Анализ" папки с названием товаров, чтобы сохранять в них информацию
lapply(file.path(analyze.dir, dir(products.dir)) , dir.create)


copy.from.product.dir <- function(product.dir.name){
  # Функция для копирования файлов .in .out из директории магазинов 
  # в директорию "Анализ"
  # product.dir.name - путь до директории товара
  
  
  copy.from.shop <- function(path.to.shop){
    # Вспомогательная функция для функции копирования
    # Производит непосредственно копирование и переименование файлов
    # path.to.shop - путь до папки магазина
    # path.to.analyze - путь до папки товара в директории "Анализ"
    
    
    files.names <- dir(path.to.shop)
    shop.name <- basename(path.to.shop)
    path.to.analyze <- file.path(analyze.dir, basename(dirname(path.to.shop)))
    
    files <- file.path(path.to.shop, dir(path.to.shop))
    file.copy(from = files, to = path.to.analyze, recursive = FALSE, copy.mode = TRUE)
    
    new.names <- file.path(path.to.analyze, paste0(shop.name, "_", files.names))
    copied.files <- file.path(path.to.analyze, files.names)
    
    file.rename(from = copied.files, to = new.names)
    
  }
  
  shops.in.dir <- file.path(product.dir.name, dir(product.dir.name))
  lapply(shops.in.dir, copy.from.shop)
  
}

# Производим копирование 
lapply(file.path(products.dir, dir(products.dir)), copy.from.product.dir)
# Успешно скопировали и переименовали файлы 

get.table <- function(path.to.table){
  # Функция для считывания таблицы и её преобразования
  # path.to.table - путь до файла с таблицей
  table <- read.table(
    file = path.to.table,
    head = TRUE,
    sep = " ",
    encoding = "UTF-8"
  )
  colnames(table)[which(names(table) == "День")] <- "День недели"
  
  index <- unlist(gregexpr("_", basename(path.to.table)))[2] - 1
  file.name <- substr(x = basename(path.to.table), start = 1, stop = index)
  # Сохраняем в список значения для итоговой таблицы
  if(grepl(".out", basename(path.to.table))){
    sales <- as.vector(table$"Продажа")
    
    total <-  sum(sales)
 
    table.list <- list(
      product = basename(dirname(path.to.table)),
      name = file.name, 
      table_df = table,
      total = total,
      sales = sales
    )
    
  }else{
    supply <- as.vector(table$"Поставка")
    
    total <-  sum(supply)
    
    table.list <- list(
      product = basename(dirname(path.to.table)),
      name = file.name, 
      table_df = table,
      total = total,
      supply = supply
    )
  }
  
  return(table.list)

}


# Делаем списки для файлов поставок и продаж
in.files <- lapply(file.path(analyze.dir, dir(analyze.dir), list.files(path = file.path(analyze.dir, dir(analyze.dir)), pattern = "*.in")), get.table)
out.files <- lapply(file.path(analyze.dir, dir(analyze.dir), list.files(path = file.path(analyze.dir, dir(analyze.dir)), pattern = "*.out")), get.table)
# Собираем данные для итоговой таблицы
# Для этого напишем функцию 
out.in.compound <- function(in.list, out.list){
  
  find.same <- function(out.list, in.list){
    # Сравниваем файл поставок и продаж на соотвествие названия магазина и продукта
    if((in.list["name"]$name == out.list["name"]$name) && (in.list["product"]$product == out.list["product"]$product)){
      product.in.out.list <- list(
        shop = in.list$name,
        product = in.list$product,
        sales = out.list$sales,
        supply = in.list$supply,
        total.sales = out.list$total,
        total.supply = in.list$total,
        table_df = cbind(in.list$table_df, "Продажа" = out.list$table_df$Продажа),
        days = in.list$table_df$"День недели"
      )
      product.in.out.list  <- product.in.out.list [!sapply(product.in.out.list ,is.null)]
      return(product.in.out.list)
    }
  }
  # Запускаем проход по файлам продаж
  result <- lapply(out.list, find.same, in.list = in.list)
  result <- result[!sapply(result,is.null)]
  return(result)
  
}
# Проход по файлам поставок
in.out.files <- lapply(in.files, out.in.compound, out.list = out.files)
in.out.files

# Таким образо у нас теперь есть списки с информацией о поставках и продажах товара в конкретном магазине
# Теперь сделаем списки с информацией о суммарных поставках и продажах

# Приведём списки к удобному для обращения виду
fine.list <- function(some.list){
  result <- some.list[[1]]
  result <- result[!sapply(result,is.null)]
  return(result)
}

in.out.files.fine <- lapply(in.out.files, fine.list)
in.out.files.fine

# Костыль с цклом, пока я не придумал решения в функциональном виде.
total <- function(list.of.info){
  # Создаём список для сохранения результата сложения данных
  # Создаём список для исключения повторного учитывания данных
  result <- list()
  products <- list()
  # Начинаем проход по списку. 
  for (first.list in list.of.info){
    
    if(!(first.list$shop %in% names(result))){
      # При нахождении первого файла магазина создаём для него позицию в результирующем списке
      result[[first.list$shop]] <- list(
        shop = first.list$shop,
        sales = first.list$sales,
        supply = first.list$supply,
        total.sales = first.list$total.sales,
        total.supply = first.list$total.supply,
        days = first.list$days
      )
      # Сохраняем товар из данных о магазине, чтобы исключить повторный подсчёт этого файла
      products[[first.list$shop]] <- c(first.list$product)
    }else{
      # В цикле ищем файлы магазина, которые ешё не были учтены в результирующем списке.
      for(second.list in list.of.info){
        if((first.list$shop == second.list$shop) && (first.list$product != second.list$product) && (!(second.list$product %in% products[[first.list$shop]]))){
            products[[first.list$shop]] <- append(products, second.list$product)
            result[[first.list$shop]]$shop <-  first.list$shop
            result[[first.list$shop]]$sales <-  result[[first.list$shop]]$sales + second.list$sales
            result[[first.list$shop]]$supply <-  result[[first.list$shop]]$supply + second.list$supply
            result[[first.list$shop]]$total.sales <-  result[[first.list$shop]]$total.sales + second.list$total.sales
            result[[first.list$shop]]$total.supply <-  result[[first.list$shop]]$total.supply + second.list$total.supply
        
      }
      
    }
      
    }
  }
  return(result)
}

# Получим списки суммарных значений поставок и продаж

total.list <- total(in.out.files.fine)
total.list


# Создание итоговой таблицы
res.tab <- data.frame(   
  "Номер магазина" = double(),
  "Выручка, руб" = double(), 
  "Прибыль, руб" = double(),
  "Реализация, конт." = double(),
  "Списание, конт." = double(),
  "Равномерность продаж" = double(),
  "Продажи макс" = double(),
  "День макс. продаж" = double(),
  "Продажи мин" = double(),
  "День мин. продаж" = double(),
  "Списание макс" = double(),
  "День макс. списания" = double()
)



# Напишем функции для заполнения строк таблицы информацией
put.in.tab <- function(shop.list, P_supply = 5500 , P_sale = 8000, P_util = 400){
  
    value <- data.frame(
      "Номер магазина" = as.double(unlist(strsplit(shop.list$shop, "Магазин_"))[2]),
      "Выручка, руб" = shop.list$total.sales * P_sale, 
      "Прибыль, руб" = shop.list$total.sales * P_sale - (shop.list$total.supply * P_supply + (shop.list$total.supply - shop.list$total.sales) * P_util),
      "Реализация, конт." = shop.list$total.sales,
      "Списание, конт." = shop.list$total.supply - shop.list$total.sales,
      "Равномерность продаж" = sd(shop.list$sales),
      "Продажи макс" = max(shop.list$sales) ,
      "День макс. продаж" = match(max(shop.list$sales), shop.list$sales),
      "Продажи мин" = min(shop.list$sales),
      "День мин. продаж" = match(min(shop.list$sales), shop.list$sales),
      "Списание макс" = max(shop.list$supply - shop.list$sales),
      "День макс. списания" = match(max(shop.list$supply - shop.list$sales), shop.list$supply - shop.list$sales)
    )

    return(value)
}
# Нет способа заполнения таблицы без циклов
for(shop.list in total.list){
  new.row <- put.in.tab(shop.list)
  res.tab <- rbind(res.tab, new.row) 
}
res.tab
# Исправляем ошибку, когда магазин 10 стоит перед магазином 2 (сортировка в алфовитном порядке)
res.tab <-  res.tab[order(res.tab$Номер.магазина), ]
res.tab

# Удаляем столбец "Номер магазина", который помогал исправить проблему алфавитной сортировки в R 
res.tab <- subset (res.tab, select = -Номер.магазина)

# Добавим строчки "Итого" и "Среднее"
res.tab 
total.tab <- data.frame(
  "Выручка, руб" = sum(res.tab$Выручка..руб), 
  "Прибыль, руб" = sum(res.tab$Прибыль..руб),
  "Реализация, конт." = sum(res.tab$Реализация..конт.),
  "Списание, конт." = sum(res.tab$Списание..конт.),
  "Равномерность продаж" = sum(res.tab$Равномерность.продаж),
  "Продажи макс" = "" ,
  "День макс. продаж" = "",
  "Продажи мин" = "",
  "День мин. продаж" = "",
  "Списание макс" = "",
  "День макс. списания" = ""
)

mid.tab <- data.frame(
  "Выручка, руб" = mean(res.tab$Выручка..руб), 
  "Прибыль, руб" = mean(res.tab$Прибыль..руб),
  "Реализация, конт." = mean(res.tab$Реализация..конт.),
  "Списание, конт." = mean(res.tab$Списание..конт.),
  "Равномерность продаж" = mean(res.tab$Равномерность.продаж),
  "Продажи макс" = "",
  "День макс. продаж" = "",
  "Продажи мин" = "",
  "День мин. продаж" = "",
  "Списание макс" = "",
  "День макс. списания" = ""
)
res.tab <- rbind(res.tab, total.tab)
res.tab <- rbind(res.tab, mid.tab)
rownames(res.tab) <- append(sprintf("Магазин_%s",seq(1:SHOPS.COUNT)), c("Итого", "Среднее")) 
res.tab
# Запись итого в файл "Итог.csv"
write.table(x = cbind(" " = rownames(res.tab), data.frame(res.tab, row.names=NULL)), 
            file = file.path(analyze.dir, "Итог.csv"),
            sep = ";", 
            row.names = F)
# Проверка на существвование файла
print(paste0("Файл создан: ", file.exists(file.path(analyze.dir, "Итог.csv"))))
# Открытие файла в программе для чтения .csv файлов по умолчанию
system(paste0("open ", file.path(analyze.dir, "Итог.csv")))
# Часть 3
# Графики

# Графики делал в порядке
# 1 и 5 
# 2 
# 3
# 6

# Задания 1 и 5

# Узнаём, какие магазины интерсуют пользователя
shops.to.graph <- c()
shop.num <- as.integer(readline(prompt="Введите количество магазинов: "))
for(i in sample(1:shop.num)){
  shops.to.graph <- append(shops.to.graph, as.integer(readline(prompt="Введите номер магазина: ")))
}


# Функция для построения графиков по 5 параметрам
graph.5 <- function(shops){
  cols <- rainbow(length(shops))
  pchs <- (1:10)
  types <- rep(c("o", "b", "s"), 4)
  ltys <- rep(c(1:5), 2)
  
  if(is.null(names(shops))){
    shops.names <- unlist(lapply(shops, function(x) x$name)) 
  }else{
    shops.names <- shops$name
  }
  
  ind <- 2
  if(length(shops.names) > 1){
    max.sales <- max(unlist(lapply(shops, function(x) max(x$sales))))
    max.supply <- max(unlist(lapply(shops, function(x) max(x$supply))))
    min.sales <- min(unlist(lapply(shops, function(x) min(x$sales))))
    min.supply <- min(unlist(lapply(shops, function(x) min(x$supply)))) 
    shop.to.graph <- shops[[1]]
    shops[[1]] <- NULL
  }else{
    max.sales <- max(shops$sales)
    max.supply <- max(shops$supply)
    min.sales <- min(shops$sales)
    min.supply <- min(shops$supply) 
    shop.to.graph <- shops
    shops <- list()
    shops.names <- shop.to.graph$shop
  }
  
  
  par(mfrow=c(2,3))
  plot(y = shop.to.graph$sales, 
       x = shop.to.graph$days,
       ylim = c(0, max.sales),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       lwd = 2, 
       xlab = "День",
       ylab = "Продажи, шт.",
       main = "Продажи, шт.")
  
  
  for(shop in shops){
    
    points(y = shop$sales, 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$sales, 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  plot(y = shop.to.graph$sales * P_sale, 
       x = shop.to.graph$days,
       ylim = c(0, max.sales * P_sale),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       lwd = 2, 
       xlab = "День",
       ylab = "Выручка, руб.",
       main = "Выручка, руб.")

  for(shop in shops){
    
    points(y = shop$sales * P_sale, 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$sales * P_sale, 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  
  plot(y = shop.to.graph$sales * P_sale - (shop.to.graph$supply * P_supply + (shop.to.graph$supply - shop.to.graph$sales) * P_util), 
       x = shop.to.graph$days,
       ylim = c(min.sales * P_sale - (max.supply * P_supply + (max.supply - min.sales) * P_util), max.sales * P_sale - (min.supply * P_supply + (min.supply - max.sales) * P_util)),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       lwd = 2, 
       xlab = "День",
       ylab = "Прибыль, руб.",
       main = "Прибыль, руб.")

  for(shop in shops){
    
    points(y = shop$sales * P_sale - (shop$supply * P_supply + (shop$supply - shop$sales) * P_util), 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$sales * P_sale - (shop$supply * P_supply + (shop$supply - shop$sales) * P_util), 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  plot(y = shop.to.graph$supply - shop.to.graph$sales, 
       x = shop.to.graph$days,
       ylim = c(0, max.supply - min.sales),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       xlab = "День",
       ylab = "Списание, шт.",
       main = "Списание, шт.")
  
  for(shop in shops){
    
    points(y = shop$supply - shop$sales, 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$supply - shop$sales, 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  # Подсчёт рентабельности
  rent <- list()
  rent[[shop.to.graph$shop]] <- ((shop.to.graph$sales * P_sale) / (shop.to.graph$supply * P_supply + (shop.to.graph$supply - shop.to.graph$sales) * P_util) - 1) * 100
  
  for(shop in shops){
    rent[[shop$shop]] <- ((shop$sales * P_sale) / (shop$supply * P_supply + (shop$supply - shop$sales) * P_util) - 1) * 100
  }
  rent <- matrix(unlist(rent), byrow=TRUE, nrow=length(rent) )
  
  
  barplot(rent,
          main = "Рентабельность, %",
          xlab = "День",
          names = shop.to.graph$days,
          ylab = "Рентабельность, %",
          col = cols[1:length(shops.names)],
          beside = T
  )
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )

}

# Напишем вспомогательную функцию для упрощённого обращения к информации о магазинах
# Для этого воспользуемся циклом(из-за особенностей языка функциональным методом решить эту задачу нельзя) 

lists.by.names <- list()
for(shop.list in in.out.files.fine){
  name <- paste0(shop.list$shop, "_", shop.list$product)
  lists.by.names[[name]] <- shop.list
  lists.by.names[[name]][["name"]] <- name
  lists.by.names[[name]][["max.sales"]] <- max(shop.list$sales)
  lists.by.names[[name]][["max.supply"]] <- max(shop.list$supply)
  lists.by.names[[name]][["min.sales"]] <- min(shop.list$sales)
  lists.by.names[[name]][["min.supply"]] <- min(shop.list$supply)
}
# Удобный список
lists.by.names 


# Функция для выбора информации из списка
user.paint <- function(shop.num, list.of.shops = lists.by.names,  product = "Яблоки"){
  name <- paste0("Магазин_", shop.num, "_",product)
  return(list.of.shops[[name]])
}

# Вызов для нескольких магазинов 
# Для примера использую продукт Молоко
graph.5(lapply(shops.to.graph, user.paint, product = "Молоко"))

# Графики для одного магазина по одному продукту 
graph.5(user.paint(shop.num = 2, product = "Хлеб"))

# Задание 2
# Возьмём код предыдущей функции и просто уберём фрагменты с генерацией графиков по продажам и выручке
# С точки зрения архитектуры программы, было бы правильно просто добавить в функцию параметры 
# отвечающие за то, какие графики требуется отобразить
# Для упрощения объяснения на защите работы я решил просто переписать код

graph.3 <- function(shops){
  cols <- rainbow(length(shops))
  pchs <- (1:10)
  types <- rep(c("o", "b", "s"), 4)
  ltys <- rep(c(1:5), 2)
  shops.names <- names(shops)
  ind <- 2
  if(!("shop" %in% shops.names)){
    max.sales <- max(unlist(lapply(shops, function(x) max(x$sales))))
    max.supply <- max(unlist(lapply(shops, function(x) max(x$supply))))
    min.sales <- min(unlist(lapply(shops, function(x) min(x$sales))))
    min.supply <- min(unlist(lapply(shops, function(x) min(x$supply)))) 
    shop.to.graph <- shops[[1]]
    shops[[1]] <- NULL
  }else{
    max.sales <- max(shops$sales)
    max.supply <- max(shops$supply)
    min.sales <- min(shops$sales)
    min.supply <- min(shops$supply) 
    shop.to.graph <- shops
    shops <- list()
    shops.names <- shop.to.graph$shop
  }

  
  par(mfrow=c(1,3))


  plot(y = shop.to.graph$sales * P_sale - (shop.to.graph$supply * P_supply + (shop.to.graph$supply - shop.to.graph$sales) * P_util), 
       x = shop.to.graph$days,
       ylim = c(min.sales * P_sale - (max.supply * P_supply + (max.supply - min.sales) * P_util), max.sales * P_sale - (min.supply * P_supply + (min.supply - max.sales) * P_util)),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       lwd = 2, 
       xlab = "День",
       ylab = "Прибыль, руб.",
       main = "Прибыль, руб.")
  
  for(shop in shops){
    
    points(y = shop$sales * P_sale - (shop$supply * P_supply + (shop$supply - shop$sales) * P_util), 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$sales * P_sale - (shop$supply * P_supply + (shop$supply - shop$sales) * P_util), 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  plot(y = shop.to.graph$supply - shop.to.graph$sales, 
       x = shop.to.graph$days,
       ylim = c(0, max.supply - min.sales),
       col = cols[1],
       cex = 2, 
       pch = pchs[1], 
       type = types[1], 
       lty = ltys[1], 
       xlab = "День",
       ylab = "Списание, шт.",
       main = "Списание, шт.")
  
  for(shop in shops){
    
    points(y = shop$supply - shop$sales, 
           x = shop$days,
           col = cols[ind],
           cex = 2,
           pch = pchs[ind],
    )
    lines(y = shop$supply - shop$sales, 
          x = shop$days,
          col = cols[ind],
          lty = ltys[ind],
          lwd = 2)
    ind <- ind + 1
  }
  ind <- 2
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
  # Подсчёт рентабельности
  rent <- list()
  rent[[shop.to.graph$shop]] <- ((shop.to.graph$sales * P_sale) / (shop.to.graph$supply * P_supply + (shop.to.graph$supply - shop.to.graph$sales) * P_util) - 1) * 100
  
  for(shop in shops){
    rent[[shop$shop]] <- ((shop$sales * P_sale) / (shop$supply * P_supply + (shop$supply - shop$sales) * P_util) - 1) * 100
  }
  rent <- matrix(unlist(rent), byrow=TRUE, nrow=length(rent) )
  
  
  barplot(rent,
          main = "Рентабельность, %",
          xlab = "День",
          names = shop.to.graph$days,
          ylab = "Рентабельность, %",
          col = cols[1:length(shops.names)],
          beside = T
  )
  
  legend("topleft",
         legend = shops.names,
         fill = cols[1:length(shops.names)]
  )
  
}

# Ранее сгенерированный в программе список с итоговой информацией по каждому магазину
total.list


# Вызываем графики по всем магазинам по всем товарам!
graph.3(total.list)


graph.3(total.list[[1]])


# Задание 3
# Воспользуемся файликом с информацией по товару в магазине 
lists.by.names

# Пишем функцию для построения круговой диаграммы 
circle.diagram <- function(info){
  par(mfrow=c(1,1))
  
  cols <- rainbow(length(info))
  pchs <- (1:10)
  types <- rep(c("o", "b", "s"), 4)
  ltys <- rep(c(1:5), 2)
  shops.names <- unlist(lapply(info, function(x) names(x)))
  total.sales <- unlist(lapply(info, function(x) x[[1]][[1]]))
  
  pie(
    total.sales,
    labels = paste0(round(100 * total.sales / sum(total.sales), 2), "%"),
    col = cols,
    main = paste0("Продажи товара: ", info[[1]][[1]][[2]])
  )
  
  legend("topright",
         legend = shops.names,
         fill = cols[1:length(shops.names)],
         cex = 0.75
  )
  
}


# Вспомогательная функция для получения списков вида магазин: продажи
get.sales.only <- function(shop){
  res <- list()
  res[[shop$shop]] <- list(shop$total.sales, shop$product)
  return(res)
}
# Сохраним созданный список в переменную
milk.total <- lapply(lapply(c(1:10), user.paint, product = "Молоко"), get.sales.only)
milk.total
# Вызываем диаграмму по молоку
circle.diagram(milk.total)



# Задание 6 
bar.total <- function(shops.df){
  cols <- colors()[sample(1:501, length(rownames(shops.df)))]
  pchs <- (1:10)
  types <- rep(c("o", "b", "s"), 4)
  ltys <- rep(c(1:5), 2)
  shops.names <- names(shops.df)
  product.names <- rownames(shops.df)

  
  
  barplot(shops.df,
          main = "Продажи",
          xlab = "Магазин",
          names = shops.names,
          ylab = "Продажи, конт.",
          col = cols,
          beside = F
  )
  
  legend("topleft",
         legend = product.names,
         fill = cols
  )
  
}


# Преобразуем ранее созданный список под наши нужды 


# Напишем функцию для получения списка вида название магазина с указанием товара : суммарные продажи
user.paint.total <- function(shop.num, list.of.shops = lists.by.names, products = PRODUCTS){
  # Скопируем функцию из программы и отредактируем её возврат, т.к. 
  # При вызове lapply мы не можем брать "часть" вывода, например по индексу
  user.paint <- function(shop.num, product, list.of.shops = lists.by.names){
    name <- paste0("Магазин_", shop.num, "_",product)
    result.list <- list(list.of.shops[[name]]$shop, list.of.shops[[name]]$product,  list.of.shops[[name]]$total.sales)
    return(result.list)
  }
  
  result <- lapply(PRODUCTS, user.paint, shop.num = shop.num, list.of.shops = list.of.shops)
  result <- list(result[[1]][[1]], (lapply(result, function(x) x[-1])))
  return(result)
}
# Проверим работоспособность
(user.paint.total(9))

# Сделаем матрицу для построения столбиковой диаграммы
# Напишем функцию для получения структурированных данных
get.total.list <- function(){
  x <- unlist(lapply(c(1:SHOPS.COUNT), user.paint.total))
  x <- split(x,             
             cut(seq_along(x),
                 SHOPS.COUNT,
                 labels = F))
  shop.product.pair <- function(shop.vec){
    list.result <- as.list( setNames(shop.vec[seq(3, length(shop.vec), 2)], shop.vec[seq(2, length(shop.vec), 2)]) )
    return(list.result)
  }
  result <- lapply(x, shop.product.pair)
  return(result)
}
# Сохраним в переменную полученную структуру
total.by.product <- get.total.list()
names(total.by.product) <- unlist(lapply(c(1:SHOPS.COUNT), function(x) paste0("Магазин_", x)))



df.to.graph<- t(data.frame(matrix(unlist(total.by.product), nrow=length(total.by.product), byrow=TRUE), row.names = names(total.by.product)))
rownames(df.to.graph) <-  names(total.by.product[[1]])
# А вот и наш дата фрейм !
df.to.graph
# Сделаем его матрицей, чтобы было удобней работать в barplot 
df.to.graph <- as.matrix(df.to.graph)
df.to.graph

# Рисуем график!
bar.total(df.to.graph)

# Задача 4 
# Не успел подготовить этот график за неделю до зачёта. 


# Ворошилов Владислав ПИ20-3
{
# Задание 1 
var1 <- 10
typeof(var1)
# Без явного указания типа у числа указывается тип double

var1 <- 29
typeof(var1)
# Без явного указания типа у числа указывается тип double

var2 <- 1.0
typeof(var2)
# Т.к. я ввёл вещественное число

var3 <- 21i
typeof(var3)
# Так как был указан символ i, говорящий о том, что число комплексное

var3 <- 23i
typeof(var3)
# Так как был указан символ i, говорящий о том, что число комплексное

var4 <- "Hello world!"
typeof(var4)
# Т.к. была введна строка

var5 <- 2L
typeof(var5)
# Т.к. был явно указан тип с помощью постфикса L

var5 <- 34L
typeof(var5)
# Т.к. был явно указан тип с помощью постфикса L

var2 <- 2/3
typeof(var2)
# Т.к. результат любого деления является вещесвтенным числом

var2 <- 4/2
typeof(var2)
# Т.к. результат любого деления является вещесвтенным числом

var6 <- 0XA
typeof(var6)
# Т.к. тип числа был явно не указан, то double

var7 <- (0XbL - 120L)
typeof(var7)
# Был получен ответ integer, т.к. оба числа типа integer 

var8 <- 0XbL - 120
typeof(0XbL - 120)
# Был получен ответ double, т.к. 120 - double, поэтому 0XbL преобразуется к double 

var9 <- (0XbL * 17)
typeof(var9)
# Т.к. 17 - double по умолчанию, то число 0XbL привелось к типу double и результат стал double

var9 <- (0XbL * 17L)
typeof(var9)
# Здесь указали L у 17, поэтому результат integer
}

{
  # Задание 2

user_input <-  readline("Введите значение: ")
int <-  as.integer(user_input)
print(paste0(int, " - Type - ", typeof(int)))

doub <-  as.double(user_input)
print(paste0(doub, " - Type - ", typeof(doub)))

log <-  as.logical(user_input)
print(paste0(log, " - Type -", typeof(log)))
# Тип character не был явно преобразован к типу logical

char <-  as.character(user_input)
print(paste0(char, " - Type - ", typeof(char)))


print(paste0("integer == double: ", int == doub))
print(paste0("integer > double: ", int > doub))
print(paste0("integer < double: ", int < doub))
# integer приведён к типу double

print(paste0("integer == logical: ", int == log))
print(paste0("integer > logical: ", int > log))
print(paste0("integer < logical: ", int < log))
# Тип character не был явно преобразован к типу logical, поэтому результат любого сравнения - NA
print(1L == T)
print(2L == T)
# Из этого видим, что logical приводится к integer при сравнениях

print(paste0("logical == character: ", log == char))
print(paste0("logical > character: ", log > char))
print(paste0("logical < character: ", log < char))
# Тип character не был явно преобразован к типу logical, поэтому результат любого сравнения - NA
print(TRUE == "TRUE")
print(TRUE == "a")
print(FALSE == "a")
print(FALSE == "FALSE")
# Из этого видно, что при сравнении logical приводится к типу character

print(paste0("double == logical: ", doub == log))
print(paste0("double > logical: ", doub > log))
print(paste0("double < logical: ", doub < log))
# Тип character не был явно преобразован к типу logical, поэтому результат любого сравнения - NA
print(2.0 == T)
print(1.0 == T)
print(0 == F)
# Из этого видно, что logical приводится к double при сравнении

print(paste0("double == character: ", doub == char))
print(paste0("double > character: ", doub > char))
print(paste0("double < character: ", doub < char))
var <- 1
print(paste0(typeof(var), ' - ', var == "1"))
print(1 == "1.0")
# Из этого видно, что при сравнениии double приводится к character

# Суммирование результата операций (скопировано из выше указанных примеров)
# integer приведён к типу double
# Из этого видим, что logical приводится к integer при сравнениях
# Из этого видно, что при сравнении logical приводится к типу character
# Из этого видно, что logical приводится к double при сравнении
# Из этого видно, что при сравнениии double приводится к character
}

{
  # Задание 3
input.1 <-  readline("Введите значение 1: ")
input.2 <-  readline("Введите значение 2: ")

int.input.1 <-  as.integer(input.1)
doub.input.1 <-  as.double(input.1)
log.input.1 <-  as.logical(input.1)
char.input.1 <-  as.character(input.1)

int.input.2 <-  as.integer(input.2)
doub.input.2 <-  as.double(input.2)
log.input.2 <-  as.logical(input.2)
char.input.2 <-  as.character(input.2)

cat(int.input.1 * doub.input.2)
cat(int.input.1 + char.input.2)
typeof(int.input.1 + log.input.2)
cat(log.input.1 - char.input.2)
cat(doub.input.1 / log.input.2)
cat(doub.input.1 %/% char.input.2)

# cat() выводит без кавычек и квадратных скобочек с числами
# print() выводит с кавычками и квадратнысм скобочками с числами

# В арифметических операциях logical преобразовывается к integer или double
# в зависимости от того, с каким из этих типов выполняется арифметическая операция
# преобразования character к другим типам при арифметическим операциях не производится
# при операциях integer и double происходит преобразование к double
}

{ 
  #Задание 4 
  
q1 <- 2
q2 <- 0
q3 <- (q1 == 2) & (q2 == 0)
print(q3)


q1 <- 2
q2 <- 0
q3 <- (q1 == 2) && (q2 == 0)
print(q3)


# при использовании едничного и двойного И для скалярного значения разницы нет
# провсто выводится результат операции

q1 <- (2:4)
q2 <- (0:2)
q3 <- (q1 == 3) & (q2 == 1)
q3


q1 <- (2:4)
q2 <- (0:2)
q3 <- (q1 == 2) && (q2 == 1)
q3

# при работе с векторами: использование единичного И из векторов берётся по одному элементу
# проверяется условие выводится ответ. Потом берётся следующий элемент из из каждого вектора и идёт проверка
# Для данного примера:
# сначала из q1 берётся 2, из q2 берётся 0.Условие (q1 == 3) & (q2 == 0) не выполняется. Выводится  False
# дальше из q1 берётся 3, из q2 берётся 1.Условие (q1 == 3) & (q2 == 0) выполняется. Выводится True
# дальше из q1 берётся 4, из q2 берётся 2.Условие (q1 == 3) & (q2 == 0) не выполняется. Выводится False
# Итог: False True False

# при использовании двойного И сравниваются векторы полностью, поэтому выводится False
}

{
  #Задание 5
  print(3/7)
  print(3/7 - 0.4285714)
  
  # Т.к.3/7 представляет собой бесконечную десятичную дробь. 
  #Произошло округление до 7 знака, но в действительности
  # оно имеет бесконечное количество знаков после запятой. 
  # Т.к. 3/7 != 0.4285714, язык вывел в ответ ещё 7 цифр из десятичного представления
  # дроби 3/7 в экспоненциальном виде. Судя по тому, что работает язык R с вещественными числами 
  # без ошибок, то здесь используется не представление с мантиссой, а decimal
}

{
  # Задание 6
  print(sqrt(2))
  print(1.414214*1.414214)
  print(sqrt(2)*sqrt(2))
  print((sqrt(2)*sqrt(2))-2)  
  
  # Возвращаясь к деятичному представлению вещественных чисел. 
  # При выводе на экран происхоидт округление до целого, но при дальнейших операциях оно не происходит.
  # (По-моему преположению) Судя по ответу в 10^-16 степени, на вещественное число приходится 128 бит, 
  # вместо стандартных в других языках 64
}



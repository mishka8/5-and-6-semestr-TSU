if (!require(lmtest)) {
  install.packages("lmtest")
  library(lmtest)
}

if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}

file_name <- "D:/Labs_R/test_1.xlsx"  # путь к вашему Excel-файлу

# 1. Импортировать данные из файла "test_1.xlsx"
if (!file.exists(file_name)) {
  stop(paste("Файл не найден:", file_name))
}

# Читаем Excel-файл
flats <- read_excel(
  file_name,
  sheet = 1,  # первый лист
  col_names = TRUE
)

# Преобразуем в data.frame (если нужно)
flats <- as.data.frame(flats)

cat("1. Данные импортированы\n", 
    "   Размерность:", nrow(flats), "строк,", ncol(flats), "столбцов\n",
    "   Исходные названия столбцов:\n")
print(colnames(flats))
cat("\n")


if (ncol(flats) != 6) {
  stop("Ожидалось 6 столбцов. Проверь структуру файла.")
}

colnames(flats) <- c("type", "rent", "floor", "total_floors", "area", "furniture")

cat("2. Столбцы переименованы:\n",
    "   ", paste(colnames(flats), collapse = ", "), "\n\n", sep = "")

flats$rent <- as.numeric(gsub(",", ".", flats$rent))
flats$floor <- as.numeric(gsub(",", ".", flats$floor))
flats$total_floors <- as.numeric(gsub(",", ".", flats$total_floors))
flats$area <- as.numeric(gsub(",", ".", flats$area))

# Приведение категориальных признаков к factor
flats$type <- trimws(tolower(flats$type))
flats$furniture <- trimws(tolower(flats$furniture))

flats$type <- as.factor(flats$type)
flats$furniture <- as.factor(flats$furniture)

cat("Структура данных после преобразований:\n",
    str(flats),
    "\n")

cat("Пропущенные значения по столбцам:\n",
    print(colSums(is.na(flats)))
    ,"\n")

# Удаляем пропуски
flats <- na.omit(flats)

# Удаляем строки с некорректными значениями
flats <- subset(flats, rent > 0 & area > 0)

cat("После очистки данных осталось:", nrow(flats), "строк\n\n")

cat("3. Построение графиков\n")

# Гистограммы и столбчатые диаграммы
#windows()
par(mfrow = c(2, 3))

hist(flats$rent, main = "Арендная плата", xlab = "rent", col = "lightblue", border = "white")
hist(flats$floor, main = "Этаж", xlab = "floor", col = "lightgreen", border = "white")
hist(flats$total_floors, main = "Всего этажей", xlab = "total_floors", col = "yellow", border = "white")
hist(flats$area, main = "Площадь", xlab = "area", col = "lightpink", border = "white")
barplot(table(flats$type), main = "Тип квартиры", col = "lightblue")
barplot(table(flats$furniture), main = "Мебель", col = "lightgreen")

par(mfrow = c(1, 1))

# Диаграммы рассеяния для числовых переменных
##windows()
pairs(
  flats[, c("rent", "area", "floor", "total_floors")],
  main = "Диаграммы рассеяния",
  col = "blue",
  pch = 16
)

# Boxplot для категориальных факторов
#windows()
par(mfrow = c(2, 2))

boxplot(rent ~ type, data = flats, main = "Цена по типу", col = c("lightblue", "lightgreen"))
boxplot(rent ~ furniture, data = flats, main = "Цена по мебели", col = c("lightblue", "lightgreen"))
boxplot(area ~ type, data = flats, main = "Площадь по типу", col = c("lightblue", "lightgreen"))
boxplot(rent ~ interaction(type, furniture), data = flats,
        main = "Цена по комбинациям факторов",
        col = "lightgray", las = 2)

par(mfrow = c(1, 1))

cat("   Графики построены\n\n")

cat("4. Анализ взаимосвязей:\n")

# Корреляции числовых признаков
cor_matrix <- cor(flats[, c("rent", "area", "floor", "total_floors")], use = "complete.obs")
cat("   Корреляционная матрица:\n")
print(round(cor_matrix, 3))
cat("\n")

# Средние значения rent по категориям
cat("   Средняя цена по типу:\n")
print(tapply(flats$rent, flats$type, mean))
cat("\n")

cat("   Средняя цена по мебели:\n")
print(tapply(flats$rent, flats$furniture, mean))
cat("\n")

# ANOVA для категориальных факторов
cat("   Проверка влияния type на rent (ANOVA):\n")
print(summary(aov(rent ~ type, data = flats)))
cat("\n")

cat("   Проверка влияния furniture на rent (ANOVA):\n")
print(summary(aov(rent ~ furniture, data = flats)))
cat("\n")

cat("5. Парная модель регрессии: rent ~ area\n")

model1 <- lm(rent ~ area, data = flats)
print(summary(model1))
cat("\n")

cat("6. Проверка остатков парной модели на нормальность:\n")

residuals1 <- residuals(model1)

#windows()
par(mfrow = c(1, 2))

hist(residuals1, main = "Гистограмма остатков", xlab = "Остатки", col = "lightblue", border = "white")
qqnorm(residuals1, main = "Q-Q plot остатков", col = "blue", pch = 16)
qqline(residuals1, col = "red", lwd = 2)

par(mfrow = c(1, 1))

shapiro1 <- shapiro.test(residuals1)
cat("   Shapiro-Wilk: W =", round(shapiro1$statistic, 4),
    ", p-value =", format.pval(shapiro1$p.value, digits = 4), "\n")

if (shapiro1$p.value > 0.05) {
  cat("   Вывод: остатки можно считать нормально распределёнными.\n\n")
} else {
  cat("   Вывод: остатки не являются нормально распределёнными.\n\n")
}

cat("7. Проверка парной модели на гетероскедастичность:\n")

#windows()
plot(
  model1$fitted.values, residuals1,
  main = "Остатки vs предсказанные значения",
  xlab = "Предсказанные значения",
  ylab = "Остатки",
  col = "blue",
  pch = 16
)
abline(h = 0, col = "red", lwd = 2)

bp1 <- bptest(model1)
cat("   Breusch-Pagan test: p-value =", format.pval(bp1$p.value, digits = 4), "\n")

if (bp1$p.value > 0.05) {
  cat("   Вывод: признаков гетероскедастичности не обнаружено.\n\n")
} else {
  cat("   Вывод: присутствует гетероскедастичность.\n\n")
}

cat("8. Устранение гетероскедастичности:\n")

# Способ 1: деление зависимой переменной на фактор area
flats$rent_div_area <- flats$rent / flats$area
model2a <- lm(rent_div_area ~ area, data = flats)

cat("   Способ 1: модель (rent / area) ~ area\n")
print(summary(model2a))
cat("\n")

bp2a <- bptest(model2a)
cat("   Breusch-Pagan для модели (rent / area) ~ area: p-value =",
    format.pval(bp2a$p.value, digits = 4), "\n\n")
#Min = -571.3 — самый маленький остаток
#1Q = -353.3 — нижний квартиль
#Median = -85.7 — медиана остатков
#3Q = 73.1 — верхний квартиль
#Max = 4951.1 — самый большой остаток

# Способ 2: логарифмирование зависимой переменной
flats$log_rent <- log(flats$rent)
model2b <- lm(log_rent ~ area, data = flats)

cat("   Способ 2: модель log(rent) ~ area\n")
print(summary(model2b))
cat("\n")

bp2b <- bptest(model2b)
cat("   Breusch-Pagan для модели log(rent) ~ area: p-value =",
    format.pval(bp2b$p.value, digits = 4), "\n\n")

cat("   - Чем выше p-value теста Breusch-Pagan, тем слабее признаки гетероскедастичности.\n")
cat("   - Для дальнейшего анализа удобнее использовать логарифмирование rent.\n\n")

cat("9. Новая парная модель: log(rent) ~ area\n")
print(summary(model2b))
cat("\n")

cat("10. Проверка остатков новой модели на нормальность:\n")

residuals2b <- residuals(model2b)

#windows()
par(mfrow = c(1, 2))

hist(residuals2b, main = "Гистограмма остатков (log)", xlab = "Остатки", col = "lightblue", border = "white")
qqnorm(residuals2b, main = "Q-Q plot остатков (log)", col = "blue", pch = 16)
qqline(residuals2b, col = "red", lwd = 2)

par(mfrow = c(1, 1))

shapiro2 <- shapiro.test(residuals2b)
cat("   Shapiro-Wilk: W =", round(shapiro2$statistic, 4),
    ", p-value =", format.pval(shapiro2$p.value, digits = 4), "\n")

if (shapiro2$p.value > 0.05) {
  cat("   Вывод: остатки новой модели можно считать нормально распределёнными.\n\n")
} else {
  cat("   Вывод: остатки новой модели не являются нормально распределёнными.\n\n")
}
cat("11. Проверка новой модели на гетероскедастичность:\n")

#windows()
plot(
  model2b$fitted.values, residuals2b,
  main = "Остатки лог-модели",
  xlab = "Предсказанные значения",
  ylab = "Остатки",
  col = "blue",
  pch = 16
)
abline(h = 0, col = "red", lwd = 2)

bp2 <- bptest(model2b)
cat("   Breusch-Pagan test: p-value =", format.pval(bp2$p.value, digits = 4), "\n")

if (bp2$p.value > 0.05) {
  cat("   Вывод: признаки гетероскедастичности отсутствуют.\n\n")
} else {
  cat("   Вывод: гетероскедастичность сохраняется.\n\n")
}

cat("12. Построение графика с линией регрессии и интервалами\n")

new_area <- data.frame(area = seq(min(flats$area), max(flats$area), length.out = 100))

pred_conf <- predict(model2b, newdata = new_area, interval = "confidence", level = 0.95)
pred_pred <- predict(model2b, newdata = new_area, interval = "prediction", level = 0.95)

# Обратное преобразование из log-шкалы
pred_conf_exp <- exp(pred_conf)
pred_pred_exp <- exp(pred_pred)

#windows()
plot(
  flats$area, flats$rent,
  main = "Цена аренды от площади",
  xlab = "Площадь, кв.м",
  ylab = "Арендная плата",
  col = "blue",
  pch = 16
)

lines(new_area$area, pred_conf_exp[, "fit"], col = "red", lwd = 2)
lines(new_area$area, pred_conf_exp[, "lwr"], col = "green", lwd = 2, lty = 2)
lines(new_area$area, pred_conf_exp[, "upr"], col = "green", lwd = 2, lty = 2)
lines(new_area$area, pred_pred_exp[, "lwr"], col = "orange", lwd = 2, lty = 3)
lines(new_area$area, pred_pred_exp[, "upr"], col = "orange", lwd = 2, lty = 3)

legend(
  "topleft",
  legend = c("Данные", "Линия регрессии", "Доверительный интервал", "Прогнозный интервал"),
  col = c("blue", "red", "green", "orange"),
  lty = c(NA, 1, 2, 3),
  pch = c(16, NA, NA, NA),
  lwd = 2
)

cat("   График построен\n\n")

cat("13. Множественная регрессия на все факторы:\n")

model_full <- lm(log_rent ~ area + floor + total_floors + type + furniture, data = flats)
print(summary(model_full))
cat("\n")

cat("14. Удаление незначимых факторов\n")

current_model <- model_full

repeat {
  sm <- summary(current_model)
  coef_table <- sm$coefficients
  
  # убираем intercept
  coef_table <- coef_table[rownames(coef_table) != "(Intercept)", , drop = FALSE]
  
  # если коэффициентов больше нет — выходим
  if (nrow(coef_table) == 0) break
  
  pvals <- coef_table[, 4]
  max_p <- max(pvals)
  
  # если все значимы — выходим
  if (max_p <= 0.05) break
  
  worst_coef <- names(which.max(pvals))
  
  # переводим имя коэффициента в имя переменной
  term_labels <- attr(terms(current_model), "term.labels")
  worst_term <- term_labels[sapply(term_labels, function(term) startsWith(worst_coef, term))]
  
  # если не нашли то пробуем точное совпадение
  if (length(worst_term) == 0) {
    worst_term <- worst_coef
  } else {
    worst_term <- worst_term[1]
  }
  
  cat("   Удаляется фактор:", worst_term,
      "(коэффициент:", worst_coef,
      ", p-value =", round(max_p, 4), ")\n")
  
  new_formula <- as.formula(
    paste("log_rent ~", paste(setdiff(term_labels, worst_term), collapse = " + "))
  )
  
  # если факторов не осталось
  if (length(setdiff(term_labels, worst_term)) == 0) {
    current_model <- lm(log_rent ~ 1, data = flats)
    break
  } else {
    current_model <- lm(new_formula, data = flats)
  }
}

model_final <- current_model

cat("\n   Итоговая модель:\n")
print(summary(model_final))
cat("\n")

cat("   Формула итоговой модели:\n")
print(formula(model_final))
cat("\n")

cat("15. Прогноз для своей квартиры\n")

cat("   Уровни переменной type:\n")
print(levels(flats$type))
cat("   Уровни переменной furniture:\n")
print(levels(flats$furniture))
cat("\n")

my_flat <- data.frame(
  area = 45,
  floor = 5,
  total_floors = 9,
  type = factor("квартира", levels = levels(flats$type)),
  furniture = factor("есть", levels = levels(flats$furniture))
)

pred_log <- predict(model_final, newdata = my_flat, interval = "prediction", level = 0.95)
pred_rent <- exp(pred_log)

cat("   Параметры квартиры:\n")
print(my_flat)
cat("\n")

cat("   Прогноз арендной платы:\n")
cat("   Точечный прогноз:", round(pred_rent[1, "fit"], 2), "\n")
cat("   95% прогнозный интервал: [",
    round(pred_rent[1, "lwr"], 2), "; ",
    round(pred_rent[1, "upr"], 2), "]\n\n", sep = "")


cat("16. Анализ остатков итоговой множественной модели\n")

residuals_final <- residuals(model_final)

#windows()
par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))

# Нормальность остатков
shapiro_final <- shapiro.test(residuals_final)
cat("   Shapiro-Wilk test: p-value =",
    format.pval(shapiro_final$p.value, digits = 4), "\n")

if (shapiro_final$p.value > 0.05) {
  cat("   Вывод: остатки можно считать нормально распределёнными.\n")
} else {
  cat("   Вывод: остатки не являются нормально распределёнными.\n")
}

# Гетероскедастичность
bp_final <- bptest(model_final)
cat("   Breusch-Pagan test: p-value =",
    format.pval(bp_final$p.value, digits = 4), "\n")

if (bp_final$p.value > 0.05) {
  cat("   Вывод: признаков гетероскедастичности нет.\n")
} else {
  cat("   Вывод: присутствует гетероскедастичность.\n")
}

cat("17. Итог по гетероскедастичности\n")
cat("   Для стабилизации дисперсии использовано логарифмирование зависимой переменной:\n")
cat("   log(rent)\n")
cat("   Итоговая модель:\n")
print(formula(model_final))
cat("\n")

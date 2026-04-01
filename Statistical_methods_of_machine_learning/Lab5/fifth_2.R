grades_data <- read.table("D:/Labs_R/grades.txt", header = TRUE, fill = TRUE, encoding = "UTF-8")

names(grades_data) <- c("Группа 1", "Группа 2", "Группа 3", "Группа 4")
cat("\nПервые несколько строк:\n")
print(head(grades_data))
cat("\n")



all_grades <- c()
all_groups <- c()

for (i in 1:ncol(grades_data)) {
  group_name <- names(grades_data)[i]
  group_grades <- grades_data[[i]]
  group_grades <- group_grades[!is.na(group_grades)]  
  
  all_groups <- c(all_groups, rep(group_name, length(group_grades)))
  all_grades <- c(all_grades, group_grades)
}

contingency_table <- table(all_groups, all_grades)
cat("Таблица сопряженности 'Группа x Оценка':\n")
print(contingency_table)
cat("\n")

cat("Таблица с итогами:\n")
print(addmargins(contingency_table))
cat("\n")

prop_row <- prop.table(contingency_table, 1) * 100
cat("Распределение оценок ПО ГРУППАМ (%):\n")
print(round(prop_row, 1))
cat("\n")

cat("проверка гипотезы\n\n")

chi_result <- chisq.test(contingency_table)

cat("Ожидаемые частоты:\n")
print(round(chi_result$expected, 2))
cat("\n")

cat("Результаты критерия Хи-квадрат:\n",
    "  Статистика χ² =", round(chi_result$statistic, 4), "\n",
    "  Степени свободы =", chi_result$parameter, "\n",
    "  p-value =", format.pval(chi_result$p.value, digits = 4), "\n\n")

if (chi_result$p.value < 0.05) {
  cat("ВЫВОД: p < 0.05 → Связь между группой и оценкой СТАТИСТИЧЕСКИ ЗНАЧИМА\n")
  cat("Распределение оценок РАЗЛИЧАЕТСЯ по группам\n")
} else {
  cat("ВЫВОД: p > 0.05 → Нет значимой связи между группой и оценкой\n")
  cat("Распределение оценок ОДНОРОДНО по группам\n")
}

# Мозаичный график
mosaicplot(contingency_table, 
           main = "Мозаичный график: Группа vs Оценка",
           xlab = "Группа", ylab = "Оценка",
           color = c("lightblue", "lightgreen", "lightyellow", "pink"),
           cex.axis = 0.8)


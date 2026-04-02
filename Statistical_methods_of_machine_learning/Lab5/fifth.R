# Лабораторная работа №5 - Критерии сравнения групп и анализ таблиц сопряженности

# Загрузка данных
pulse_data <- read.table("D:/Labs_R/pulse.txt", header = TRUE, fill = TRUE, na.strings = c("", "NA")) 

cat("\nПервые несколько строк:\n")
print(head(pulse_data, 10))
cat("\n")

CB <- pulse_data$CB  # пациенты  нездоровые до лечения 
EB <- pulse_data$EB  # здоровые до лечения 
CA <- pulse_data$CA  # пациенты после лечения 
EA <- pulse_data$EA  # здоровые после лечения

cat("Количество наблюдений в каждой группе:\n",
    "CB (пациенты до):", sum(!is.na(CB)), "наблюдений\n",
    "EB (здоровые до):", sum(!is.na(EB)), "наблюдений\n",
    "CA (пациенты после):", sum(!is.na(CA)), "наблюдений\n",
    "EA (здоровые после):", sum(!is.na(EA)), "наблюдений\n\n")

CB_clean <- CB[!is.na(CB)]
EB_clean <- EB[!is.na(EB)]
CA_clean <- CA[!is.na(CA)]
EA_clean <- EA[!is.na(EA)]

#првоерка нормальности 
#оцениваем распределение данных
#и графики ниже показывают нормально ли распределены данные 
#чем ближе к линии тем нормальнее
par(mfrow = c(2, 4))

hist(CB_clean, main = "Группа CB (пациенты до)", xlab = "Пульс", col = "lightblue")
hist(EB_clean, main = "Группа EB (здоровые до)", xlab = "Пульс", col = "lightgreen")
hist(CA_clean, main = "Группа CA (пациенты после)", xlab = "Пульс", col = "lightblue")
hist(EA_clean, main = "Группа EA (здоровые после)", xlab = "Пульс", col = "lightgreen")

qqnorm(CB_clean, main = "Q-Q plot: CB", col = "blue", pch = 16)
qqline(CB_clean, col = "red", lwd = 2)
qqnorm(EB_clean, main = "Q-Q plot: EB", col = "green", pch = 16)
qqline(EB_clean, col = "red", lwd = 2)
qqnorm(CA_clean, main = "Q-Q plot: CA", col = "blue", pch = 16)
qqline(CA_clean, col = "red", lwd = 2)
qqnorm(EA_clean, main = "Q-Q plot: EA", col = "green", pch = 16)
qqline(EA_clean, col = "red", lwd = 2)

par(mfrow = c(1, 1))

cat("Тест Шапиро-Уилка на нормальность:\n")

safe_shapiro <- function(x, name)
{
  if (length(x) >= 3 && length(x) <= 5000) 
  {
    test <- shapiro.test(x)
    cat(name, ": W =", round(test$statistic, 4), 
        ", p-value =", format.pval(test$p.value, digits = 4),
        ifelse(test$p.value > 0.05, " → НОРМАЛЬНО", " → НЕ НОРМАЛЬНО"), "\n")
    return(test$p.value)
  } 
  else 
  {
    cat(name, ": недостаточно данных для теста (n =", length(x), ")\n")
    return(NA)
  }
}

p_CB <- safe_shapiro(CB_clean, "CB (пациенты до)")
cat("\n")
p_EB <- safe_shapiro(EB_clean, "EB (здоровые до)")
cat("\n")
p_CA <- safe_shapiro(CA_clean, "CA (пациенты после)")
cat("\n")
p_EA <- safe_shapiro(EA_clean, "EA (здоровые после)")
cat("\n")


#сравниваем до и после 
#получается что будет по 30 значений 
n_patients <- min(length(CB_clean), length(CA_clean))
n_healthy <- min(length(EB_clean), length(EA_clean))

CB_paired <- CB_clean[1:n_patients]
CA_paired <- CA_clean[1:n_patients]
EB_paired <- EB_clean[1:n_healthy]
EA_paired <- EA_clean[1:n_healthy]

#CB и EB имеют нормальное распределение
#CA и EA имеют отклонения от нормальности

# Группа пациентов (CB vs CA)
cat("Группа пациентов (CB - до, CA - после):\n")
cat("Используем", n_patients, "пар наблюдений\n")

#елси распределение нормальное то используем t-test
#если распределение не нормальное то используем Парный тест улксона
if (length(CB_paired) >= 3 && length(CA_paired) >= 3) {
  if (!is.na(p_CB) && !is.na(p_CA) && p_CB > 0.05 && p_CA > 0.05) {
    test_patients <- t.test(CB_paired, CA_paired, paired = TRUE)
    cat("Использован: парный t-тест\n")
  } else {
    test_patients <- wilcox.test(CB_paired, CA_paired, paired = TRUE)
    cat("Использован: парный тест Уилкоксона\n")
  }
  cat("Результат: p-value =", format.pval(test_patients$p.value, digits = 4), "\n")
  if (test_patients$p.value < 0.05) {
    cat("ВЫВОД: Статистически значимые различия (p < 0.05)\n")
  } else {
    cat("ВЫВОД: Нет статистически значимых различий (p > 0.05)\n")
  }
} else {
  cat("Недостаточно данных для парного сравнения\n")
}
cat("\n")

# Группа здоровых (EB vs EA)
cat("Группа здоровых (EB - до, EA - после):\n")
cat("Используем", n_healthy, "пар наблюдений\n")

if (length(EB_paired) >= 3 && length(EA_paired) >= 3) {
  if (!is.na(p_EB) && !is.na(p_EA) && p_EB > 0.05 && p_EA > 0.05) {
    test_healthy <- t.test(EB_paired, EA_paired, paired = TRUE)
    cat("Использован: парный t-тест\n")
  } else {
    test_healthy <- wilcox.test(EB_paired, EA_paired, paired = TRUE)
    cat("Использован: парный тест Уилкоксона\n")
  }
  cat("Результат: p-value =", format.pval(test_healthy$p.value, digits = 4), "\n")
  if (test_healthy$p.value < 0.05) {
    cat("ВЫВОД: Статистически значимые различия (p < 0.05)\n")
  } else {
    cat("ВЫВОД: Нет статистически значимых различий (p > 0.05)\n")
  }
} else {
  cat("Недостаточно данных для парного сравнения\n")
}
cat("\n")

# Построим ящики с усами
boxplot(CB_clean, CA_clean, EB_clean, EA_clean,
        names = c("Пациенты до", "Пациенты после", "Здоровые до", "Здоровые после"),
        main = "Сравнение пульса до и после применения лекарства",
        ylab = "Пульс",
        col = c("lightblue", "lightblue", "lightgreen", "lightgreen"))
grid()


#сравнение больных и здоровых
# Сравнение до применения (CB vs EB)
cat("Сравнение ДО применения (пациенты vs здоровые):\n")
if (length(CB_clean) >= 2 && length(EB_clean) >= 2) {
  if (!is.na(p_CB) && !is.na(p_EB) && p_CB > 0.05 && p_EB > 0.05) {
    test_before <- t.test(CB_clean, EB_clean, var.equal = FALSE)
    cat("Использован: t-тест Уэлча\n")
  } else {
    test_before <- wilcox.test(CB_clean, EB_clean)
    cat("Использован: тест Манна-Уитни (Уилкоксона)\n")
  }
  cat("Результат: p-value =", format.pval(test_before$p.value, digits = 4), "\n")
  if (test_before$p.value < 0.05) {
    cat("ВЫВОД: Статистически значимые различия - группы различаются до лечения\n")
  } else {
    cat("ВЫВОД: Нет статистически значимых различий - группы однородны до лечения\n")
  }
} else {
  cat("Недостаточно данных для сравнения\n")
}
cat("\n")

# Сравнение после применения (CA vs EA)
cat("Сравнение ПОСЛЕ применения (пациенты vs здоровые):\n")
if (length(CA_clean) >= 2 && length(EA_clean) >= 2) {
  if (!is.na(p_CA) && !is.na(p_EA) && p_CA > 0.05 && p_EA > 0.05) {
    test_after <- t.test(CA_clean, EA_clean, var.equal = FALSE)
    cat("Использован: t-тест Уэлча\n")
  } else {
    test_after <- wilcox.test(CA_clean, EA_clean)
    cat("Использован: тест Манна-Уитни (Уилкоксона)\n")
  }
  cat("Результат: p-value =", format.pval(test_after$p.value, digits = 4), "\n")
  if (test_after$p.value < 0.05) {
    cat("ВЫВОД: Статистически значимые различия - группы различаются после лечения\n")
  } else {
    cat("ВЫВОД: Нет статистически значимых различий - группы однородны после лечения\n")
  }
} else {
  cat("Недостаточно данных для сравнения\n")
}
cat("\n")

# Ящики с усами для сравнения
par(mfrow = c(1, 2))

boxplot(CB_clean, EB_clean,
        names = c("Пациенты", "Здоровые"),
        main = "До применения",
        ylab = "Пульс",
        col = c("lightblue", "lightgreen"),
        ylim = range(c(CB_clean, EB_clean, CA_clean, EA_clean), na.rm = TRUE))
grid()

boxplot(CA_clean, EA_clean,
        names = c("Пациенты", "Здоровые"),
        main = "После применения",
        ylab = "Пульс",
        col = c("lightblue", "lightgreen"),
        ylim = range(c(CB_clean, EB_clean, CA_clean, EA_clean), na.rm = TRUE))
grid()

par(mfrow = c(1, 1))


#выводы про лекартсва 

mean_CB <- mean(CB_clean, na.rm = TRUE)
mean_CA <- mean(CA_clean, na.rm = TRUE)
mean_EB <- mean(EB_clean, na.rm = TRUE)
mean_EA <- mean(EA_clean, na.rm = TRUE)

cat("Средние значения пульса:\n")
cat("  Пациенты до (CB):   ", round(mean_CB, 2), "\n")
cat("  Пациенты после (CA):", round(mean_CA, 2), "\n")
cat("  Здоровые до (EB):   ", round(mean_EB, 2), "\n")
cat("  Здоровые после (EA):", round(mean_EA, 2), "\n\n")

# Изменения
change_patients <- mean_CA - mean_CB
change_healthy <- mean_EA - mean_EB

cat("Изменение у пациентов:   ", round(change_patients, 2), "\n",
    "Изменение у здоровых:   ", round(change_healthy, 2), "\n\n")


# Выводы
if (exists("test_patients") && test_patients$p.value < 0.05) {
  if (mean_CA < mean_CB) {
    cat("Лекарство ЭФФЕКТИВНО для пациентов: пульс значимо снизился\n")
  } else if (mean_CA > mean_CB) {
    cat("Лекарство влияет на пациентов, но пульс повысился\n")
  }
} else {
  cat("Лекарство НЕ эффективно для пациентов: изменения не значимы\n")
}

if (exists("test_healthy") && test_healthy$p.value < 0.05) {
  cat("Лекарство значимо влияет на здоровых (возможны побочные эффекты)\n")
} else {
  cat("Лекарство не влияет на здоровых (побочных эффектов нет)\n")
}

# Дополнительный вывод на основе сравнения групп
if (exists("test_before") && test_before$p.value < 0.05 && 
    exists("test_after") && test_after$p.value > 0.05) {
  cat("\nЛекарство нормализует пульс пациентов до уровня здоровых людей\n")
}

#Лекарственное средство эффективно: 
#у пациентов пульс значимо снизился с 82.97 до 75.93 (p < 0.05) и достиг уровня здоровых людей (75.37).
#У здоровых испытуемых пульс практически не изменился: 
#с 79.73 до 75.37 (p > 0.05), что свидетельствует об отсутствии побочных эффектов. 
#Таким образом, препарат нормализует пульс пациентов, не воздействуя на здоровый организм.

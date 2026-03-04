#Llamando a las librerias
install.packages("ggpubr")
install.packages("agricolae")
install.packages("ggpubr")
library(dplyr)
library(car)
library(agricolae)
library(broom)
library(ggplot2)
library(readr)
library(ggpubr)
library(stringr)

#Leyendo los datos
var_crecimiento <- read.csv("C:/Analisis_inoculantes/Iniculantes/Datos/Var_Crecimiento.csv")
attach(var_crecimiento)

var_crecimiento$Tratamientos <- as.factor(var_crecimiento$Tratamientos)
var_crecimiento$Bloques <- as.factor(var_crecimiento$Bloques)

#Andeva, diagnostico y parametros del modelo para la variable altura

modelo_alt <- aov(Altura ~ Tratamientos + Bloques, data = var_crecimiento)
anova_alt <- anova(modelo_alt)
sw_test_alt <- shapiro.test(residuals(modelo_alt))
lev_test_alt <- leveneTest(Altura ~ Tratamientos, data = var_crecimiento)
HSDTukey_test_alt <- HSD.test(modelo_alt, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(Altura)
res_alt <- residuals(modelo_alt)
Pred_alt <- predict(modelo_alt)
df_res_vs_pr_alt <- data.frame(Pred_alt, res_alt)
scatterplotMatrix(df_res_vs_pr_alt)
scatterplot(Pred_alt, res_alt)

#Andeva, diagnostico y parametros del modelo para la variable Numero de hojas

modelo_hoj <- aov(Numero_de_hojas ~ Tratamientos + Bloques, data = var_crecimiento)
anova_hoj <- anova(modelo_hoj)
sw_test_hoj <- shapiro.test(residuals(modelo_hoj))
lev_test_hoj <- leveneTest(Numero_de_hojas ~ Tratamientos, data = var_crecimiento)
HSDTukey_test_hoj <- HSD.test(modelo_hoj, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(Numero_de_hojas)
res_hoj <- residuals(modelo_hoj)
Pred_hoj <- predict(modelo_hoj)
df_res_vs_pr_hoj <- data.frame(Pred_hoj, res_hoj)
scatterplotMatrix(df_res_vs_pr_hoj)
scatterplot(Pred_alt, res_hoj)

#Andeva, diagnostico y parametros del modelo para la variable Diametro del tallo

modelo_diam <- aov(Diametro ~ Tratamientos + Bloques, data = var_crecimiento)
anova_diam <- anova(modelo_diam)
sw_test_diam <- shapiro.test(residuals(modelo_hoj))
lev_test_diam <- leveneTest(Diametro ~ Tratamientos, data = var_crecimiento)
HSDTukey_test_diam <- HSD.test(modelo_diam, trt = "Tratamientos", group = TRUE, alpha = 0.05)        
qqnorm(Diametro)
res_diam <- residuals(modelo_diam)
Pred_diam <- predict(modelo_diam)
df_res_vs_pr_diam <- data.frame(Pred_diam, res_diam)
scatterplotMatrix(df_res_vs_pr_diam)
scatterplot(Pred_diam, res_diam)

#Calculando estadisticos de los datos

cv_alt <- round(100*sd(Altura)/mean(Altura), 2)
cv_hoj <- round(100*sd(Numero_de_hojas)/mean(Numero_de_hojas), 2)
cv_diam <- round(100*sd(Diametro)/mean(Diametro), 2)

r2_alt <- round((anova_alt$'Sum Sq'[1]+anova_alt$'Sum Sq'[2]) / sum(anova_alt$'Sum Sq'), 2)
r2_hoj <- round((anova_hoj$'Sum Sq'[1]+anova_hoj$'Sum Sq'[2]) / sum(anova_hoj$'Sum Sq'), 2)
r2_diam <- round((anova_diam$'Sum Sq'[1]+anova_diam$'Sum Sq'[2]) / sum(anova_diam$'Sum Sq'), 2)


#Construyendo un marco de datos
trt <- c(rownames(HSDTukey_test_diam$groups))
letra_alt <- c(HSDTukey_test_alt$groups)
letra_hoj <- c(HSDTukey_test_hoj$groups)
letra_diam <- c(HSDTukey_test_diam$groups)
df <- data.frame(trt, letra_alt, letra_diam, letra_hoj)
df_stad <- data.frame(cv_alt,
                      cv_diam,
                      cv_hoj, r2_alt,
                      r2_diam, r2_hoj, 
                      sw_test_alt$p.value, 
                      sw_test_diam$p.value,
                      sw_test_hoj$p.value,
                      lev_test_alt$`F value`[1],
                      lev_test_diam$`F value`[1],
                      lev_test_hoj$`F value`[1])


#Graficando los datos
library(ggplot2)
library(stringr)
install.packages("extrafont")
library(extrafont)
font_import(pattern = "Arial", prompt = FALSE)
fonts()

etiq_zz <- function(x) {
        ifelse(seq_along(x) %% 2 == 1,
               paste0(x, "\n"),
               paste0("\n", x))
}


graf_alt <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Altura),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Altura, label = groups),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "Tratamientos", y = "Altura") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
                axis.text.y  = element_text(size = 10, face = "bold"),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = element_text(face = "bold"))+
        annotate("text", x = "Testigo", y = 75, label = "Levene = 0.6581")+
        annotate("text", x = "Testigo", y = 80, label = "ShapWilk = 0.5020")+
        annotate("text", x = "Testigo", y = 85, label = "CV = 23%")+
        annotate("text", x = "Testigo", y = 90, label = expression(R^2 == 0.61))+
        annotate("text", x = "Testigo", y = 95, label = "pvalor = 0.0382")
print(graf_alt)

graf_hoj <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Numero_de_hojas),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Numero_de_hojas, label = groups),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 20),
                breaks = seq(0, 20, by = 4)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"))+
        labs(x = "Tratamientos", y = "Numero de hojas") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))+
        annotate("text", x = "Testigo", y = 15, label = "Levene = 0.7320")+
        annotate("text", x = "Testigo", y = 16, label = "ShapWilk = 0.3190")+
        annotate("text", x = "Testigo", y = 17, label = "CV = 16%")+
        annotate("text", x = "Testigo", y = 18, label = expression(R^2 == 0.65))+
        annotate("text", x = "Testigo", y = 19, label = "pvalor = 0.0034")
print(graf_hoj)       

graf_diam <- ggplot(data = df) +
        geom_col(aes(x = trt, y = Diametro),
                 fill = "#808080", width = 0.5) +
        geom_text(aes(x = trt, y = Diametro, label = groups),
                  vjust = -0.5,
                  size = 4,
                  fontface = "bold")+
        scale_x_discrete(labels = function(x) str_replace_all(x, "_+", "\n")) +
        scale_y_continuous(limits = c(0, 6),
                           breaks = seq(0, 6, by = 1)) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(size = 11, face = "bold"),
              axis.text.y = element_text(size = 11, face = "bold"),
              margin = margin(t = 6))+
        labs(x = "Tratamientos", y = "Diámetro") +
        theme_minimal(base_size = 10) +
        theme(axis.text.x  = element_text(size = 10, face = "bold"),
              axis.text.y  = element_text(size = 10, face = "bold"),
              axis.title.x = element_text(face = "bold"),
              axis.title.y = element_text(face = "bold"))+
        annotate("text", x = "Testigo", y = 6.0, label = "Levene = 0.4073")+
        annotate("text", x = "Testigo", y = 5.7, label = "ShapWilk = 0.3188")+
        annotate("text", x = "Testigo", y = 5.4, label = "CV = 9.5%")+
        annotate("text", x = "Testigo", y = 5.1, label = expression(R^2 == 0.74))+
        annotate("text", x = "Testigo", y = 4.8, label = "pvalor = 0.0041")
print(graf_diam)
#Convierte el data frame llamado "Supuestos_y_estadisticos" a una tabla en formato apa

graf_crecimiento <- ggarrange(graf_alt, graf_hoj, graf_diam, 
                              ncol = 2, nrow = 2, common_by())
print(graf_crecimiento)







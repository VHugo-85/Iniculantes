#Diseño experimental BCA
# Bloque I
B1 <- c("T7","T3","T4","T1","T5","T2","T8","T6")
# Bloque II
B2 <- c("T2","T5","T3","T8","T6","T1","T7","T4")
# Bloque III
B3 <- c("T3","T4","T2","T6","T8","T5","T1","T7")
# Bloque IV
B4 <- c("T8","T7","T1","T3","T4","T6","T2","T5")

BCA <- data.frame(
        Bloques = rep(paste0("B", 1:4), each = 8),
        plot = 1:32,
        tratamiento = c(B1, B2, B3, B4)
)
print(BCA)

#Conectando los marcos de datos o variables
Var_Crecimiento <- read_excel("Datos/Var_Crecimiento.xlsx")
Var_Productivas <- read_excel("Datos/Var_Productivas.xlsx")
Var_Rendimiento <- read_excel("Datos/Var_Rendimiento.xlsx")



